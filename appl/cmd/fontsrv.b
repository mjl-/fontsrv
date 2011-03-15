implement Fontsrv;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "readdir.m";
	readdir: Readdir;
include "names.m";
	names: Names;
include "styx.m";
	styx: Styx;
	Tmsg, Rmsg: import styx;
include "styxservers.m";
	styxservers: Styxservers;
	Styxserver, Fid, Navigator, Navop: import styxservers;
	nametree: Nametree;
	Tree: import nametree;
include "tables.m";
	tables: Tables;
	Strhash: import tables;

Fontsrv: module {
	init:	fn(nil: ref Draw->Context, args: list of string);
};

Mflag: int;
dflag: int;
argdflag: int;
argxdpi := argydpi := 96;
ftpath := "/mnt/ft";
mtpt: con "/mnt/font";
rulefile := "/fonts/rules";

config: Cfg;

styles := array[] of {"r", "i", "b", "ib"};
Fquestion, Fstar, Fpointsize: con 1<<iota; # flags in Fspec.parse

# an empty list denotes "*".  an empty string, -1 or (-1, -1) denotes "?"
Fspec: adt {
	ps:	list of string;
	nms:	list of string;
	sts:	list of int;
	szs:	list of ref (int, int);

	parse:	fn(s: string, flags: int, c: ref Cfg): (ref Fspec, string);
	text:	fn(fs: self ref Fspec): string;
};

Font: adt {
	single:	int;
	fs:	ref Fspec;
	path:	string;
};

Fmatch: adt {
	f:	ref Font;
	nm:	string;
	st:	int;
	sz:	int;
	score:	int;

	path:	fn(fm: self ref Fmatch): string;
	text:	fn(fm: self ref Fmatch): string;
};

Cfg: adt {
	dflag:		int;
	lit:		ref Strhash[ref Fspec];
	fonts:		list of ref Font;
	fallback:	string;
	rewrites:	list of ref (ref Fspec, ref Fspec);
	xdpi, ydpi:	int;
};

Ncache: con 64;
# 0 is most recently used.
Query: type (string, string, string, string);  # request, request-fspec, rewritten-fspec, font-path
cache := array[Ncache] of Query;

Qroot, Qctl, Qlookup, Qlist, Qcache: con iota;
srv: ref Styxserver;
tree: ref Tree;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	names = load Names Names->PATH;
	readdir = load Readdir Readdir->PATH;
	styx = load Styx Styx->PATH;
	styx->init();
	styxservers = load Styxservers Styxservers->PATH;
	styxservers->init(styx);
	nametree = load Nametree Nametree->PATH;
	nametree->init();
	tables = load Tables Tables->PATH;

	sys->pctl(Sys->NEWPGRP, nil);

	arg->init(args);
	arg->setusage(arg->progname()+" [-dM] [-f rulefile] [-p ftpath] [-r xdpi ydpi]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	argdflag = ++dflag;
		'M' =>	Mflag++;
		'f' =>	rulefile = arg->earg();
		'p' =>	ftpath = arg->earg();
		'r' =>	argxdpi = int arg->earg();
			argydpi = int arg->earg();
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args != 0)
		arg->usage();

	err := readconfig();
	if(err != nil)
		fail("readconfig: "+err);

	(xtree, navc) := nametree->start();
	tree = xtree;
	tree.create(big Qroot, dir(".", 8r555|Sys->DMDIR, Qroot));
	tree.create(big Qroot, dir("ctl", 8r222, Qctl));
	tree.create(big Qroot, dir("lookup", 8r666, Qlookup));
	tree.create(big Qroot, dir("list", 8r666, Qlist));
	tree.create(big Qroot, dir("cache", 8r444, Qcache));

	fd := sys->fildes(0);
	fds := array[2] of ref Sys->FD;
	if(!Mflag) {
		if(sys->pipe(fds) < 0)
			fail(sprint("pipe: %r"));
		fd = fds[0];
	}

	msgc: chan of ref Tmsg;
	(msgc, srv) = Styxserver.new(fd, Navigator.new(navc), big Qroot);

	if(Mflag)
		return styxsrv(msgc);

	spawn styxsrv(msgc);
	if(sys->mount(fds[1], nil, mtpt, sys->MREPL, nil) < 0) {
		killgrp(pid());
		fail(sprint("mount: %r"));
	}
}


Parse: adt {
	b: ref Iobuf;
	path:	string;
	ln: int;

	open:	fn(path: string): ref Parse;
	next:	fn(p: self ref Parse): (array of string, string);
	pos:	fn(p: self ref Parse): string;
};

Parse.open(path: string): ref Parse
{
	b := bufio->open(path, bufio->OREAD);
	if(b == nil)
		return nil;
	return ref Parse(b, path, 0);
}

Parse.next(p: self ref Parse): (array of string, string)
{
	for(;;) {
		s := p.b.gets('\n');
		if(s == nil) {
			if(p.b.getc() != bufio->EOF)
				return (nil, sprint("read: %r"));
			return (nil, nil);
		}
		p.ln++;
		a := l2a(str->unquoted(s));
		if(len a == 0 || str->prefix("#", str->drop(a[0], " \t")))
			continue;
		return (a, nil);
	}
}

Parse.pos(p: self ref Parse): string
{
	return sprint("%q:%d", p.path, p.ln);
}


# format from ttffs:
# stem fspec
readindex(dir: string, c: ref Cfg)
{
	path := dir+"/index";
	p := Parse.open(path);
	if(p == nil)
		return warn(sprint("open %q: %r", path));
	for(;;) {
		(a, err) := p.next();
		if(err != nil)
			raise sprint("error:%s: %s", p.pos(), err);
		if(a == nil)
			break;
		if(len a != 2)
			raise sprint("error:%s: expected two params", p.pos());
		fs: ref Fspec;
		(fs, err) = Fspec.parse(a[1], 0, c);
		if(err != nil)
			raise sprint("error:%s: bad fontspec: %s", p.pos(), err);
		f := ref Font(0, fs, dir+"/"+a[0]);
		c.fonts = f::c.fonts;
		say(sprint("readindex, %s", fs.text()));
	}
}

checkargs(cmd: string, n, h: int)
{
	if(n != h)
		raise sprint("error:%s needs %d args, saw %d", cmd, n, h);
}

readconfig(): string
{
	p: ref Parse;
	{
		f := rulefile;
		(ok, nil) := sys->stat(f);
		b := bufio->open(f, bufio->OREAD);
		if(b == nil) {
			if(ok == 0)
				raise sprint("error:open %q: %r", f);
			# continue as if file was empty
			b = bufio->aopen(array[0] of byte);
			f = "<none>";
		}
		p = ref Parse(b, f, 0);
		config = *xreadconfig(p);
		dflag = config.dflag;
		return nil;
	} exception x {
	"error:*" =>
		x = x[len "error:":];
		if(p != nil)
			x = p.pos()+": ";
		return x;
	}
}

xreadconfig(p: ref Parse): ref Cfg
{
	c := ref Cfg;
	c.dflag = argdflag;
	c.xdpi = argxdpi;
	c.ydpi = argydpi;
	c.lit = c.lit.new(11, nil);
	readindex(ftpath, c);
	n := 0;
	for(;;) {
		(a, perr) := p.next();
		if(perr != nil)
			raise "error:"+perr;
		if(a == nil)
			break;
		cmd := a[0];
		a = a[1:];
		case cmd {
		"dpi" =>
			if(n != 0)
				raise "error:dpi must be first rule";
			checkargs(cmd, 2, len a);
			c.xdpi = xint(a[0]);
			c.ydpi = xint(a[1]);
		"rewrite" =>
			checkargs(cmd, 2, len a);
			ofs := xparsefspec(a[0], Fquestion|Fstar|Fpointsize, c);
			nfs := xparsefspec(a[1], Fquestion|Fstar|Fpointsize, c);
			c.rewrites = ref (ofs, nfs)::c.rewrites;
		"fallback" =>
			checkargs(cmd, 1, len a);
			c.fallback = str->tolower(a[0]);
		# should a literal be in Fpointsize?
		#"literal" =>
		#	checkargs(cmd, 2, len a);
		#	fs := xparsefspec(a[1], Fquestion, c);
		#	c.lit.del(a[0]);
		#	c.lit.add(a[0], fs);
		"font" =>
			checkargs(cmd, 2, len a);
			path := a[0];
			fs := xparsefspec(a[1], 0, c);
			ff := ref Font(1, fs, path);
			c.fonts = ff::c.fonts;
		"walk" =>
			checkargs(cmd, 1, len a);
			walk(a[0], c);
		"debug" =>
			checkargs(cmd, 1, len a);
			c.dflag = xint(a[0]);
		* =>
			raise sprint("error:bad directive %#q", cmd);
		}
		n++;
	}
	walk("/fonts", c);
	mkknownfonts(c);
	mkdefaults(c);
	c.fonts = rev(c.fonts);
	c.rewrites = rev(c.rewrites);
	return c;
}


isknownmono(fs: ref Fspec): int
{
	nm := hd fs.nms;
	return str->prefix("pelm", nm) || str->prefix("lucm", nm) || str->prefix("misc", nm) || suffix("mono", nm);
}

isknownserif(fs: ref Fspec): int
{
	return hd fs.nms == "st";
}

# most translated sizes are the ascent of the font.  not the correct choice, but closest.
# the sizes of some fonts have been changed to prevent clashes.
# some fonts reference missing subfonts
knownfonts := array[] of {
#"lucidasans-chinese.r.%	/fonts/chinese/lucidasans/unicode.	.font	8=12 9=13 10=14 13=15",
#"lucm-chinese.r.%	/fonts/chinese/lucm/unicode.		.font	9=14",
#"pelm-chinese.r.%	/fonts/chinese/pelm/unicode.		.font	9=14 16=15",
#"pelm-chinese-%.r.14	/fonts/chinese/pelm/			.8.font	big5 fangsong hanc hanj hku",
#"pelm-chinese-song.r.%	/fonts/chinese/pelm/song.		.font	8=14 9=15",

"misc-ascii.r.%		/fonts/misc/ascii.			.font	5x7=5 6x10=8",
"misc-latin1.r.%	/fonts/misc/latin1.			.font	6x10=8 6x13=11 8x13=10",
"misc.r.%		/fonts/misc/unicode.			.font	6x13=11",
"%.r.8			/fonts/misc/				.font	st",

"charon.r.%		/fonts/charon/plain.			.font	tiny=9 small=11 normal=14 large=16 vlarge=21",
"charon.i.%		/fonts/charon/italic.			.font	tiny=9 small=11 normal=14 large=16 vlarge=21",
"charon.b.%		/fonts/charon/bold.			.font	tiny=9 small=11 normal=14 large=16 vlarge=21",
"charon-mono.r.%	/fonts/charon/cw.			.font	tiny=9 small=11 normal=14 large=16 vlarge=21",

"minicharon.r.%		/fonts/minicharon/plain.		.font	tiny=9 small=10 normal=11 large=12 vlarge=14",
"minicharon.i.%		/fonts/minicharon/italic.		.font	tiny=9 small=10 normal=11 large=12 vlarge=14",
"minicharon.b.%		/fonts/minicharon/bold.			.font	tiny=9 small=10 normal=11 large=12 vlarge=14",
"minicharon-mono.r.%	/fonts/minicharon/cw.			.font	tiny=9 small=10 normal=11 large=12 vlarge=14",

"lucida-latin1.b.%	/fonts/lucida/latin1B.			.font	5=9 6=10 7=12 8=14 9=16 10=17 12=21 14=25 16=29 18=32 20=36 24=43 28=50 32=57",
"lucida-latin1.i.%	/fonts/lucida/latin1I.			.font	5=9 6=10 7=12 8=14 9=16 10=17 12=21 14=25 16=29 18=32 20=36 24=43 28=50 32=57",
"lucida-latin1-mono.r.%	/fonts/lucida/latin1CW.			.font	5=9 6=10 7=12 8=14 9=16 10=17 12=21 14=25 16=29 18=32 20=36 24=43 28=50 32=57",
# latin1CW.7.2.font?
"lucida.r.%		/fonts/lucida/unicode.			.font	5=10 6=12 7=14 8=16 9=18 10=20 12=24 14=28 16=32 18=36 20=40 24=48 28=56 32=64",

# latin1_13.font?
"lucidasans.r.%		/fonts/lucidasans/unicode.		.font	6=9 7=11 8=13 9x24=21 10=16 13=21",
"lucidasans-latin1.r.%	/fonts/lucidasans/latin1.		.font	6=9 7=11 8=13 10=16 13=21",
"lucidasans-latin1.i.%	/fonts/lucidasans/italiclatin1.		.font	6=9 7=11 8=13 10=16 13=21",
"lucidasans-latin1.b.%	/fonts/lucidasans/boldlatin1.		.font	6=9 7=11 8=13 10=16 13=21",
"lucidasans-latin1-mono.r.%	/fonts/lucidasans/typelatin1.		.font	6=9 7=11",
"lucidasans-latin1-mono.b.%	/fonts/lucidasans/boldtypelatin1.	.font	6=8 7=10",
"lucidasans-euro.r.%	/fonts/lucidasans/euro.			.font	7=10 8=12",
"lucidasans-news.r.%	/fonts/lucidasans/news.			.font	6=8 7=10 8=12 10=15",

"lucm%			/fonts/lucm/				.9.font	euro=-euro.r.13 latin1=-latin1.r.13 unicode=.r.14",

"pelm-ascii.r.%		/fonts/pelm/ascii.			.font	10=14 12=18 16=23",
"pelm-euro.r.%		/fonts/pelm/euro.			.font	8=11 9=14",
"pelm-latin1.r.%	/fonts/pelm/latin1.			.font	8=11 9=13",
"pelm-%.r.14		/fonts/pelm/				.9.font	fangsong hku jis", # hanc hanj
#"pelm-song.r.%		/fonts/pelm/song.			.font	9=14 9x24=20",
"pelm.r.%		/fonts/pelm/unicode.			.font	8=11 9=14 9x24=21",
};

# font is considered usable when all its subfonts exist
usablefont(p: string): int
{
	b := bufio->open(p, bufio->OREAD);
	if(b == nil)
		return 0;
	if(b.gets('\n') == nil)
		return 0;
	case p {
	"/fonts/lucidasans/euro.8.font" or
	"/fonts/lucm/euro.9.font" =>
		# these miss subfont /fonts/lucm/cyrillic.9, but are used in inferno-os nonetheless
		return 1;
	}
	d := names->dirname(p);
	for(;;) {
		s := b.gets('\n');
		if(s == nil) {
			if(b.getc() == bufio->EOF)
				return 1;
			return 0;
		}
		if(s[len s-1] == '\n')
			s = s[:len s-1];
		(nil, l) := sys->tokenize(s, " \t");
		a := l2a(l);
		f: string;
		case len a {
		3 =>	f = a[2];
		4 =>	f = a[3];
		* =>	return 0;
		}
		sp := names->rooted(d, f);
		if(sys->stat(sp).t0 != 0)
			return 0;
	}
}

mkknownfonts(c: ref Cfg)
{
	for(i := 0; i < len knownfonts; i++) {
		a := l2a(str->unquoted(knownfonts[i]));
		(repl0, repl1) := str->splitstrl(a[0], "%");
		use := 0;
		if(repl1 != nil) {
			use = 1;
			repl1 = repl1[1:];
		}
		pre := a[1];
		suf := a[2];
		for(j := 3; j < len a; j++) {
			(k, v) := str->splitstrl(a[j], "=");
			if(v == nil)
				v = k;
			else
				v = v[1:];
			s := "."+repl0;
			if(use)
				s = sprint(".%s%s%s", repl0, v, repl1);
			fs := xparsefspec(s, 0, c);
			path := pre+k+suf;
			if(usablefont(path)) {
				f := ref Font(1, fs, path);
				c.fonts = f::c.fonts;
				say(sprint("existing font %q\t\t%q", path, fs.text()));
			} else {
				if(isknownmono(fs))
					fs.nms = list of {"mono"};
				else if(isknownserif(fs))
					fs.nms = list of {"serif"};
				else
					fs.nms = list of {"sans"};
				say(sprint("missing/incomplete font %q\t\t%q", path, fs.text()));
			}
			if(c.lit.find(path) == nil) {
				nfs := xparsefspec(s, Fpointsize, c);
				fs.szs = nfs.szs;
				c.lit.add(path, fs);
			}
		}
	}
	deffs := ref Fspec(nil, "*default*"::nil, 0::nil, ref (13, 13+1)::nil);
	def := ref Font(1, deffs, "*default*");
	if(c.lit.find("*default*") == nil)
		c.lit.add("*default*", deffs);
	c.fonts = def::c.fonts;
}


walk(p: string, c: ref Cfg)
{
	(d, n) := readdir->init(p, readdir->NONE);
	if(n < 0)
		return warn(sprint("readdir %q: %r", p));
	for(i := 0; i < n; i++) {
		np := sprint("%s/%s", p, d[i].name);
		if(d[i].mode&sys->DMDIR) {
			walk(np, c);
			continue;
		}
		nm := d[i].name;
		if(!suffix(".font", nm))
			continue;
		nm = "."+nm[:len nm-len ".font"];
		(fs, err) := Fspec.parse(nm, 0, c);
		if(err != nil)
			continue;
		say(sprint("walked %q %s", np, fs.text()));
		f := ref Font(1, fs, np);
		c.fonts = f::c.fonts;
	}
}


mkdefaults(c: ref Cfg)
{
	l := list of {
	(".?..",	".mono.."),
	("..?.",	"..r."),
	("...?",	"...12"),
	};
	for(; l != nil; l = tl l) {
		(o, n) := hd l;
		ofs := xparsefspec(o, Fquestion|Fstar|Fpointsize, c);
		nfs := xparsefspec(n, Fquestion|Fstar|Fpointsize, c);
		c.rewrites = ref (ofs, nfs)::c.rewrites;
	}
	if(c.fallback == nil)
		c.fallback = "mono";
}


textnames(l: list of string): list of string
{
	if(l == nil)
		return ""::nil;
	r: list of string;
	for(; l != nil; l = tl l)
		if(hd l == nil)
			r = "?"::r;
		else
			r = hd l::r;
	return rev(r);
}

textstyles(l: list of int): list of string
{
	if(l == nil)
		return ""::nil;
	r: list of string;
	for(; l != nil; l = tl l)
		case hd l {
		-1 =>	r = "?"::r;
		* =>	r = styles[hd l]::r;
		}
	return rev(r);
}

textsizes(l: list of ref (int, int)): list of string
{
	if(l == nil)
		return ""::nil;
	r: list of string;
	for(; l != nil; l = tl l) {
		(a, b) := *hd l;
		if(a < 0)
			r = "?"::r;
		else if(b == a+1)
			r = sprint("%d", a)::r;
		else if(b >= 0)
			r = sprint("%d-%d", a, b-1)::r;
		else
			r = sprint("%d-", a)::r;
	}
	return rev(r);
}

Fspec.text(fs: self ref Fspec): string
{
	return sprint("%s.%s.%s.%s",
		join(textnames(fs.ps), ","),
		join(textnames(fs.nms), ","),
		join(textstyles(fs.sts), ","),
		join(textsizes(fs.szs), ","));
}

Fspec.parse(s: string, flags: int, c: ref Cfg): (ref Fspec, string)
{
	{
		return (xparsefspec(s, flags, c), nil);
	} exception x {
	"error:*" =>
		return (nil, x[len "error:":]);
	}
}

xparsefspec(s: string, flags: int, c: ref Cfg): ref Fspec 
{
#say(sprint("xparsefspec %q", s));
	(ps, rem) := str->splitstrl(s, ".");
	if(rem == nil)
		raise "error:bad format, no dot";
	s = rem[1:];
	szs, sts, nms: string;
	(s, szs) = str->splitstrr(s, ".");
	if(s == nil)
		raise "error:bad format, no sizes";
	(s, sts) = str->splitstrr(s[:len s-1], ".");
	if(s == nil)
		raise "error:bad format, no styles";
	nms = s[:len s-1];
	(nil, pl) := xparsenames(ps);
	(opt1, nml) := xparsenames(nms);
	(opt2, stl) := xparsestyles(sts);
	(opt3, szl) := xparsesizes(szs, flags&Fpointsize, c);
	opt := opt1|opt2|opt3;
	if((opt & Fquestion) && (flags & Fquestion) == 0)
		raise "error:'?' not allowed";
	if((opt & Fstar) && (flags & Fstar) == 0)
		raise "error:'*' not allowed";
	return ref Fspec(pl, nml, stl, szl);
}

xparsenames(s: string): (int, list of string)
{
	if(s == nil || s == "*")
		return (Fstar, nil);
	if(s == "?")
		return (Fquestion, list of {""});
	r: list of string;
	for(l := sys->tokenize(s, ",").t1; l != nil; l = tl l)
		r = str->tolower(hd l)::r;
	return (0, rev(r));
}

xparsestyles(s: string): (int, list of int)
{
	if(s == nil || s == "*")
		return (Fstar, nil);
	if(s == "?")
		return (Fquestion, list of {-1});
	r: list of int;
	for(l := sys->tokenize(s, ",").t1; l != nil; l = tl l) {
		if((st := find(styles, hd l)) < 0)
			raise sprint("error:bad style %#q", hd l);
		r = st::r;
	}
	rr: list of int;
	for(; r != nil; r = tl r)
		rr = hd r::rr;
	return (0, rr);
}

xint(s: string): int
{
	(i, rem) := str->toint(s, 10);
	if(rem != nil)
		raise sprint("error:bad int %#q", s);
	return i;
}

xparsesizes(s: string, pt: int, c: ref Cfg): (int, list of ref (int, int))
{
	if(s == nil || s == "*")
		return (Fstar, nil);
	if(s == "?")
		return (Fquestion, list of {ref (-1, -1)});
	r: list of ref (int, int);
	for(l := sys->tokenize(s, ",").t1; l != nil; l = tl l) {
		(as, bs) := str->splitstrl(s, "-");
		a := xint(as);
		if(pt)
			a = a*c.ydpi/72;
		if(bs == nil) {
			r = ref (a, a+1)::r;
			continue;
		}
		bs = bs[1:];
		if(bs == nil)
			r = ref (a, -1)::r;
		else {
			b := xint(bs);
			if(pt)
				b = b*c.ydpi/72;
			r = ref (a, b)::r;
		}
	}
	return (0, rev(r));
}


Fmatch.path(fm: self ref Fmatch): string
{
	path := fm.f.path;
	if(!fm.f.single)
		path += sprint(".%s.%d.font", styles[fm.st], fm.sz);
	return path;
}

Fmatch.text(fm: self ref Fmatch): string
{
	return sprint(".%s.%s.%d", fm.nm, styles[fm.st], fm.sz);
}

matchprog(a, b: list of string): int
{
	if(b == nil)
		return 1;
	for(; a != nil; a = tl a)
		for(l := b; l != nil; l = tl l) {
			s := hd b;
			if(hd a == s || s != nil && s[len s-1] == '*' && str->prefix(s[:len s-1], hd a))
				return 1;
		}
	return 0;
}

matchname(a, b: list of string): int
{
	if(b == nil)
		return 1;
	for(; a != nil; a = tl a)
		for(l := b; l != nil; l = tl l)
			if(hd a == hd l)
				return 1;
	return 0;
}

matchstyle(a, b: list of int): int
{
	if(b == nil)
		return 1;
	for(; a != nil; a = tl a)
		for(l := b; l != nil; l = tl l)
			if(hd a == hd l)
				return 1;
	return 0;
}

matchsize(a, b: list of ref (int, int)): int
{
	if(b == nil)
		return 1;
	for(; a != nil; a = tl a)
		for(l := b; l != nil; l = tl l) {
			(as, ae) := *hd a;
			(bs, be) := *hd l;
			if(bs < 0) {
				if(as < 0)
					return 1;
				continue;
			}
			if(as < 0)
				continue;
			if(as >= bs && (as < be || be < 0) ||
				ae > bs && (ae <= be || be < 0) || 
				as < bs && ae > be)
				return 1;
		}
	return 0;
}

# does a match b?
rewritematch(a, b: ref Fspec): int
{
	if(matchprog(a.ps, b.ps))
	if(matchname(a.nms, b.nms))
	if(matchstyle(a.sts, b.sts))
	if(matchsize(a.szs, b.szs))
		return 1;
	return 0;
}

rewrite(fs: ref Fspec)
{
#say(sprint("rewriting %s", fs.text()));
	for(l := config.rewrites; l != nil; l = tl l) {
		(o, n) := *hd l;
		if(!rewritematch(fs, o))
			continue;
		rewrite(n);
		nfs := ref *fs;
		if(n.ps != nil) nfs.ps = n.ps;
		if(n.nms != nil) nfs.nms = n.nms;
		if(n.sts != nil) nfs.sts = n.sts;
		if(n.szs != nil) nfs.szs = n.szs;
		if(dflag) say(sprint("rewritten, %s -> %s: %s -> %s", o.text(), n.text(), fs.text(), nfs.text()));
		*fs = *nfs;
	}
}

fontfindname(s: string): ref Font
{
	for(l := config.fonts; l != nil; l = tl l)
	for(n := (hd l).fs.nms; n != nil; n = tl n)
		if(s == hd n)
			return hd l;
	return nil;
}


zerofmatch: Fmatch;
fontfind(fs: ref Fspec): ref Fmatch
{
	fm := ref zerofmatch;
	for(l := config.fonts; l != nil; l = tl l)
		score(fs, hd l, fm);
	if(fm.score == 0)
		fm = nil;
	return fm;
}

# in order of importance while selecting:
# exact font name
# exact style
# exact size
# substring font name, 8 bit
# 255-abs difference in px, 8 bit
# is fallback
Sfallback: con 1<<0;
Ssizeoffshift: con 1;
Ssizeoffmask: con 16rff;
Ssubstrmask: con 16rff;
Ssubstrshift: con Ssizeoffshift+8;
Ssize,
Sstyle,
Sname: con 1<<(Ssubstrshift+8+iota);

score(fs: ref Fspec, f: ref Font, fm: ref Fmatch)
{
	for(nl := fs.nms; nl != nil; nl = tl nl)
	for(stl := fs.sts; stl != nil; stl = tl stl)
	for(szl := fs.szs; szl != nil; szl = tl szl)
		scorefont(hd nl, hd stl, hd szl, f, fm);
}

scorefont(n: string, st: int, sz: ref (int, int), f: ref Font, fm: ref Fmatch)
{
	fs := f.fs;
	for(nl := fs.nms; nl != nil; nl = tl nl)
	for(stl := fs.sts; stl != nil; stl = tl stl)
	for(szl := fs.szs; szl != nil; szl = tl szl) {
		msz: int;
		v := 0;
		if(n == hd nl)
			v |= Sname;
		else if(issubstr(n, hd nl))
			v |= ((Ssubstrmask-len hd nl)&Ssubstrmask)<<Ssubstrshift;
		else if(hd nl == config.fallback)
			v |= Sfallback;
		if(st == hd stl)
			v |= Sstyle;
		fsz := hd szl;
		if(sz.t0 >= fsz.t0 && (sz.t1 <= fsz.t1 || fsz.t1 < 0)) {
			v |= Ssize;
			msz = sz.t0;
		} else {
			o: int;
			if(sz.t0 > fsz.t1) {
				msz = fsz.t1;
				o = sz.t0-fsz.t1;
			} else {
				msz = fsz.t0;
				o = fsz.t0-sz.t1;
			}
			v |= ((Ssizeoffmask-o)&Ssizeoffmask)<<Ssizeoffshift;
		}
		if(v > fm.score) {
			nfm := ref Fmatch(f, hd nl, hd stl, msz, v);
			if(fm.score > 0)
				if(dflag) say(sprint("old fmatch %s score %ux, new fmatch %s score %ux", fm.text(), fm.score, nfm.text(), nfm.score));
			else
				if(dflag) say(sprint("first match %s score %ux", nfm.text(), nfm.score));
			*fm = *nfm;
		}
	}
}

issubstr(sub, s: string): int
{
	nsub := len sub;
	ns := len s;
	n := ns-nsub;
next:
	for(i := 0; i <= n; i++) {
		for(j := 0; j < nsub; j++)
			if(s[i+j] != sub[j])
				continue next;
		return 1;
	}
	return 0;
}

cacheset(t: Query, c0, c1: array of Query)
{
	nc := array[Ncache] of Query;
	nc[0] = t;
	nc[1:] = c0;
	nc[1+len c0:] = c1;
	cache = nc;
}

# query -> (orig, rewritten, path, error)
lookup(s: string): (string, string, string, string)
{
	for(i := 0; i < len cache; i++) {
		(q, o, m, r) := cache[i];
		if(q == s) {
			if(i != 0)
				cacheset((q, o, m, r), cache[:i], cache[i+1:]);
			if(dflag) say(sprint("lookup, hit, %q -> %q -> %q", s, m, r));
			return (o, m, r, nil);
		}
	}

	if(dflag) say(sprint("lookup: %s", s));
	(ps, rem) := str->splitstrl(s, ".");
	if(rem == nil)
		return (nil, nil, nil, "bad format, no dot");

	if(ps == "fontsrv")
		return (rem[1:], rem[1:], rem[1:], nil);

	fs := config.lit.find(rem[1:]);
	if(fs == nil) {
		err: string;
		(fs, err) = Fspec.parse(s, Fquestion|Fpointsize, ref config);
		if(err != nil)
			return (nil, nil, nil, err);
	} else {
		fs = ref *fs;
		{
			opt: int;
			(opt, fs.ps) = xparsenames(ps);
			if(opt & Fstar)
				return (nil, nil, nil, "'*' not allowed");
		} exception x {
		"error:*" =>
			return (nil, nil, nil, x[len "error:":]);
		}
	}
	ofs := ref *fs;
	if(dflag) say(sprint("before rewrite, %s", fs.text()));
	rewrite(fs);
	if(dflag) say(sprint("after rewrite, %s", fs.text()));
	fm := fontfind(fs);
	if(fm == nil)
		(o, m, r) := ("", "", "*default*");
	else
		(o, m, r) = (ofs.text(), fm.text(), fm.path());
	cacheset((s, o, m, r), cache[:Ncache-1], array[0] of Query);
	if(dflag) say(sprint("lookup, miss, %q -> %q -> %q", s, m, r));
	return (o, m, r, nil);
}

mknames(): string
{
	tab := Strhash[string].new(11, nil);
	r: string;
	for(l := config.fonts; l != nil; l = tl l) {
		nm := hd (hd l).fs.nms;
		if(tab.find(nm) == nil) {
			r += ","+nm;
			tab.add(nm, nm);
		}
	}
	if(r != nil)
		r = r[1:];
	return r;
}

xwrite(f: ref Fid, m: ref Tmsg.Write, s: string)
{
	f.data = array of byte s;
	srv.reply(ref Rmsg.Write(m.tag, len m.data));
}

styxsrv(msgc: chan of ref Tmsg)
{
	sys->pctl(Sys->FORKNS, nil);
next:
	for(;;) {
		mm := <-msgc;
		if(mm == nil)
			break next;
		pick m := mm {
		Readerror =>
			warn("read error: "+m.error);
			break next;
		}
		dostyx(mm);
	}
	tree.quit();
	killgrp(pid());
}

dostyx(mm: ref Tmsg)
{
	pick m := mm {
	Write =>
		(f, err) := srv.canwrite(m);
		if(f == nil)
			return replyerror(m, err);
		case int f.path {
		Qctl =>
			case string m.data {
			"reload" or
			"reload\n" =>
				err = readconfig();
				if(err != nil)
					replyerror(m, err);
				else {
					cache = array[Ncache] of Query;
					srv.reply(ref Rmsg.Write(m.tag, len m.data));
				}
				return;
			* =>
				return replyerror(m, "bad request");
			}
		Qlookup =>
			if(dflag) say(sprint("Twrite Qlookup, %q", string m.data));
			mm, r: string;
			(nil, mm, r, err) = lookup(string m.data);
			if(err != nil)
				replyerror(m, err);
			else
				xwrite(f, m, sprint("%s\n%s\n", r, mm));
			return;
		Qlist =>
			s := string m.data;
			a := l2a(str->unquoted(s));
			if(len a == 0)
				return replyerror(m, "empty request");
			r: string;
			case a[0] {
			"names" =>
				r = ","+mknames();
			"styles" =>
				if(len a != 2)
					return replyerror(m, "'styles' takes 1 argument");
				ff := fontfindname(a[1]);
				if(ff == nil)
					return replyerror(m, "no such font");
				for(sts := ff.fs.sts; sts != nil; sts = tl sts)
					r += ","+styles[hd sts];
			"sizes" =>
				if(len a != 3)
					return replyerror(m, "'sizes' takes 2 arguments");
				st := find(styles, a[2]);
				if(st < 0)
					return replyerror(m, "bad style");
				ff := fontfindname(a[1]);
				if(ff == nil)
					return replyerror(m, "no such font");
				for(l := ff.fs.sts; l != nil && hd l != st; l = tl l)
					{}
				if(l == nil)
					break; # no font sizes
				for(szs := ff.fs.szs; szs != nil; szs = tl szs) {
					(b, e) := *hd szs;
					r += ","+string b;
					if(e != b+1) {
						r += "-";
						if(e >= 0)
							r += string e;
					}
				}
			* =>
				return replyerror(m, "bad request");
			}
			if(r != nil)
				r = r[1:];  # comma
			return xwrite(f, m, r);
		}
	Read =>
		(f, nil) := srv.canread(m);
		if(f != nil)
			case int f.path {
			Qlookup =>
				m.offset = big 0;
			Qcache =>
				if(f.data == nil) {
					s: string;
					for(i := 0; i < len cache && cache[i].t0 != nil; i++) {
						(q, o, r, p) := cache[i];
						s += sprint("%q %q %q %q\n", q, o, r, p);
					}
					f.data = array of byte s;
				}
			}
			case int f.path {
			Qlookup or
			Qlist or
			Qcache =>
				if(f.data == nil && int f.path == Qlist)
					f.data = array of byte mknames();
				if(f.data == nil)
					replyerror(m, "no query set");
				else {
					if(int f.path == Qlookup && dflag)
						say(sprint("Rread Qlookup, %q", string f.data));
					srv.reply(styxservers->readbytes(m, f.data));
				}
				return;
			}
	}
	srv.default(mm);
}

dir(name: string, mode, path: int): Sys->Dir
{
	d := sys->zerodir;
	d.name = name;
	d.uid = d.gid = "fontsrv";
	d.qid.path = big path;
	d.qid.qtype = Sys->QTFILE;
	if(mode&Sys->DMDIR)
		d.qid.qtype = Sys->QTDIR;
	d.mode = mode;
	return d;
}

replyerror(m: ref Tmsg, s: string)
{
	srv.reply(ref Rmsg.Error(m.tag, s));
}

find(a: array of string, s: string): int
{
	for(i := 0; i < len a; i++)
		if(a[i] == s)
			return i;
	return -1;
}

join(l: list of string, sep: string): string
{
	if(l == nil)
		return nil;
	s := "";
	for(; l != nil; l = tl l)
		s += sep+hd l;
	return s[len sep:];
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

suffix(suf, s: string): int
{
	return len s >= len suf && s[len s-len suf:] == suf;
}

pid(): int
{
	return sys->pctl(0, nil);
}

killgrp(pid: int)
{
	sys->fprint(sys->open(sprint("/prog/%d/ctl", pid), Sys->OWRITE), "killgrp");
}

say(s: string)
{
	if(dflag)
		warn(s);
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
