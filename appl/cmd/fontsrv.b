implement Fontsrv;

# Qfs requests (writes):
# - free-form string (e.g. /fonts/pelm/unicode.8.font)
# - font-spec, i.e.: name-style-size[-n|+n], with name, style & size optionally don't-cared by a question mark
# style is one of:  r (regular), i (italic), b (bold), ib (italic+bold)
# size is a number >= 2.
#
# Qfs responses (reads):
# - "ok" path name style size
# - "err" message
# for "ok", the name, style & size may be a question mark when it is unknown.
# new requests reset the response.  responses can be read multiple times, offsets are ignore.

# config file format:
#
# "ttf" name style sizes path-stem
# "font" name style size path
# "name" oname nname
# "alias" str font-spec
#
# a "name" specifies an alias for in a font-spec (e.g. "serif" can map to SomeFontName).
# an "alias" specified a font-spec for a given exact string (e.g. "sans-r-8" for /fonts/pelm/unicode.8.font).
# a "font" specifies a normal inferno font.
# a "ttf" specifies a ttf font and the sizes in which it is available.

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


dflag: int;
fontdb := "/fonts/fontdb";

Wttf, Wfont: con iota;  # Font.which
Sitalic, Sbold: con 1<<iota; # Font.style
Font: adt {
	which:	int;
	name:	string;
	style:	int;
	sizes:	array of ref (int, int);
			# start-end (exclusive).  end may be -1, denoting no limit.
			# for Wfont, sizes[0].t0 is the size
	path:	string;	# full path for Wfont, dir+stem without .size.font for Wttf
	next:	cyclic ref Font;
};

Fontspec: adt {
	name:	string;
	style:	int;
	size:	int;
	off:	int;
};

fonts,
fontslast: ref Font;
names: ref Strhash[string]; # name -> name
aliases: ref Strhash[ref Fontspec]; # str -> font-spec

Qroot, Qfs, Qfsctl: con iota;

srv: ref Styxserver;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	styx = load Styx Styx->PATH;
	styx->init();
	styxservers = load Styxservers Styxservers->PATH;
	styxservers->init(styx);
	nametree = load Nametree Nametree->PATH;
	nametree->init();
	tables = load Tables Tables->PATH;

	sys->pctl(Sys->NEWPGRP|Sys->FORKNS, nil);

	arg->init(args);
	arg->setusage(arg->progname()+" [-d] [-f fontdb]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		'f' =>	fontdb = arg->arg();
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args != 0)
		arg->usage();

	names = names.new(11, nil);
	aliases = aliases.new(11, nil);
	readconfig(fontdb);

	(tree, navc) := nametree->start();
	tree.create(big Qroot, dir(".", 8r555|Sys->DMDIR, Qroot));
	tree.create(big Qroot, dir("fs", 8r666, Qfs));
	tree.create(big Qroot, dir("fsctl", 8r222, Qfsctl));

	msgc: chan of ref Tmsg;
	(msgc, srv) = Styxserver.new(sys->fildes(0), Navigator.new(navc), big Qroot);

	styxsrv(msgc);
	tree.quit();
	killgrp(pid());
}

parsefontspec(s: string): (ref Fontspec, string)
{
	name, sts, szs: string;
	off := 0;
	(name, s) = str->splitstrl(s, "-");
	if(s != nil)
		(sts, s) = str->splitstrl(s[1:], "-");
	if(s != nil)
		(szs, s) = str->splitl(s[1:], "+-");
	if(s != nil) {
		(off, s) = str->toint(s, 10);
		if(s != nil)
			return (nil, sprint("bad offset %#q", s));
	}
	if(name == nil || sts == nil || szs == nil)
		return (nil, "empty name, style or size");
	if(name == "?")
		name = "";
	if(sts == "?")
		st := -1;
	else if((st = getstyle(sts)) < 0)
		return (nil, sprint("bad style %#q", sts));
	if(szs == "?")
		sz := -1;
	else if((sz = getpos(szs)) < 0)
		return (nil, sprint("bad size %#q", szs));
	fs := ref Fontspec(name, st, sz, off);
	return (fs, nil);
}

# returns whether font has size
fontcansize(f: ref Font, size: int): int
{
	for(i := 0; i < len f.sizes; i++) {
		(s, e) := *f.sizes[i];
		if(size >= s && (e == -1 || size < e))
			return 1;
	}
	return 0;
}

fontmatch(f: ref Font, fs: ref Fontspec): int
{
	return (fs.name == nil || fs.name == f.name) &&
		(fs.style < 0 || fs.style == f.style) &&
		(fs.size < 0 || fontcansize(f, fs.size));
}

sort[T](a: array of T, ge: ref fn(a, b: T): int)
{
	for(i := 1; i < len a; i++) {
		tmp := a[i];
		for(j := i; j > 0 && ge(a[j-1], tmp); j--)
			a[j] = a[j-1];
		a[j] = tmp;
	}
}

sizege(a, b: ref Font): int
{
	return a.sizes[0].t0 >= b.sizes[0].t0;
}

# return all matching fonts.
fontenum(x: ref Font, fs: ref Fontspec): (array of ref Font, int)
{
	l: list of ref Font;
	i := 0;
	m := -1;
	for(f := fonts; f != nil; f = f.next)
		if(fontmatch(f, fs)) {
			l = f::l;
			if(f == x)
				m = i;
			i++;
		}
	if(m < 0)
		return (nil, -1);
	a := l2a(l);
	sort(a, sizege);
	return (a, m);
}

# find the font name,style,size,off.
# name nil -> don't care.
# style/size -1 -> don't care.
# does not perform lookup in "names".
# first hit is returned.
fontfind(fs: ref Fontspec): (ref Font, int)
{
	name := names.find(fs.name);
	if(name != nil) {
		fs = ref *fs;
		fs.name = name;
	}
	for(f := fonts; f != nil; f = f.next) {
		if(!fontmatch(f, fs))
			continue;
		if(fs.off != 0) {
			case f.which {
			Wttf =>
				fs = ref *fs;
				if(fs.size < 0)
					fs.size = f.sizes[0].t0;
				fs.size += fs.off;
				if(fontmatch(f, fs))
					return (f, fs.size);
			Wfont =>
				nfs := ref Fontspec(f.name, f.style, -1, 0);
				(a, i) := fontenum(f, nfs);
				i += fs.off;
				if(i >= 0 && i < len a)
					return (a[i], a[i].sizes[0].t0);
			}
		} else {
			sz := fs.size;
			if(sz < 0)
				sz = f.sizes[0].t0;
			return (f, sz);
		}
	}
	return (nil, -1);
}

styles := array[] of {"r", "i", "b", "ib"};
getstyle(s: string): int
{
	for(i := 0; i < len styles; i++)
		if(styles[i] == s)
			return i;
	return -1;
}

getstyles(s: string): array of int
{
	(nil, l) := sys->tokenize(s, ",");
	if(l == nil)
		return nil;
	a := array[len l] of int;
	i := 0;
	for(; l != nil; l = tl l) {
		st := getstyle(hd l);
		if(st < 0)
			return nil;
		a[i++] = st;
	}
	return a;
}

getpos(s: string): int
{
	(v, rem) := str->toint(s, 10);
	if(rem != nil)
		return -1;
	return v;
}

getsizes(sz: string): array of ref (int, int)
{
	(nil, l) := sys->tokenize(sz, ",");
	if(l == nil)
		return nil;
	i := 0;
	a := array[len l] of ref (int, int);
	for(; l != nil; l = tl l) {
		(s, e) := str->splitstrl(hd l, "-");
		if(e == nil) {
			v := getpos(s);
			if(v < 0)
				return nil;
			a[i++] = ref (v, v+1);
			continue;
		}
		e = e[1:];
		ss := 2;
		ee := -1;
		if(s != nil) {
			ss = getpos(s);
			if(ss < 0)
				return nil;
		}
		if(e != nil) {
			ee = getpos(e);
			if(ee < 0)
				return nil;
		}
		a[i++] = ref (ss, ee);
	}
	return a;
}

fontadd(f: ref Font)
{
say(sprint("fontadd, name=%q, style=%d, nsizes=%d", f.name, f.style, len f.sizes));
	if(fonts == nil)
		fonts = f;
	else
		fontslast.next = f;
	fontslast = f;
}

checkargs(p, cmd: string, l, n, h: int)
{
	if(n != h)
		fail(sprint("%q:%d: %s needs %d args, saw %d", p, l, cmd, n, h));
}

readconfig(p: string)
{
	b := bufio->open(p, bufio->OREAD);
	n := 0;
	for(;;) {
		s := b.gets('\n');
		if(s == nil) {
			if(b.getc() != bufio->EOF)
				fail(sprint("read: %r"));
			break;
		}
		n++;
		a := l2a(str->unquoted(s));
		if(len a == 0 || str->prefix("#", str->drop(a[0], " \t")))
			continue;
		case a[0] {
		"alias" =>
			checkargs(p, a[0], n, 2, len a-1);
			(fs, err) := parsefontspec(a[2]);
			if(err != nil)
				fail(sprint("%q:%d: bad font-spec: %s", p, n, err));
			aliases.del(a[1]);
			aliases.add(a[1], fs);
		"name" =>
			checkargs(p, a[0], n, 2, len a-1);
			names.del(a[1]);
			names.add(a[1], a[2]);
		"ttf" or
		"font" =>
			checkargs(p, a[0], n, 4, len a-1);
			sts := getstyles(a[2]);
			if(sts == nil)
				fail(sprint("%q:%d: bad styles %#q", p, n, a[2]));
			szs := getsizes(a[3]);
			if(szs == nil)
				fail(sprint("%q:%d: bad sizes %#q", p, n, a[3]));
			which := Wfont;
			if(a[0] == "ttf")
				which = Wttf;
			for(i := 0; i < len sts; i++)
				fontadd(ref Font(which, a[1], sts[i], szs, a[4], nil));
		* =>
			fail(sprint("%q:%d: bad directive %#q", p, n, a[0]));
		}
	}
}

styxsrv(msgc: chan of ref Tmsg)
{
	for(;;) {
		mm := <-msgc;
		if(mm == nil)
			return;
		pick m := mm {
		Readerror =>
			return warn("read error: "+m.error);
		}
		dostyx(mm);
	}
}

xwrite(f: ref Fid, m: ref Tmsg.Write, s: string)
{
say(sprint("xwrite, %s -> %s", string m.data, s));
	f.data = array of byte s;
	srv.reply(ref Rmsg.Write(m.tag, len m.data));
}

dostyx(mm: ref Tmsg)
{
	pick m := mm {
	Write =>
		(f, err) := srv.canwrite(m);
		if(f == nil)
			return replyerror(m, err);
		case int f.path {
		Qfs =>
			s := string m.data;
			fs := aliases.find(s);
			if(fs == nil) {
				err: string;
				(fs, err) = parsefontspec(s);
				if(err != nil) {
					(ok, nil) := sys->stat(s);
					if(ok >= 0)
						return xwrite(f, m, sprint("ok %q ? ? ?", s));
					return xwrite(f, m, sprint("err %q", "no such file, and bad font-spec: "+err));
				}
			}
			(ff, sz) := fontfind(fs);
			if(ff != nil) {
				path := ff.path;
				if(ff.which == Wttf)
					path += sprint(".%d.font", sz);
				return xwrite(f, m, sprint("ok %q %q %q %d", path, ff.name, styles[ff.style], sz));
			}
			return xwrite(f, m, sprint("err %q", "no match"));
		Qfsctl =>
			return replyerror(m, "not yet");
		}
	Read =>
		(f, nil) := srv.canread(m);
		if(f != nil)
			case int f.path {
			Qfs =>
				m.offset = big 0;
				srv.reply(styxservers->readbytes(m, f.data));
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

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
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
