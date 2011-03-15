implement Showfonts;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
include "arg.m";
include "string.m";
	str: String;
include "tk.m";
	tk: Tk;
include "tkclient.m";
	tkclient: Tkclient;

Showfonts: module {
	init:	fn(ctxt: ref Draw->Context, argv: list of string);
};


ftsrvpath: con "/mnt/font";

gen: int;

listwidget: string;
top: ref Tk->Toplevel;
wmctl: chan of string;
cfid: string;

init(ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(ctxt == nil)
		fail("no window context");
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	str = load String String->PATH;
	tk = load Tk Tk->PATH;
	tkclient = load Tkclient Tkclient->PATH;

	arg->init(args);
	arg->setusage(arg->progname()+" [names styles sizes ...]");
	while((c := arg->opt()) != 0)
		case c {
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args % 3 != 0)
		arg->usage();

	sys->pctl(Sys->NEWPGRP, nil);
	tkclient->init();
	(top, wmctl) = tkclient->toplevel(ctxt, "", "showfonts", Tkclient->Appl);

	cmdc := chan of string;
	tk->namechan(top, cmdc, "cmd");

	tkcmd("frame .n");

	tkcmd("frame .l");
	tkcmd("frame .l.f");
	tkcmd("button .l.f.b -text set -command {send cmd set}");
	tkcmd("button .l.f.c -text cancel -command {pack forget .l; .n.error configure -text {}}");
	tkcmd("listbox .l.l -selectmode multiple -yscrollcommand {.l.sy set}");
	tkcmd("scrollbar .l.sy -command {.l.l yview}");
	tkcmd("pack .l.sy -side left -fill y");
	tkcmd("pack .l.f");
	tkcmd("pack .l.f.b .l.f.c -side left");
	tkcmd("pack .l.l -fill both -expand 1");
	
	tkcmd("frame .ff");
	tkcmd("canvas .ff.c -yscrollcommand {.ff.sy set}");
	tkcmd("frame .f");
	cfid = tkcmd(".ff.c create window 0 0 -window .f -anchor nw");
	tkcmd("scrollbar .ff.sy -command {.ff.c yview}");
	tkcmd("pack .ff.sy -side left -fill y");
	tkcmd("pack .ff.c -expand 1 -fill both");
	ww := list of {
	("t",	"text",		"The quick brown fox jumps over the lazy dog 1234567890 !@#$%^&*({|;\"<?["),
	("n",	"name",		"?"),
	("st",	"style",	"?"),
	("sz",	"size",		"?"),
	};
	row := 0;
	for(; ww != nil; ww = tl ww) {
		(w, lb, txt) := hd ww;
		if(w == "t")
			tkcmd(sprint("label .n.x%s -text %s", w, tk->quote(lb)));
		else
			tkcmd(sprint("button .n.x%s -text %s -command {send cmd %s}", w, tk->quote(lb), lb));
		tkcmd(sprint("entry .n.%s", w));
		if(txt != nil)
			tkcmd(sprint(".n.%s insert 0 %s", w, tk->quote(txt)));
		if(w == "t")
			tkcmd(sprint(".n.t configure -width %dw", len txt));
		tkcmd(sprint("grid -row %d -column 0 .n.x%s -sticky we", row, w));
		tkcmd(sprint("grid -row %d -column 1 .n.%s -sticky we", row, w));
		row++;
	}
	tkcmd("button .n.b.add -text add -command {send cmd add}");
	tkcmd("button .n.b.clear -text clear -command {send cmd clear}");
	tkcmd("label .n.error");
	tkcmd("frame .n.b");
	tkcmd("grid .n.b -columnspan 2 -sticky w");
	tkcmd("pack .n.b.add .n.b.clear -side left");
	tkcmd("grid .n.error -columnspan 2 -sticky we");
	tkcmd("grid columnconfigure .n 1 -weight 1");
	for(wl := list of {"n", "st", "sz"}; wl != nil; wl = tl wl)
		tkcmd(sprint("bind .n.%s {<Key-\n>} {send cmd add}", hd wl));
	for(wl = list of {"n", "st", "sz", "b.add", "b.clear"}; wl != nil; wl = tl wl)
		tkcmd(sprint("bind .n.%s {<Key-\t>} {focus next}", hd wl));
	tkcmd("pack .n -expand 1 -fill x -anchor nw");
	tkcmd("pack .ff -expand 1 -fill both -anchor nw");

	a := l2a(args);
	tkcmd("pack propagate . 0");
	for(i := 0; i < len a; i += 3)
		fontsadd(a[i:i+3], tkcmd(".n.t get"));
	canvassize();

	tkclient->onscreen(top, nil);
	tkclient->startinput(top, "kbd"::"ptr"::nil);

	for(;;) alt {
	s := <-top.ctxt.kbd =>
		tk->keyboard(top, s);

	s := <-top.ctxt.ptr =>
		tk->pointer(top, *s);

	s := <-top.ctxt.ctl or
	s = <-top.wreq or
	s = <-wmctl =>
		tkclient->wmctl(top, s);
		if(str->prefix("!", s))
			canvassize();

	s := <-cmdc =>
		sys->print("cmd: %q\n", s);
		cmd(s);
		tkcmd("update");
	}
}

canvassize()
{
	w := tkcmd(".ff.c cget -actwidth");
	tkcmd(sprint(".ff.c itemconfigure %s -width %s", cfid, w));
	tkcmd(".ff.c configure -scrollregion {0 0 [.ff.c cget -width] [.f cget -height]}");
}

lookup(fs: string): (string, string, string)
{
	p := sprint("%s/lookup", ftsrvpath);
	fd := sys->open(p, Sys->ORDWR);
	if(fd == nil)
		return (nil, nil, sprint("open %q: %r", p));
	if(sys->write(fd, d := array of byte fs, len d) != len d)
		return (nil, nil, sprint("write %q: %r", p));
	buf := array[1024] of byte;
	n := sys->read(fd, buf, len buf);
	if(n < 0)
		return (nil, nil, sprint("read %q: %r", p));
	v := string buf[:n];
	fp: string;
	(fp, v) = str->splitstrl(v, "\n");
	if(v != nil)
		v = v[1:];
	if(v != nil && v[len v-1] == '\n')
		v = v[:len v-1];
	return (v, fp, nil);
}

fontadd(n, st, sz, text: string)
{
	i := gen++;
	lw := sprint(".f.l%d", i);
	w := sprint(".f.t%d", i);
	fs := sprint("%s.%s.%s", n, st, sz);
	(nfs, np, err) := lookup("showfonts."+fs);
	if(err != nil)
		return warn(err);
	tkcmd(sprint("label %s -text %s -fg #404040", lw, tk->quote(sprint("%s -> %s -> %s", fs, nfs, np))));
	tkcmd(sprint("entry %s -font %s", w, tk->quote(fs)));
	tkcmd(sprint("%s delete 0 end; %s insert 0 %s", w, w, tk->quote(text)));
	tkcmd(sprint("pack %s -expand 1 -fill x -anchor w", lw));
	tkcmd(sprint("pack %s -expand 1 -fill x -anchor w", w));
}

fontsadd(v: array of string, text: string)
{
	for(c := sys->tokenize(v[2], ",").t1; c != nil; c = tl c)
	for(b := sys->tokenize(v[1], ",").t1; b != nil; b = tl b)
	for(a := sys->tokenize(v[0], ",").t1; a != nil; a = tl a)
		fontadd(hd a, hd b, hd c, text);
}

fsget(): array of string
{
	a := array[3] of string;
	i := 0;
	for(wl := list of {"n", "st", "sz"}; wl != nil; wl = tl wl) {
		w := sprint(".n.%s", hd wl);
		a[i++] = tkcmd(sprint("%s get", w));
	}
	return a;
}

enum(s: string): list of string
{
	p := sprint("%s/list", ftsrvpath);
	fd := sys->open(p, Sys->ORDWR);
	if(fd != nil)
	if(sys->fprint(fd, "%s", s) >= 0)
	if(sys->seek(fd, big 0, sys->SEEKSTART) == big 0)
	if((n := sys->readn(fd, buf := array[1024] of byte, len buf)) >= 0)
		return sys->tokenize(string buf[:n], ",").t1;
	error(sprint("%r"));
	return nil;
}

listfill(w: string, l: list of string)
{
	listwidget = w;
	tkcmd("pack forget .l");
	tkcmd("pack .l -after .n -fill x");
	tkcmd(".l.l delete 0 end");
	for(; l != nil; l = tl l)
		tkcmd(sprint(".l.l insert end %s", tk->quote(hd l)));
}

cmd(s: string)
{
	case s {
	"add" =>
		fontsadd(fsget(), tkcmd(".n.t get"));
		canvassize();
	"clear" =>
		for(i := 0; i < gen; i++) {
			lw := sprint(".f.l%d", i);
			w := sprint(".f.t%d", i);
			tkcmd(sprint("pack forget %s %s", lw, w));
			tkcmd(sprint("destroy %s %s", lw, w));
		}
		gen = 0;
	"name" =>
		l := enum("names");
		listfill(".n.n", l);
	"style" =>
		l := enum(sprint("styles %q", tkcmd(".n.n get")));
		listfill(".n.st", l);
	"size" =>
		l := enum(sprint("sizes %q %q", tkcmd(".n.n get"), tkcmd(".n.st get")));
		listfill(".n.sz", l);
	"set" =>
		r: list of string;
		for(l := sys->tokenize(tkcmd(".l.l curselection"), " ").t1; l != nil; l = tl l)
			r = tkcmd(".l.l get "+hd l)::r;
		w := listwidget;
		r = rev(r);
		tkcmd(sprint("%s delete 0 end; %s insert 0 %s", w, w, tk->quote(join(r, ","))));
		tkcmd("pack forget .l");
	}
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

join(l: list of string, sep: string): string
{
	if(l == nil)
		return nil;
	r: string;
	for(; l != nil; l = tl l)
		r += sep+hd l;
	return r[len sep:];
}

error(s: string)
{
	tkcmd(".n.error configure -text "+tk->quote(s));
}

tkcmd(s: string): string
{
	r := tk->cmd(top, s);
	if(r != nil && r[0] == '!')
		warn(sprint("tkcmd: %q -> %s", s, r));
	return r;
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
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
