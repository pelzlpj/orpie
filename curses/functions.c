/* addch */

ML1(addch,err,chtype)
ML2(waddch,err,window,chtype)
ML3(mvaddch,err,int,int,chtype)
ML4(mvwaddch,err,window,int,int,chtype)
ML1(echochar,err,chtype)
ML2(wechochar,err,window,chtype)

/* addchstr */

#define copie(l,id,ar) int i,c=l,r; \
  chtype *t=malloc((c+1)*sizeof(chtype)); \
  if(t==NULL) failwith("Out of memory"); \
  for(i=0;i<c;i++) t[i]=Int_val(Field(ar,i+id)); \
  t[i]=0;
#define call(f) r=f; free(t); r_err(r);
ML1d(addchstr,err,chtype array)
BEG1 copie(Wosize_val(aa),0,aa) call(addchstr(t)) END
ML2d(waddchstr,err,window,chtype array)
BEG2 copie(Wosize_val(ab),0,ab) call(waddchstr(a_window(aa),t)) END
ML3d(mvaddchstr,err,int,int,chtype array)
BEG3 copie(Wosize_val(ac),0,ac) call(mvaddchstr(a_int(aa),a_int(ab),t)) END
ML4d(mvwaddchstr,err,window,int,int,chtype array)
BEG4 copie(Wosize_val(ad),0,ad)
  call(mvwaddchstr(a_window(aa),a_int(ab),a_int(ac),t)) END
ML3d(addchnstr,err,chtype array,int,int)
BEG3 int i0=Int_val(ab);
  copie(Int_val(ac),i0,aa)
  call(addchnstr(t,Int_val(ac))) END
ML4d(waddchnstr,err,window,chtype array,int,int)
BEG4 int i0=Int_val(ac);
  copie(Int_val(ad),i0,ab)
  call(waddchnstr(a_window(aa),t,Int_val(ad))) END
ML5d(mvaddchnstr,err,int,int,chtype array,int,int)
BEG5 int i0=Int_val(ad);
  copie(Int_val(ae),i0,ac)
  call(mvaddchnstr(a_int(aa),a_int(ab),t,Int_val(ae))) END
ML6d(mvwaddchnstr,err,window,int,int,chtype array,int,int)
BEG6 int i0=Int_val(ae);
  copie(Int_val(af),i0,ad)
  call(mvwaddchnstr(a_window(aa),a_int(ab),a_int(ac),t,Int_val(af))) END
#undef copie
#undef call

/* addstr */

ML1(addstr,err,string)
ML2(waddstr,err,window,string)
ML3(mvaddstr,err,int,int,string)
ML4(mvwaddstr,err,window,int,int,string)
ML3d(addnstr,err,string,int,int)
BEG3 r_err(addnstr(a_string(aa)+a_int(ab),a_int(ac))); END
ML4d(waddnstr,err,window,string,int,int)
BEG4 r_err(waddnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); END
ML5d(mvaddnstr,err,int,int,string,int,int)
BEG5 r_err(mvaddnstr(a_int(aa),a_int(ab),
  a_string(ac)+a_int(ad),a_int(ae))); END
ML6d(mvwaddnstr,err,window,int,int,string,int,int)
BEG6 r_err(mvwaddnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); END
  
/* attr */

ML1(attroff,unit,int)
ML2(wattroff,unit,window,int)
ML1(attron,unit,int)
ML2(wattron,unit,window,int)
ML1(attrset,unit,int)
ML2(wattrset,unit,window,int)
ML0(standend,unit)
ML1(wstandend,unit,window)
ML0(standout,unit)
ML1(wstandout,unit,window)
#if 1
ML1d(attr_off,unit,attr_t)
BEG1 attr_off(a_attr_t(aa),NULL); CAMLreturn(Val_unit); END
ML2d(wattr_off,unit,window,attr_t)
BEG2 wattr_off(a_window(aa),a_attr_t(ab),NULL); CAMLreturn(Val_unit); END
ML1d(attr_on,unit,attr_t)
BEG1 attr_on(a_attr_t(aa),NULL); CAMLreturn(Val_unit); END
ML2d(wattr_on,unit,window,attr_t)
BEG2 wattr_on(a_window(aa),a_attr_t(ab),NULL); CAMLreturn(Val_unit); END
ML2d(attr_set,unit,attr_t,int)
BEG2 attr_set(a_attr_t(aa),a_int(ab),NULL); CAMLreturn(Val_unit); END
ML3d(wattr_set,unit,window,attr_t,int)
BEG3 wattr_set(a_window(aa),a_attr_t(ab),a_int(ac),NULL); CAMLreturn(Val_unit); END
#else
ML1d(attr_off,unit,attr_t)
BEG1 attr_off(a_attr_t(aa)); CAMLreturn(Val_unit); END
ML2d(wattr_off,unit,window,attr_t)
BEG2 wattr_off(a_window(aa),a_attr_t(ab)); CAMLreturn(Val_unit); END
ML1d(attr_on,unit,attr_t)
BEG1 attr_on(a_attr_t(aa)); CAMLreturn(Val_unit); END
ML2d(wattr_on,unit,window,attr_t)
BEG2 wattr_on(a_window(aa),a_attr_t(ab)); CAMLreturn(Val_unit); END
ML2d(attr_set,unit,attr_t,int)
BEG2 attr_set(a_attr_t(aa)); CAMLreturn(Val_unit); END
ML3d(wattr_set,unit,window,attr_t,int)
BEG3 wattr_set(a_window(aa),a_attr_t(ab)); CAMLreturn(Val_unit); END
#endif
ML3d(chgat,unit,int,attr_t,int)
BEG3 chgat(a_int(aa),a_attr_t(ab),a_int(ac),NULL); CAMLreturn(Val_unit); END
ML4d(wchgat,unit,window,int,attr_t,int)
BEG4 wchgat(a_window(aa),a_int(ab),a_attr_t(ac),a_int(ad),NULL);
  CAMLreturn(Val_unit); END
ML5d(mvchgat,unit,int,int,int,attr_t,int)
BEG5 mvchgat(a_int(aa),a_int(ab),a_int(ac),a_attr_t(ad),a_int(ae),NULL);
  CAMLreturn(Val_unit); END
ML6d(mvwchgat,unit,window,int,int,int,attr_t,int)
BEG6 mvwchgat(a_window(aa),a_int(ab),a_int(ac),a_int(ad),a_attr_t(ae),
  a_int(af),NULL); CAMLreturn(Val_unit); END

/* beep */

ML0(beep,err)
ML0(flash,err)

/* bkgd */

ML1(bkgdset,unit,chtype)
ML2(wbkgdset,unit,window,chtype)
ML1(bkgd,unit,chtype)
ML2(wbkgd,unit,window,chtype)
ML1(getbkgd,chtype,window)

/* border */

ML8(border,unit,chtype,chtype,chtype,chtype,chtype,chtype,chtype,chtype)
ML9(wborder,unit,window,chtype,chtype,chtype,chtype,chtype,chtype,chtype,chtype)
ML3(box,unit,window,chtype,chtype)
ML2(hline,unit,chtype,int)
ML3(whline,unit,window,chtype,int)
ML2(vline,unit,chtype,int)
ML3(wvline,unit,window,chtype,int)

ML4(mvhline,unit,int,int,chtype,int)
ML5(mvwhline,unit,window,int,int,chtype,int)
ML4(mvvline,unit,int,int,chtype,int)
ML5(mvwvline,unit,window,int,int,chtype,int)

/* clear */

ML0(erase,unit)
ML1(werase,unit,window)
ML0(clear,unit)
ML1(wclear,unit,window)
ML0(clrtobot,unit)
ML1(wclrtobot,unit,window)
ML0(clrtoeol,unit)
ML1(wclrtoeol,unit,window)

/* color */

ML0(start_color,err)
ML3(init_pair,err,int,int,int)
ML4(init_color,err,int,int,int,int)
ML0(has_colors,bool)
ML0(can_change_color,bool)
ML1d(color_content,int*int*int,int)
BEG1 short x,y,z; if(color_content(Int_val(aa),&x,&y,&z)==ERR) 
  x=y=z=-1; r_int_int_int(x,y,z); END
ML1d(pair_content,int*int,int)
BEG1 short x,y; if(pair_content(Int_val(aa),&y,&x)==ERR) x=y=-1;
  r_int_int(x,y); END
ML0d(colors,int) BEG0 r_int(COLORS); END
ML0d(color_pairs,int) BEG0 r_int(COLOR_PAIRS); END

/* delch */

ML0(delch,err)
ML1(wdelch,err,window)
ML2(mvdelch,err,int,int)
ML3(mvwdelch,err,window,int,int)

/* deleteln */

ML0(deleteln,err)
ML1(wdeleteln,err,window)
ML1(insdelln,err,int)
ML2(winsdelln,err,window,int)
ML0(insertln,err)
ML1(winsertln,err,window)

/* getch */

ML0(getch,int)
ML1(wgetch,int,window)
ML2(mvgetch,int,int,int)
ML3(mvwgetch,int,window,int,int)
ML1(ungetch,err,int)

/* getstr */

ML1d(getstr,err,string)
BEG1 r_err(getnstr(a_string(aa),string_length(aa))); END
ML2d(wgetstr,err,window,string)
BEG2 r_err(wgetnstr(a_window(aa),a_string(ab),string_length(ab))); END
ML3d(mvgetstr,err,int,int,string)
BEG3 r_err(mvgetnstr(a_int(aa),a_int(ab),a_string(ac),string_length(ac))); END
ML4d(mvwgetstr,err,window,int,int,string)
BEG4 r_err(mvwgetnstr(a_window(aa),a_int(ab),a_int(ac),a_string(ad),
  string_length(ad))); END
ML3d(getnstr,err,string,int,int)
BEG3 r_err(getnstr(a_string(aa)+a_int(ab),a_int(ac))); END
ML4d(wgetnstr,err,window,string,int,int)
BEG4 r_err(wgetnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); END
ML5d(mvgetnstr,err,int,int,string,int,int)
BEG5 r_err(mvgetnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),
  a_int(ae))); END
ML6d(mvwgetnstr,err,window,int,int,string,int,int)
BEG6 r_err(mvwgetnstr(a_window(aa),a_int(ab),a_int(ac),a_string(ad)+a_int(ae),
  a_int(af))); END

/* getyx */

ML1d(getyx,int*int,window)
BEG1 int x,y; getyx(a_window(aa),y,x); r_int_int(x,y); END
ML1d(getparyx,int*int,window)
BEG1 int x,y; getparyx(a_window(aa),y,x); r_int_int(x,y); END
ML1d(getbegyx,int*int,window)
BEG1 int x,y; getbegyx(a_window(aa),y,x); r_int_int(x,y); END
ML1d(getmaxyx,int*int,window)
BEG1 int x,y; getmaxyx(a_window(aa),y,x); r_int_int(x,y); END
  
/* inch */

ML0(inch,chtype)
ML1(winch,chtype,window)
ML2(mvinch,chtype,int,int)
ML3(mvwinch,chtype,window,int,int)

/* inchstr */

#define alloue(n) int i,ne=n; chtype *tbl=malloc((ne+1)*sizeof(chtype));
#define copie(t,id) \
  for(i=0;i<ne;i++) Store_field(t,i+id,Val_int(tbl[i])); \
  r_err(ret)
ML1d(inchstr,err,chtype array)
BEG1 alloue(Wosize_val(aa))
  int ret=inchnstr(tbl,ne);
  copie(aa,0); END
ML2d(winchstr,err,window,chtype array)
BEG2 alloue(Wosize_val(ab))
  int ret=winchnstr(a_window(aa),tbl,ne);
  copie(ab,0); END
ML3d(mvinchstr,err,int,int,chtype array)
BEG3 alloue(Wosize_val(ac))
  int ret=mvinchnstr(a_int(aa),a_int(ab),tbl,ne);
  copie(ac,0); END
ML4d(mvwinchstr,err,window,int,int,chtype array)
BEG4 alloue(Wosize_val(ad))
  int ret=mvwinchnstr(a_window(aa),a_int(ab),a_int(ac),tbl,ne);
  copie(ad,0); END
ML3d(inchnstr,err,chtype array,int,int)
BEG3 int i0=a_int(ab);
  alloue(Wosize_val(aa)-i0)
  int ret=inchnstr(tbl,a_int(ac));
  copie(aa,i0); END
ML4d(winchnstr,err,window,chtype array,int,int)
BEG4 int i0=a_int(ac);
  alloue(Wosize_val(ab)-i0)
  int ret=winchnstr(a_window(aa),tbl,a_int(ad));
  copie(ab,i0); END
ML5d(mvinchnstr,err,int,int,chtype array,int,int)
BEG5 int i0=a_int(ad);
  alloue(Wosize_val(ac)-i0)
  int ret=mvinchnstr(a_int(aa),a_int(ab),tbl,a_int(ae));
  copie(ac,i0); END
ML6d(mvwinchnstr,err,window,int,int,chtype array,int,int)
BEG6 int i0=a_int(ae);
  alloue(Wosize_val(ad)-i0)
  int ret=mvwinchnstr(a_window(aa),a_int(ab),a_int(ac),tbl,a_int(af));
  copie(ad,i0); END
#undef alloue
#undef copie

/* insch */

ML1(insch,err,chtype)
ML2(winsch,err,window,chtype)
ML3(mvinsch,err,int,int,chtype)
ML4(mvwinsch,err,window,int,int,chtype)

/* initscr */

ML0(initscr,window)
ML0(endwin,unit)
ML0(isendwin,bool)
ML3d(newterm,screen,string,Unix.file_descr,Unix.file_descr)
BEG3
  CAMLlocal1(r);
  int fda=dup(a_int(ab)),fdb=dup(a_int(ac));
  FILE *fa=fdopen(fda,"w"),*fb=fdopen(fdb,"r");
  SCREEN *s;
  AWB(r);
  r=alloc_tuple(3);
  Store_field(r,0,Val_int(fa));
  Store_field(r,1,Val_int(fb));
  s=newterm(a_string(aa),fa,fb);
  if(s==NULL){ fclose(fa); fclose(fb); failwith("newterm"); }
  Store_field(r,2,(value)s);
  CAMLreturn(r); END
ML1(set_term,unit,screen)
ML1d(delscreen,unit,screen)
BEG1 delscreen(a_screen(aa));
  fclose((FILE * )Field(aa,0)); fclose((FILE * )Field(aa,1));
  CAMLreturn(Val_unit); END
ML0d(stdscr,window)
BEG0 r_window(stdscr); END
ML0d(null_window,window) BEG0 r_window(NULL); END

/* insstr */

ML1d(insstr,err,string)
BEG1 r_err(insnstr(a_string(aa),string_length(aa))); END
ML2d(winsstr,err,window,string)
BEG2 r_err(winsnstr(a_window(aa),a_string(ab),string_length(ab))); END
ML3d(mvinsstr,err,int,int,string)
BEG3 r_err(mvinsnstr(a_int(aa),a_int(ab),a_string(ac),string_length(ac))); END
ML4d(mvwinsstr,err,window,int,int,string)
BEG4 r_err(mvwinsnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad),string_length(ad))); END
ML3d(insnstr,err,string,int,int)
BEG3 r_err(insnstr(a_string(aa)+a_int(ab),a_int(ac))); END
ML4d(winsnstr,err,window,string,int,int)
BEG4 r_err(winsnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); END
ML5d(mvinsnstr,err,int,int,string,int,int)
BEG5 r_err(mvinsnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),a_int(ae))); END
ML6d(mvwinsnstr,err,window,int,int,string,int,int)
BEG6 r_err(mvwinsnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); END

/* instr */

ML1d(instr,err,string)
BEG1 r_err(innstr(a_string(aa),string_length(aa))); END
ML2d(winstr,err,window,string)
BEG2 r_err(winnstr(a_window(aa),a_string(ab),string_length(ab))); END
ML3d(mvinstr,err,int,int,string)
BEG3 r_err(mvinnstr(a_int(aa),a_int(ab),a_string(ac),string_length(ac))); END
ML4d(mvwinstr,err,window,int,int,string)
BEG4 r_err(mvwinnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad),string_length(ad))); END
ML3d(innstr,err,string,int,int)
BEG3 r_err(innstr(a_string(aa)+a_int(ab),a_int(ac))); END
ML4d(winnstr,err,window,string,int,int)
BEG4 r_err(winnstr(a_window(aa),a_string(ab)+a_int(ac),a_int(ad))); END
ML5d(mvinnstr,err,int,int,string,int,int)
BEG5 r_err(mvinnstr(a_int(aa),a_int(ab),a_string(ac)+a_int(ad),a_int(ae))); END
ML6d(mvwinnstr,err,window,int,int,string,int,int)
BEG6 r_err(mvwinnstr(a_window(aa),a_int(ab),a_int(ac),
  a_string(ad)+a_int(ae),a_int(af))); END

/* inopts */

ML0(cbreak,err)
ML0(nocbreak,err)
ML0(echo,err)
ML0(noecho,err)
ML1(halfdelay,err,int)
ML2(intrflush,err,window,bool)
ML2(keypad,err,window,bool)
ML2(meta,err,window,bool)
ML2(nodelay,err,window,bool)
ML0(raw,err)
ML0(noraw,err)
ML0(noqiflush,unit)
ML0(qiflush,unit)
ML2(notimeout,err,window,bool)
ML1(timeout,unit,int)
ML2(wtimeout,unit,window,int)
ML1d(typeahead,err,Unix.file_descr)
BEG1 r_err(typeahead(a_int(aa))); END
ML0d(notypeahead,err)
BEG0 r_err(typeahead(-1)); END

/* kernel */

ML0(def_prog_mode,unit)
ML0(def_shell_mode,unit)
ML0(reset_prog_mode,unit)
ML0(reset_shell_mode,unit)
ML0(resetty,unit)
ML0(savetty,unit)
ML0d(getsyx,int*int)
BEG0 int x,y; getsyx(y,x); r_int_int(x,y); END
ML2(setsyx,unit,int,int)
ML1(curs_set,err,int)
ML1(napms,unit,int)

ML1d(ripoffline,unit,bool)
BEG1 ripoffline(Bool_val(aa)?1:-1,ripoff_callback); CAMLreturn(Val_unit); END
ML0d(get_ripoff,window*int)
BEG0 if(ripoff_niv==0) failwith("get_ripoff"); ripoff_niv--;
  r_int_int(ripoff_w[ripoff_niv],ripoff_l[ripoff_niv]); END
  

/* mouse */

/*getmouse*/
/*ungetmouse*/
ML1d(mousemask,int*int,int)
BEG1 mmask_t r=1234,n=Int_val(aa); n=mousemask(n,&r); r_int_int(n,r); END
/* La souris ne marche pas avec les xterms que j'ai ici, donc je suis un
 * peu bloqué pour le testing. De toutes façons, ce n'est pas standard. */

/* move */

ML2(move,err,int,int)
ML3(wmove,err,window,int,int)

/* outopts */

ML2(clearok,unit,window,bool)
ML2(idlok,unit,window,bool)
ML2(idcok,unit,window,bool)
ML2(immedok,unit,window,bool)
ML2(leaveok,unit,window,bool)
ML2(setscrreg,err,int,int)
ML3(wsetscrreg,err,window,int,int)
ML2(scrollok,unit,window,bool)
ML0(nl,unit)
ML0(nonl,unit)

/* overlay */

ML2(overlay,err,window,window)
ML2(overwrite,err,window,window)
ML9(copywin,err,window,window,int,int,int,int,int,int,bool)

/* pad */

ML2(newpad,window,int,int)
ML5(subpad,window,window,int,int,int,int)
ML7(prefresh,err,window,int,int,int,int,int,int)
ML7(pnoutrefresh,err,window,int,int,int,int,int,int)
ML2(pechochar,err,window,chtype)

/* refresh */

ML0(refresh,err)
ML1(wrefresh,err,window)
ML1(wnoutrefresh,err,window)
ML0(doupdate,err)
ML1(redrawwin,err,window)
ML3(wredrawln,err,window,int,int)

/* resize */

ML3(wresize,err,window,int,int)
ML2(resizeterm,err,int,int)

/* scr_dump */

ML1(scr_dump,err,string)
ML1(scr_restore,err,string)
ML1(scr_init,err,string)
ML1(scr_set,err,string)

/* scroll */

ML1(scroll,err,window)
ML1(scrl,err,int)
ML2(wscrl,err,window,int)

/* slk */

ML1(slk_init,err,int)
ML3(slk_set,err,int,string,int)
ML0(slk_refresh,err)
ML0(slk_noutrefresh,err)
ML1(slk_label,string,int)
ML0(slk_clear,err)
ML0(slk_restore,err)
ML0(slk_touch,err)
ML1(slk_attron,err,attr_t)
ML1(slk_attroff,err,attr_t)
ML1(slk_attrset,err,attr_t)

/* termattrs */

ML0(baudrate,int)
ML0(erasechar,char)
ML0(has_ic,bool)
ML0(has_il,bool)
ML0(killchar,char)
ML0(longname,string)
ML0(termattrs,attr_t)
ML0(termname,string)

/* termcap */

ML1d(tgetent,bool,string)
BEG1 CAMLreturn(Val_bool(tgetent(NULL,String_val(aa))==1)); END
ML1(tgetflag,bool,string)
ML1(tgetnum,int,string)
ML1d(tgetstr,bool,string)
BEG1 r_string(tgetstr(String_val(aa),NULL)); END
ML3(tgoto,string,string,int,int)

/* terminfo */

ML2d(setupterm,err,string,Unix.file_descr)
BEG2 int r; r_err(setupterm(a_string(aa),a_int(ab),&r)); END
ML1(setterm,err,string)
ML0d(cur_term,terminal)
BEG0 r_terminal(cur_term); END
ML1(set_curterm,terminal,terminal)
ML1(del_curterm,err,terminal)
ML2d(restartterm,err,string,Unix.file_descr)
BEG2 int r; r_err(restartterm(a_string(aa),a_int(ab),&r)); END
ML1(putp,err,string)
ML1(vidattr,err,chtype)
ML4(mvcur,err,int,int,int,int)
ML1d(tigetflag,bool,string)
BEG1 r_bool(tigetflag(a_string(aa))>0); END
ML1(tigetnum,int,string)
ML1d(tigetstr,string,string)
BEG1 char *s=tigetstr(a_string(aa));
  if((s==NULL)||(s==(char * )-1)) failwith("tigetstr");
  CAMLreturn(copy_string(s)); END
ML3d(tputs,err,string,int,(char->unit))
BEG3 putc_function=ac;
  r_err(tputs(a_string(aa),a_int(ab),putc_callback)); END
ML2d(vidputs,err,chtype,(char->unit))
BEG2 putc_function=ab;
  r_err(vidputs(a_chtype(aa),putc_callback)); END
ML2d(tparm,string,string,int array)
BEG2 int t[10],i,n=Wosize_val(ab);
  if(n>10) n=10;
  for(i=0;i<n;i++) t[i]=a_int(Field(ab,i));
  r_string(tparm(a_string(aa),t[0],t[1],t[2],t[3],t[4],
    t[5],t[6],t[7],t[8],t[9])); END
#define arrayret(nt) \
  CAMLlocal1(s); \
  int n=a_int(aa); AWB(s) \
  s=alloc_tuple(3); \
  Store_field(s,0,Val_unit); \
  Store_field(s,1,Val_unit); \
  Store_field(s,2,Val_unit); \
  if((nt##names[n]==NULL)||(nt##codes[n]==NULL)||(nt##fnames[n]==NULL)){ \
    CAMLlocal1(ns); AWB(ns) \
    ns=copy_string(""); \
    Store_field(s,0,ns); \
    Store_field(s,1,ns); \
    Store_field(s,2,ns); \
  }else{ \
    Store_field(s,0,copy_string(nt##names[n])); \
    Store_field(s,1,copy_string(nt##codes[n])); \
    Store_field(s,2,copy_string(nt##fnames[n])); \
  } \
  CAMLreturn(s);
ML1d(bool_terminfo_variable,string*string*string,int) BEG1 arrayret(bool) END
ML1d(num_terminfo_variable,string*string*string,int) BEG1 arrayret(num) END
ML1d(str_terminfo_variable,string*string*string,int) BEG1 arrayret(str) END

/* touch */

ML1(touchwin,err,window)
ML3(touchline,err,window,int,int)
ML1(untouchwin,err,window)
ML4(wtouchln,err,window,int,int,bool)
ML2(is_linetouched,int,window,int)
ML1(is_wintouched,bool,window)

/* util */

ML1(unctrl,string,chtype)
ML1(keyname,string,int)
ML0(filter,unit)
ML1(use_env,unit,bool)
ML2d(putwin,err,window,Unix.file_descr)
BEG2 int fd=dup(a_int(ab)); FILE *f=fdopen(fd,"w");
  int r=putwin(a_window(aa),f); fclose(f); r_err(r); END
ML1d(getwin,window,Unix.file_descr)
BEG1 int fd=dup(a_int(aa)); FILE *f=fdopen(fd,"r");
  int r=putwin(a_window(aa),f); fclose(f); r_err(r); END
ML1(delay_output,err,int)
ML0(flushinp,unit)

/* window */

ML4(newwin,window,int,int,int,int)
ML1(delwin,err,window)
ML3(mvwin,err,window,int,int)
ML5(subwin,window,window,int,int,int,int)
ML5(derwin,window,window,int,int,int,int)
ML3(mvderwin,err,window,int,int)
ML1(dupwin,window,window)
ML1(wsyncup,unit,window)
ML2(syncok,err,window,bool)
ML1(wcursyncup,unit,window)
ML1(wsyncdown,unit,window)

/* Fonctions auxiliaires */

#define ca(i,n) Store_field(tr,i,Val_int(ACS_##n));
ML0d(get_acs_codes,Acs.acs)
BEG0 CAMLlocal1(tr); AWB(tr)
  tr=alloc_tuple(32);
ca( 0,ULCORNER)	ca( 1,LLCORNER)	ca( 2,URCORNER)	ca( 3,LRCORNER)
ca( 4,LTEE)	ca( 5,RTEE)	ca( 6,BTEE)	ca( 7,TTEE)
ca( 8,HLINE)	ca( 9,VLINE)	ca(10,PLUS)	ca(11,S1)
ca(12,S9)	ca(13,DIAMOND)	ca(14,CKBOARD)	ca(15,DEGREE)
ca(16,PLMINUS)	ca(17,BULLET)	ca(18,LARROW)	ca(19,RARROW)
ca(20,DARROW)	ca(21,UARROW)	ca(22,BOARD)	ca(23,LANTERN)
ca(24,BLOCK)	ca(25,S3)	ca(26,S7)	ca(27,LEQUAL)
ca(28,GEQUAL)	ca(29,PI)	ca(30,NEQUAL)	ca(31,STERLING)
  CAMLreturn(tr);
END
#undef ca

/* Du travail pour les esclaves de M$ */
ML0d(winch_handler_on,unit)
BEG0 signal(SIGWINCH,winch_handler); CAMLreturn(Val_unit); END
ML0d(winch_handler_off,unit)
BEG0 signal(SIGWINCH,SIG_IGN); CAMLreturn(Val_unit); END

ML0d(get_size,int*int)
BEG0 struct winsize ws;
  ioctl(0,TIOCGWINSZ,&ws);
  r_int_int(ws.ws_row,ws.ws_col);
END
ML1d(get_size_fd,int*int,Unix.file_descr)
BEG1 struct winsize ws;
  ioctl(a_int(aa),TIOCGWINSZ,&ws);
  r_int_int(ws.ws_row,ws.ws_col);
END

