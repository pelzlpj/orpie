type window
type screen
type terminal
type chtype = int
type attr_t = int
type err = bool

#define quote(a) #a

#define ML0(f,tr) \
  external f : unit -> tr = quote(mlcurses_##f)
#define ML1(f,tr,ta) \
  external f : ta -> tr = quote(mlcurses_##f)
#define ML2(f,tr,ta,tb) \
  external f : ta -> tb -> tr = quote(mlcurses_##f)
#define ML3(f,tr,ta,tb,tc) \
  external f : ta -> tb -> tc -> tr = quote(mlcurses_##f)
#define ML4(f,tr,ta,tb,tc,td) \
  external f : ta -> tb -> tc -> td -> tr = quote(mlcurses_##f)
#define ML5(f,tr,ta,tb,tc,td,te) \
  external f : ta -> tb -> tc -> td -> te -> tr = quote(mlcurses_##f)
#define ML6(f,tr,ta,tb,tc,td,te,tf) \
  external f : ta -> tb -> tc -> td -> te -> tf -> tr \
  = quote(mlcurses_##f##_bytecode) quote(mlcurses_##f##_native)
#define ML7(f,tr,ta,tb,tc,td,te,tf,tg) \
  external f : ta -> tb -> tc -> td -> te -> tf -> tg -> tr \
  = quote(mlcurses_##f##_bytecode) quote(mlcurses_##f##_native)
#define ML8(f,tr,ta,tb,tc,td,te,tf,tg,th) \
  external f : ta -> tb -> tc -> td ->  te -> tf -> tg -> th -> tr \
  = quote(mlcurses_##f##_bytecode) quote(mlcurses_##f##_native)
#define ML9(f,tr,ta,tb,tc,td,te,tf,tg,th,ti) \
  external f : ta -> tb -> tc -> td -> te -> tf -> tg -> th -> ti -> tr \
  = quote(mlcurses_##f##_bytecode) quote(mlcurses_##f##_native)

#define ML0d(f,tr) ML0(f,tr)
#define ML1d(f,tr,ta) ML1(f,tr,ta)
#define ML2d(f,tr,ta,tb) ML2(f,tr,ta,tb)
#define ML3d(f,tr,ta,tb,tc) ML3(f,tr,ta,tb,tc)
#define ML4d(f,tr,ta,tb,tc,td) ML4(f,tr,ta,tb,tc,td)
#define ML5d(f,tr,ta,tb,tc,td,te) ML5(f,tr,ta,tb,tc,td,te)
#define ML6d(f,tr,ta,tb,tc,td,te,tf) ML6(f,tr,ta,tb,tc,td,te,tf)

#define BEG (*
#define BEG0 BEG
#define BEG1 BEG
#define BEG2 BEG
#define BEG3 BEG
#define BEG4 BEG
#define BEG5 BEG
#define BEG6 BEG
#define BEG7 BEG
#define BEG8 BEG
#define BEG9 BEG
#define END *)

module Acs = struct
  type acs = {
    ulcorner:	chtype;
    llcorner:	chtype;
    urcorner:	chtype;
    lrcorner:	chtype;
    ltee:	chtype;
    rtee:	chtype;
    btee:	chtype;
    ttee:	chtype;
    hline:	chtype;
    vline:	chtype;
    plus:	chtype;
    s1:		chtype;
    s9:		chtype;
    diamond:	chtype;
    ckboard:	chtype;
    degree:	chtype;
    plminus:	chtype;
    bullet:	chtype;
    larrow:	chtype;
    rarrow:	chtype;
    darrow:	chtype;
    uarrow:	chtype;
    board:	chtype;
    lantern:	chtype;
    block:	chtype;
    s3:		chtype;
    s7:		chtype;
    lequal:	chtype;
    gequal:	chtype;
    pi:		chtype;
    nequal:	chtype;
    sterling:	chtype
  }
  let bssb a = a.ulcorner
  let ssbb a = a.llcorner
  let bbss a = a.urcorner
  let sbbs a = a.lrcorner
  let sbss a = a.rtee
  let sssb a = a.ltee
  let ssbs a = a.btee
  let bsss a = a.ttee
  let bsbs a = a.hline
  let sbsb a = a.vline
  let ssss a = a.plus
end


#include "functions.c"

let null_window = null_window ()

let bool_terminfo_variables = Hashtbl.create 67
let num_terminfo_variables = Hashtbl.create 67
let str_terminfo_variables = Hashtbl.create 601

let () =
  let rec ins f h n =
    let (a, b, c) = f n in
    if a = "" then ()
    else (
      Hashtbl.add h c (a, b);
      ins f h (n + 1)
    ) in
  ins bool_terminfo_variable bool_terminfo_variables 0;
  ins num_terminfo_variable num_terminfo_variables 0;
  ins str_terminfo_variable str_terminfo_variables 0
  
/*
(* Bon, je vais recopier les constantes directement, parceque je n'ai
 * aucune idée de comment générer ça automatiquement proprement. Si ça ne
 * marche pas chez vous, il vous suffit de regarder l'include, et de
 * corriger à la main. Faites-le moi savoir, à tout hasard... *)
*/

module A = struct
  let normal = 0
  let attributes = 0x7FFFFF00 
  let chartext   = 0x000000FF
  let color      = 0x0000FF00
  let standout   = 0x00010000
  let underline  = 0x00020000
  let reverse    = 0x00040000
  let blink      = 0x00080000
  let dim        = 0x00100000
  let bold       = 0x00200000
  let altcharset = 0x00400000
  let invis      = 0x00800000
  let protect    = 0x01000000
  let horizontal = 0x02000000
  let left       = 0x04000000
  let low        = 0x08000000
  let right      = 0x10000000
  let top        = 0x20000000
  let vertical   = 0x40000000
  let combine = List.fold_left (lor) 0
  let color_pair n = (n lsl 8) land color
  let pair_number a = (a land color) lsr 8
end

(*/* Je sais, c'est moche, mais ça marche */*)
module WA = A

module Color = struct
  let black	= 0
  let red	= 1
  let green	= 2
  let yellow	= 3
  let blue	= 4
  let magenta	= 5
  let cyan	= 6
  let white	= 7
end

module Key = struct
#include "keys.ml"
  let f n = f0 + n
end
