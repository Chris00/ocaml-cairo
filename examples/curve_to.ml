(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let x  = 25.6  and y  = 128.0
let x1 = 102.4 and y1 = 230.4
let x2 = 153.6 and y2 = 25.6
let x3 = 230.4 and y3 = 128.0

let () =
  let cr = Cairo.create(Cairo.PDF.create "curve_to.pdf" ~w:400. ~h:300.) in

  move_to cr x y;
  curve_to cr x1 y1 x2 y2 x3 y3;

  set_line_width cr 10.0;
  stroke cr;

  set_source_rgba cr 1. 0.2 0.2 0.6;
  set_line_width cr 6.0;
  move_to cr x y;   line_to cr x1 y1;
  move_to cr x2 y2; line_to cr x3 y3;
  stroke cr;

  Surface.finish(get_target cr)
