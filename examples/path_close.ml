(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  Cairo.set_line_width cr 0.05;
  Cairo.set_source_rgb cr 0. 0. 0.;

  Cairo.move_to cr 0.25 0.25;
  Cairo.line_to cr 0.5  0.375;
  Cairo.rel_line_to cr 0.25 (-0.125);
  let pi_4 = atan 1. in
  Cairo.arc cr 0.5 0.5 ~r:(0.25 *. sqrt 2.) ~a1:(-. pi_4) ~a2:pi_4;
  Cairo.rel_curve_to cr (-0.25) (-0.125) (-0.25) 0.125 (-0.5) 0.;
  Cairo.Path.close cr;

  Cairo.stroke cr;

  (* Write output *)
  Cairo.PNG.write surface "path_close.png"
