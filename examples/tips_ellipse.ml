(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let two_pi = 8. *. atan 1.

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  Cairo.set_line_width cr 0.1;

  Cairo.save cr;
  Cairo.scale cr 0.5 1.;
  Cairo.arc cr 0.5 0.5 ~r:0.40 ~a1:0. ~a2:two_pi;
  Cairo.stroke cr;

  Cairo.translate cr 1. 0.;
  Cairo.arc cr 0.5 0.5 ~r:0.40 ~a1:0. ~a2:two_pi;
  Cairo.restore cr;
  Cairo.stroke cr;

  (* Write output *)
  Cairo.PNG.write surface "tips_ellipse.png"
