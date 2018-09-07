(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.move_to cr 0. 0.;
  Cairo.line_to cr 1. 1.;
  Cairo.move_to cr 1. 0.;
  Cairo.line_to cr 0. 1.;
  Cairo.set_line_width cr 0.2;
  Cairo.stroke cr;

  Cairo.rectangle cr 0. 0. ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 1. 0. 0. 0.80;
  Cairo.fill cr;

  Cairo.rectangle cr 0. 0.5 ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 0. 1. 0. 0.60;
  Cairo.fill cr;

  Cairo.rectangle cr 0.5 0. ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 0. 0. 1. 0.40;
  Cairo.fill cr;

  (* Write output *)
  Cairo.PNG.write surface "setsourcergba.png"
