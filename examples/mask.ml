(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  let linpat = Cairo.Pattern.create_linear ~x0:0. ~y0:0. ~x1:1. ~y1:1. in
  Cairo.Pattern.add_color_stop_rgb linpat 0. 0.3 0.8;
  Cairo.Pattern.add_color_stop_rgb linpat ~ofs:1. 0. 0.8 0.3;

  let radpat = Cairo.Pattern.create_radial ~x0:0.5 ~y0:0.5 ~r0:0.25
                                           ~x1:0.5 ~y1:0.5 ~r1:0.75 in
  Cairo.Pattern.add_color_stop_rgba radpat 0. 0. 0. 1.;
  Cairo.Pattern.add_color_stop_rgba radpat ~ofs:0.5 0. 0. 0. 0.;

  Cairo.set_source cr linpat;
  Cairo.mask cr radpat;

  (* Write output *)
  Cairo.PNG.write surface "mask.png"
