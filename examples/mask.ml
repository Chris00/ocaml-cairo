(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 120 120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  let linpat = Cairo.Pattern.create_linear 0. 0. 1. 1. in
  Cairo.Pattern.add_color_stop_rgb linpat 0. 0.3 0.8;
  Cairo.Pattern.add_color_stop_rgb linpat ~ofs:1. 0. 0.8 0.3;

  let radpat = Cairo.Pattern.create_radial 0.5 0.5 0.25 0.5 0.5 0.75 in
  Cairo.Pattern.add_color_stop_rgba radpat 0. 0. 0. 1.;
  Cairo.Pattern.add_color_stop_rgba radpat ~ofs:0.5 0. 0. 0. 0.;

  Cairo.set_source cr linpat;
  Cairo.mask cr radpat;

  (* Write output *)
  Cairo.PNG.write surface "mask.png"
