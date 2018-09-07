(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let () =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  let radpat = Cairo.Pattern.create_radial ~x0:0.25 ~y0:0.25 ~r0:0.1
                                           ~x1:0.5  ~y1:0.5  ~r1:0.5 in
  Cairo.Pattern.add_color_stop_rgb radpat  1.0 0.8 0.8;
  Cairo.Pattern.add_color_stop_rgb radpat ~ofs:1.  0.9 0.0 0.0;

  for i=1 to 9 do
    for j=1 to 9 do
      Cairo.rectangle cr (float i /. 10. -. 0.04)
                         (float j /. 10. -. 0.04) ~w:0.08 ~h:0.08;
    done
  done;
  Cairo.set_source cr radpat;
  Cairo.fill cr;

  let linpat = Cairo.Pattern.create_linear ~x0:0.25 ~y0:0.35
                                           ~x1:0.75 ~y1:0.65 in
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.00  1. 1. 1. 0.0;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.25  0. 1. 0. 0.5;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.50  1. 1. 1. 0.0;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.75  0. 0. 1. 0.5;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:1.00  1. 1. 1. 0.0;

  Cairo.rectangle cr 0.0 0.0 ~w:1. ~h:1.;
  Cairo.set_source cr linpat;
  Cairo.fill cr;

  (* Write output *)
  Cairo.PNG.write surface "setsourcegradient.png"
