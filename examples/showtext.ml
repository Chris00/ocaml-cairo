(* This file is part of the tutorial
   http://archimedes.forge.ocamlcore.org/cairo/
*)

open Cairo

let () =
  let surface = Image.create Image.ARGB32 120 120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  scale cr 120. 120.;

  (* Drawing code goes here *)
  set_source_rgb cr 0.0 0.0 0.0;
  select_font_face cr "Georgia" ~weight:Bold;
  set_font_size cr 1.2;
  let te = text_extents cr "a" in
  move_to cr (0.5 -. te.width /. 2. -. te.x_bearing)
    (0.5 -. te.height /. 2. -. te.y_bearing);
  show_text cr "a";

  (* Write output and clean up *)
  PNG.write surface "showtext.png"
