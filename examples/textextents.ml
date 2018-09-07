(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

open Cairo

let () =
  let text = "joy" in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:600 ~h:600 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 600. 600.;
  Cairo.set_font_size cr 0.5;

  (* Drawing code goes here *)
  Cairo.set_source_rgb cr 0.0 0.0 0.0;
  Cairo.select_font_face cr "Georgia" ~weight:Bold;

  let ux, uy = Cairo.device_to_user_distance cr 1. 1. in
  let px = max ux uy in
  let fe = Cairo.font_extents cr in
  let te = Cairo.text_extents cr text in
  (* The position of the text will be (x, y) *)
  let x = 0.5 -. te.x_bearing -. te.width /. 2.
  and y = 0.5 -. fe.descent +. fe.baseline /. 2. in

  (* baseline, descent, ascent, height (in dashed green) *)
  Cairo.set_line_width cr (4. *. px);
  Cairo.set_dash cr [| 9. *. px |];
  Cairo.set_source_rgba cr 0. 0.6 0. 0.5;
  let horizontal_line y =
    Cairo.move_to cr (x +. te.x_bearing) y;
    Cairo.rel_line_to cr te.width 0. in
  horizontal_line y;
  horizontal_line (y +. fe.descent);
  horizontal_line (y -. fe.ascent);
  horizontal_line (y -. fe.baseline);
  Cairo.stroke cr;

  (* extents: width & height (in dashed blue) *)
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  Cairo.set_line_width cr px;
  Cairo.set_dash cr [| 3. *. px |];
  Cairo.rectangle cr (x +. te.x_bearing) (y +. te.y_bearing)
    ~w:te.width ~h:te.height;
  Cairo.stroke cr;

  (* text *)
  Cairo.move_to cr x y;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.show_text cr text;

  (* bearing (solid blue line) *)
  Cairo.set_dash cr [| |];
  Cairo.set_line_width cr (2. *. px);
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  Cairo.move_to cr x y;
  Cairo.rel_line_to cr te.x_bearing te.y_bearing;
  Cairo.stroke cr;

  (* text's advance (blue dot) *)
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  let two_pi = 8. *. atan 1. in
  Cairo.arc cr (x +. te.x_advance) (y +. te.y_advance) ~r:(6. *. px)
    ~a1:0. ~a2:two_pi;
  Cairo.fill cr;

  (* reference point (x,y) (red dot) *)
  Cairo.arc cr x y ~r:(6. *. px) ~a1:0. ~a2:two_pi;
  Cairo.set_source_rgba cr 0.75 0. 0. 0.5;
  Cairo.fill cr;

  (* Write output *)
  Cairo.PNG.write surface "textextents.png"
