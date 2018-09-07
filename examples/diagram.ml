(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

open Cairo

let diagram_draw_source cr =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr

let diagram_draw_mask cr =
  Cairo.set_source_rgb cr 1. 0.9 0.6;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr

let diagram_draw_mask_pattern cr pat =
  Cairo.set_source_rgb cr 1. 0.9 0.6;
  Cairo.mask cr pat

let diagram_draw_dest cr =
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr

let stroke_draw_mask cr =
  Cairo.Group.push cr;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.rectangle cr 0.20 0.20 ~w:0.6 ~h:0.6;
  Cairo.rectangle cr 0.30 0.30 ~w:0.4 ~h:0.4;
  Cairo.set_fill_rule cr EVEN_ODD;
  Cairo.fill cr;
  Cairo.set_fill_rule cr WINDING;

  diagram_draw_mask_pattern cr (Cairo.Group.pop cr);

  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.set_source_rgb cr 0. 0.6 0.;

  let px, py = Cairo.device_to_user_distance cr 1. 1. in
  Cairo.set_line_width cr (max px py);
  Cairo.stroke cr

let stroke_draw_dest cr =
  diagram_draw_dest cr;
  Cairo.set_line_width cr 0.1;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.stroke cr

let fill_draw_mask cr =
  Cairo.Group.push cr;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.set_fill_rule cr EVEN_ODD;
  Cairo.fill cr;
  Cairo.set_fill_rule cr WINDING;

  diagram_draw_mask_pattern cr (Cairo.Group.pop cr);

  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.set_source_rgb cr 0. 0.6 0.;
  let px, py = Cairo.device_to_user_distance cr 1. 1. in
  Cairo.set_line_width cr (max px py);
  Cairo.stroke cr

let fill_draw_dest cr =
  diagram_draw_dest cr;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.fill cr

let showtext_draw_mask cr =
  (* yellow mask color *)
  Cairo.set_source_rgb cr 1. 0.9 0.6;

  (* rectangle with an "a"-shaped hole *)
  Cairo.select_font_face cr "Georgia" ~weight:Bold;
  Cairo.set_font_size cr 1.2;
  let te = Cairo.text_extents cr "a" in
  Cairo.Group.push cr;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.move_to cr (0.5 -. te.width /. 2. -. te.x_bearing)
    (0.5 -. te.height /. 2. -. te.y_bearing);
  Cairo.Path.text cr "a";
  Cairo.set_fill_rule cr EVEN_ODD;
  Cairo.fill cr;
  Cairo.set_fill_rule cr WINDING;
  Cairo.Group.pop_to_source cr;
  Cairo.paint cr;

  (* show the outline of the glyph with a green line *)
  Cairo.move_to cr (0.5 -. te.width /. 2. -. te.x_bearing)
    (0.5 -. te.height /. 2. -. te.y_bearing);
  Cairo.set_source_rgb cr 0. 0.6 0.;

  let ux, uy = Cairo.device_to_user_distance cr 1. 1. in
  Cairo.set_line_width cr (max ux uy);
  Cairo.Path.text cr "a";
  Cairo.stroke cr

let showtext_draw_dest cr =
  (* white background *)
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr;

  (* black letter "a" *)
  Cairo.set_source_rgb cr 0.0 0.0 0.0;
  Cairo.select_font_face cr "Georgia" ~weight:Bold;
  Cairo.set_font_size cr 1.2;
  let te = Cairo.text_extents cr "a" in
  Cairo.move_to cr (0.5 -. te.width /. 2. -. te.x_bearing)
    (0.5 -. te.height /. 2. -. te.y_bearing);
  Cairo.show_text cr "a"

let paint_draw_source cr =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr

let paint_draw_dest cr =
  diagram_draw_dest cr;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.paint cr ~alpha:0.5

let mask_draw_source cr =
  let linpat = Cairo.Pattern.create_linear ~x0:0. ~y0:0. ~x1:1. ~y1:1. in
  Cairo.Pattern.add_color_stop_rgb linpat 0. 0.3 0.8;
  Cairo.Pattern.add_color_stop_rgb linpat 0. 0.8 0.3 ~ofs:1.;
  Cairo.set_source cr linpat;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.fill cr

let mask_draw_mask cr =
  let radialinv = Cairo.Pattern.create_radial ~x0:0.5 ~y0:0.5 ~r0:0.25
                                              ~x1:0.5 ~y1:0.5 ~r1:0.75 in
  Cairo.Pattern.add_color_stop_rgba radialinv 0. 0. 0. 0.;
  Cairo.Pattern.add_color_stop_rgba radialinv ~ofs:0.5 0. 0. 0. 1.;
  Cairo.save cr;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.clip cr;
  diagram_draw_mask_pattern cr radialinv;
  Cairo.restore cr

let mask_draw_dest cr =
  let linpat = Cairo.Pattern.create_linear ~x0:0. ~y0:0. ~x1:1. ~y1:1. in
  Cairo.Pattern.add_color_stop_rgb linpat 0. 0.3 0.8;
  Cairo.Pattern.add_color_stop_rgb linpat ~ofs:1. 0. 0.8 0.3;

  let radpat = Cairo.Pattern.create_radial ~x0:0.5 ~y0:0.5 ~r0:0.25
                                           ~x1:0.5 ~y1:0.5 ~r1:0.75 in
  Cairo.Pattern.add_color_stop_rgba radpat 0. 0. 0. 1.;
  Cairo.Pattern.add_color_stop_rgba radpat ~ofs:0.5  0. 0. 0. 0.;

  diagram_draw_dest cr;
  Cairo.save cr;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.clip cr;
  Cairo.set_source cr linpat;
  Cairo.mask cr radpat;
  Cairo.restore cr




let draw_dest name cr =
  if name = "stroke" then stroke_draw_dest cr
  else if name = "fill" then fill_draw_dest cr
  else if name = "showtext" then showtext_draw_dest cr
  else if name = "paint" then paint_draw_dest cr
  else if name = "mask" then mask_draw_dest cr
  else diagram_draw_dest cr

let draw_mask name cr =
  if name = "stroke" then stroke_draw_mask cr
  else if name = "fill" then fill_draw_mask cr
  else if name = "showtext" then showtext_draw_mask cr
  else if name = "paint" then ()
  else if name = "mask" then mask_draw_mask cr
  else diagram_draw_mask cr

let draw_source name cr =
  if name = "paint" then paint_draw_source cr
  else if name = "mask" then mask_draw_source cr
  else diagram_draw_source cr


let diagram fname alpha0 alpha1 alpha2 =
  let width=160. and height=120. in
  let svg_filename = fname ^ ".svg"
  and png_filename = fname ^ ".png" in
  let surf = Cairo.SVG.create svg_filename ~w:width ~h:height in
  let cr = Cairo.create surf in

  (*
   * show layers separately on the right
   *)
  let layer draw =
    Cairo.save cr;
    Cairo.Group.push cr;
    Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
    Cairo.clip cr;
    draw fname cr;
    Cairo.Group.pop_to_source cr;
    Cairo.paint cr;
    Cairo.restore cr;
  in
  Cairo.save cr;
  Cairo.scale cr (height /. 3.) (height /. 3.);
  (* source *)
  Cairo.translate cr (3. *. width /. height -. 1.) 0.;
  layer draw_source;
  (* mask *)
  Cairo.translate cr 0. 1.;
  layer draw_mask;
  (* destination *)
  Cairo.translate cr 0. 1.;
  layer draw_dest;
  Cairo.restore cr;

  (* draw a border around the layers *)
  Cairo.save cr;
  Cairo.scale cr (height /. 3.) (height /. 3.);
  Cairo.translate cr (3. *. width /. height -. 1.) 0.;
  let ux, uy = Cairo.device_to_user_distance cr 2. 2. in
  Cairo.set_line_width cr (max ux uy);
  Cairo.rectangle cr 0. 0. ~w:1. ~h:3.;
  Cairo.clip_preserve cr;
  Cairo.stroke cr;
  Cairo.rectangle cr 0. 1. ~w:1. ~h:1.;
  Cairo.stroke cr;
  Cairo.restore cr;

  (*
   * layer diagram on the left
   *)
  let left_layers ~tx ~ty alpha draw =
    Cairo.save cr;
    Cairo.scale cr (width -. height /. 3.) height;
    Cairo.transform cr { xx=0.6; yx=0.; xy=1./.3.; yy=0.5; x0=tx; y0=ty };
    Cairo.Group.push cr;
    Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
    Cairo.clip cr;
    draw fname cr;
    Cairo.Group.pop_to_source cr;
    Cairo.paint cr ~alpha;
    Cairo.restore cr;
  in
  (* destination layer *)
  left_layers alpha0 ~tx:0.02 ~ty:0.45 begin fun fname cr ->
    draw_dest fname cr;
    (* this layer gets a black border *)
    Cairo.set_source_rgb cr 0. 0. 0.;
    let ux, uy = Cairo.device_to_user_distance cr 2. 2. in
    Cairo.set_line_width cr (max ux uy);
    Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
    Cairo.stroke cr
  end;
  (* mask layer *)
  left_layers alpha1 draw_mask ~tx:0.04 ~ty:0.25;
  (* source layer *)
  left_layers alpha2 draw_source ~tx:0.06 ~ty:0.05;

  (* write output *)
  Cairo.PNG.write surf png_filename;
  Cairo.Surface.finish surf


let () =
  diagram "destination" 1.0 0.15 0.15;
  diagram "the-mask" 0.15 1.0 0.15;
  diagram "source" 0.15 0.15 1.0;

  diagram "stroke" 1.0 0.8 0.4;
  diagram "fill" 1.0 0.8 0.4;
  diagram "showtext" 1.0 0.8 0.4;
  diagram "paint" 1.0 0.8 0.4;
  diagram "mask" 1.0 0.8 0.4
