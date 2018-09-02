(*						  -*- coding:utf-8 -*-  *)
(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr =
    Cairo.create(Cairo.PDF.create "text_align_center.pdf" ~w:400. ~h:300.) in

  (* Take the cont from the command line if given: *)
  let font = try Sys.argv.(1) with _ -> "Sans" in
  let utf8 = "cairo" in

  select_font_face cr font;
  set_font_size cr 52.;

  let x0 = 128. and y0 = 128. in
  let e = text_extents cr utf8 in
  let x = x0 -. (e.width /. 2. +. e.x_bearing) in
  let y = y0 -. (e.height /. 2. +. e.y_bearing) in

  move_to cr x y;  show_text cr utf8;

  (* Funny UTF8 symbols *)
  move_to cr 0. (2. *. y0);
  show_text cr "€ ∑ ¬ ∫ ≤ ≥ ∞";

  (* draw helping lines *)
  set_source_rgba cr 1. 0.2 0.2 0.6;
  set_line_width cr 6.0;
  arc cr x y ~r:10.0 ~a1:0. ~a2:two_pi;
  fill cr;
  arc cr 0. (2. *. y0) ~r:10.0 ~a1:0. ~a2:two_pi;
  fill cr;
  move_to cr x0 0.;  line_to cr x0 (2. *. y0);
  move_to cr 0. y0;  line_to cr (2. *. x0) y0;
  stroke cr;


  Surface.finish(get_target cr)
