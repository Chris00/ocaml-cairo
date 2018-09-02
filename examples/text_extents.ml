(*						  -*- coding:utf-8 -*-  *)
(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "text_extents.pdf" ~w:400. ~h:300.) in

  (* Take the cont from the command line if given: *)
  let font = try Sys.argv.(1) with _ -> "Sans" in
  let utf8 = "cairo" in

  select_font_face cr font;
  set_font_size cr 100.0;
  let e = text_extents cr utf8 in
  let x = 25. in
  let y = 150. in

  move_to cr x y;  show_text cr utf8;
  show_text cr "∫";

  (* draw helping lines *)
  set_source_rgba cr 1. 0.2 0.2 0.6;
  set_line_width cr 6.0;
  arc cr x y ~r:10. ~a1:0. ~a2:two_pi;
  fill cr;
  move_to cr x y;
  rel_line_to cr 0. (-. e.height);
  rel_line_to cr e.width 0.;
  rel_line_to cr e.x_bearing (-. e.y_bearing);
  stroke cr;

  Surface.finish(get_target cr)
