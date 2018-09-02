(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let () =
  let cr = Cairo.create(Cairo.PDF.create "fill_stroke.pdf" ~w:400. ~h:300.) in

  move_to cr 128.0 25.6;
  line_to cr 230.4 230.4;
  rel_line_to cr (-102.4) 0.;
  curve_to cr 51.2 230.4 51.2 128.0 128.0 128.0;
  Path.close cr;

  move_to cr 64.0 25.6;
  rel_line_to cr 51.2 51.2;
  rel_line_to cr (-51.2) 51.2;
  rel_line_to cr (-51.2) (-51.2);
  Path.close cr;

  set_line_width cr 10.0;
  set_source_rgb cr 0. 0. 1.;
  fill_preserve cr;
  set_source_rgb cr 0. 0. 0.;
  stroke cr;

  Surface.finish(get_target cr)
