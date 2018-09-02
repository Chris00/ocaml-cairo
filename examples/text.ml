(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "text.pdf" ~w:400. ~h:300.) in

  (* Take the cont from the command line if given: *)
  let font = try Sys.argv.(1) with _ -> "Sans" in
  select_font_face cr font ~weight:Bold;
  set_font_size cr 90.0;

  move_to cr 10. 135.;
  show_text cr "Hello";

  move_to cr 70. 165.;
  Path.text cr "void";
  set_source_rgb cr 0.5 0.5 1.;
  fill_preserve cr;
  set_source_rgb cr 0. 0. 0.;
  set_line_width cr 2.56;
  stroke cr;

  (* draw helping lines *)
  set_source_rgba cr 1. 0.2 0.2 0.6;
  arc cr 10. 135. ~r:5.12 ~a1:0. ~a2:two_pi;
  Path.close cr;
  arc cr 70. 165. ~r:5.12 ~a1:0. ~a2:two_pi;
  fill cr;

  Surface.finish(get_target cr)
