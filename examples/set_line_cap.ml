(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "set_line_cap.pdf" ~w:400. ~h:300.) in

  let line x =
    set_source_rgb cr 0. 0. 0.;
    set_line_width cr 30.0;
    move_to cr x 50.0;   line_to cr x 200.0;
    stroke cr;
    (* draw helping lines *)
    set_source_rgb cr 1. 0.2 0.2;
    set_line_width cr 2.56;
    move_to cr x 50.0;   line_to cr x 200.0;
    stroke cr  in

  set_line_cap cr BUTT; (* default *)
  line 64.0;
  set_line_cap cr ROUND;
  line 128.0;
  set_line_cap cr SQUARE;
  line 192.0;

  Surface.finish(get_target cr)
