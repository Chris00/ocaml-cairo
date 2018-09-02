(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let () =
  let cr = Cairo.create(Cairo.PDF.create "set_line_join.pdf" ~w:400. ~h:300.) in

  set_line_width cr 40.96;

  let corner join =
    rel_line_to cr 51.2 (-51.2);
    rel_line_to cr 51.2 51.2;
    set_line_join cr join;
    stroke cr in

  move_to cr 76.8 84.48;
  corner JOIN_MITER; (* default *)

  move_to cr 76.8 161.28;
  corner JOIN_BEVEL;

  move_to cr 76.8 238.08;
  corner JOIN_ROUND;

  Surface.finish(get_target cr)
