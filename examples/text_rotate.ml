open Printf
open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "text_rotate.pdf" 400. 400.) in

  (* Take the cont from the command line if given: *)
  let font = try Sys.argv.(1) with _ -> "Sans" in
  select_font_face cr font;
  set_font_size cr 30.0;

  let x0 = 200. and y0 = 200. in
  translate cr x0 y0;
  save cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  move_to cr (-150.) 0.;  line_to cr 300. 0.;  stroke cr;
  move_to cr 0. (-150.);  line_to cr 0. 300.;  stroke cr;
  restore cr;
  let n = 12 in
  let da = two_pi /. float n in
  for i = 0 to n - 1 do
    save cr;
    let angle = float i *. da in
    rotate cr angle;
    move_to cr 0. 0.;

    show_text cr (sprintf "angle %g" (360. *. float i /. float n));
    restore cr;
  done;
  Surface.finish(get_target cr)
