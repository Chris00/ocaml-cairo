(* 						-*- coding:utf-8 -*- *)
open Printf
open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "text_rotate.pdf" ~w:400. ~h:400.) in

  (* Take the cont from the command line if given: *)
  let font = try Sys.argv.(1) with _ -> "Georgia" in
  select_font_face cr font;
  set_font_size cr 50.0;

  let x0 = 200. and y0 = 200. in
  translate cr x0 y0;
  save cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  move_to cr (-150.) 0.;  line_to cr 300. 0.;  stroke cr;
  move_to cr 0. (-150.);  line_to cr 0. 300.;  stroke cr;
  restore cr;
  let n = 4 in
  let da = two_pi /. float n in
  move_to cr 0. 0.;
  for i = 0 to n - 1 do
    save cr;
    let angle = float i *. da in
    set_source_rgba cr 0. 0. 0. (float(n - i) /. float n);
    rotate cr angle;

    show_text cr (sprintf "j φ=%g°" (360. *. float i /. float n));
    set_source_rgba cr 0. 0. 1. 0.3;
    arc cr 0. 0. ~r:2. ~a1:0. ~a2:two_pi;
    fill cr;
    restore cr;
  done;
  Surface.finish(get_target cr)
