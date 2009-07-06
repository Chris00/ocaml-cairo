(* Examples by Øyvind Kolås taken from http://cairographics.org/samples/ *)

let pi = 4. *. atan 1.

let draw_arc cr arc =
  let xc = 128.
  and yc = 128. in
  let radius = 100. in
  let angle1 = 45. *. pi /. 180.0 in
  let angle2 = 180. *. pi /. 180.0 in
  Cairo.set_line_width cr 10.;
  arc cr ~x:xc ~y:yc ~r:radius ~a1:angle1 ~a2:angle2;
  Cairo.stroke cr;

  (* draw helping lines *)
  Cairo.set_source_rgba cr 1. 0.2 0.2 0.6;
  Cairo.set_line_width cr 6.;

  Cairo.arc cr xc yc 10. 0. (2. *. pi);
  Cairo.fill cr;

  Cairo.arc cr xc yc radius angle1 angle1;
  Cairo.line_to cr xc yc;
  Cairo.arc cr xc yc radius angle2 angle2;
  Cairo.line_to cr xc yc;
  Cairo.stroke cr


let () =
  let surface = Cairo.PDF.create "arcs.pdf" 500. 300. in
  let cr = Cairo.create surface in

  (* Arc *)
  draw_arc cr Cairo.arc;

  Cairo.translate cr 200. 0.;
  (* Arc negative *)
  Cairo.set_source_rgb cr 0. 0.8 0.;
  draw_arc cr Cairo.arc_negative;

  Cairo.Surface.finish surface
