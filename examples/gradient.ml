(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "gradient.pdf" ~w:400. ~h:300.) in

  let pat = Pattern.create_linear ~x0:0.0 ~y0:0.0  ~x1:0.0 ~y1:256.0 in
  Pattern.add_color_stop_rgba pat ~ofs:1. 0. 0. 0. 1.;
  Pattern.add_color_stop_rgba pat ~ofs:0. 1. 1. 1. 1.;
  rectangle cr 0. 0. ~w:256. ~h:256.;
  set_source cr pat;
  fill cr;

  let pat = Pattern.create_radial ~x0:115.2 ~y0:102.4 ~r0:25.6
              ~x1:102.4 ~y1:102.4 ~r1:128.0 in
  Pattern.add_color_stop_rgba pat  1. 1. 1. 1.;
  Pattern.add_color_stop_rgba pat ~ofs:1. 0. 0. 0. 1.;
  set_source cr pat;
  arc cr 128.0 128.0 ~r:76.8 ~a1:0. ~a2:two_pi;
  fill cr;

  Surface.finish(get_target cr)
