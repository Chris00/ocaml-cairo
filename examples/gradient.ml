(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "gradient.pdf" 400. 300.) in

  let pat = Pattern.create_linear 0.0 0.0  0.0 256.0 in
  Pattern.add_color_stop_rgba pat ~ofs:1. 0. 0. 0. 1.;
  Pattern.add_color_stop_rgba pat ~ofs:0. 1. 1. 1. 1.;
  rectangle cr 0. 0. 256. 256.;
  set_source cr pat;
  fill cr;

  let pat = Pattern.create_radial 115.2 102.4 25.6  102.4 102.4 128.0 in
  Pattern.add_color_stop_rgba pat  1. 1. 1. 1.;
  Pattern.add_color_stop_rgba pat ~ofs:1. 0. 0. 0. 1.;
  set_source cr pat;
  arc cr 128.0 128.0 76.8 0. two_pi;
  fill cr;

  Surface.finish(get_target cr)
