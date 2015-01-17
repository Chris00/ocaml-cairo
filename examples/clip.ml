(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

let pi = 4. *. atan 1.

let () =
  let surface = Cairo.PDF.create "clip.pdf" 300. 300. in
  let cr = Cairo.create surface in

  Cairo.arc cr 128. 128. 76.8 0. (2. *. pi);
  Cairo.clip cr;

  Cairo.Path.clear cr; (* current path is not consumed by Cairo.clip *)
  Cairo.rectangle cr 0. 0. 256. 256.;
  Cairo.fill cr;
  Cairo.set_source_rgb cr 0. 1. 0.;
  Cairo.move_to cr 0. 0.;
  Cairo.line_to cr 256. 256.;
  Cairo.move_to cr 256. 0.;
  Cairo.line_to cr 0.   256.;
  Cairo.set_line_width cr 10.;
  Cairo.stroke cr;

  Cairo.Surface.finish surface
