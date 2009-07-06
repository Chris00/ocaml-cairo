(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let two_pi = 8. *. atan 1.

let () =
  let cr = Cairo.create(Cairo.PDF.create "fill_style.pdf" 400. 300.) in

  set_line_width cr 6.;

  let figure fill_style r g b =
    rectangle cr 12. 12. 232. 70.;
    Path.sub cr;  arc cr 64. 64. 40. 0. two_pi;
    Path.sub cr;  arc_negative cr 192. 64. 40.  0. (-. two_pi);
    set_fill_rule cr fill_style;
    set_source_rgb cr r g b;     fill cr ~preserve:true;
    set_source_rgb cr 0. 0. 0.;  stroke cr
  in

  figure EVEN_ODD 0. 0.7 0.;
  translate cr 0. 128.;
  figure WINDING  0. 0. 0.9;

  Surface.finish(get_target cr)
