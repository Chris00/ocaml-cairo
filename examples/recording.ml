(* Demonstrate the use of recording surfaces.  *)

open Printf

let () =
  let extents = { Cairo.x = 0.0; y = 0.0; w = 120.0; h = 120.0 } in
  let surface = Cairo.Recording.create ~extents Cairo.COLOR_ALPHA in
  let cr = Cairo.create surface in

  (* Drawing code goes here *)
  Cairo.set_line_width cr 2.;
  Cairo.set_source_rgb cr 1. 0. 0.;

  Cairo.move_to cr 25. 25.;
  Cairo.line_to cr 120. 120.;
  Cairo.stroke cr;
  let r = Cairo.Recording.ink_extents surface in
  printf "extents = {x=%g; y=%g; w=%g; h=%g}\n"
    r.Cairo.x r.Cairo.y r.Cairo.w r.Cairo.h;

  (* Replay the recorded content to a PNG and PDF output *)
  (* PNG *)
  let png_surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create png_surface in
  Cairo.set_source_surface cr surface ~x:0.0 ~y:0.0;
  Cairo.paint cr;
  Cairo.PNG.write png_surface "recording.png";
  (* PDF *)
  let pdf_surface = Cairo.PDF.create "recording.pdf" ~w:120.0 ~h:120.0 in
  let cr = Cairo.create pdf_surface in
  Cairo.set_source_surface cr surface ~x:0.0 ~y:0.0;
  Cairo.paint cr;
  Cairo.Surface.finish (Cairo.get_target cr)
