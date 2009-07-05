
let () =
  let surface = Cairo.PDF.create "hello.pdf" 1000 1000 in
  let cr = Cairo.create surface in
  Cairo.set_line_width cr 0.1;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 10. 10. 500. 500.;
  Cairo.stroke cr;
  Cairo.Surface.finish surface;

