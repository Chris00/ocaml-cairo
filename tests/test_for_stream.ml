open Printf

let make create fname =
  let fh = open_out fname in
  let surface = create (output_string fh) ~w:100. ~h:100. in
  let ctx = Cairo.create surface in

  Cairo.set_line_width ctx 1.;
  Cairo.move_to ctx 0. 0.;
  Cairo.line_to ctx 100. 100.;
  Cairo.stroke ctx;
  Cairo.Surface.finish surface; (* Important for the data to be written *)
  flush fh;
  close_out fh;
  printf "Wrote %S.\n" fname

let () =
  let tmp = Filename.get_temp_dir_name() in
  make Cairo.SVG.create_for_stream (Filename.concat tmp "cairo-test.svg");
  Gc.major();
  make Cairo.PDF.create_for_stream (Filename.concat tmp "cairo-test.pdf");
