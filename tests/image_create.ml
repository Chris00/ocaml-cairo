open Printf
open Cairo
open Bigarray

let create() =
  let data = Array1.create int8_unsigned c_layout 360_000 in
  Gc.finalise (fun _ -> eprintf "DESTROY bigarray 'data'\n%!") data;
  let surf = Image.create_for_data8 data Image.RGB24 ~w:300 ~h:300 in
  Cairo.create surf

let () =
  let cr = create() in
  printf "With Cairo handle:\n%!";
  set_source_rgb cr 1. 1. 1.;
  rectangle cr 0. 0. ~w:300. ~h:300.;
  fill cr;

  Gc.compact();  Gc.compact();
  set_source_rgb cr 1. 0. 0.;
  move_to cr 10. 150.;
  set_font_size cr 100.;
  show_text cr "Hello";
  Gc.compact();  Gc.compact();

  eprintf "- Write image\n%!";
  PNG.write (get_target cr) "test_image.png";
  eprintf "- Finish surface\n%!";
  Surface.finish (get_target cr);
  Gc.compact()


(* Test for stride < 0 (not handled for now) and for incoherent width
   / stride *)
let () =
  let mat = Array1.create int8_unsigned c_layout 80_000 in
  let test_stride stride =
    try
      let surf = Image.create_for_data8 mat Image.A8 ~w:100 ~h:100 ~stride in
      assert(Image.get_stride surf = stride)
    with Error INVALID_STRIDE ->
      assert(stride < 100)
  in

  test_stride 108;
  test_stride 99;
  test_stride 0;
  test_stride (-108);
