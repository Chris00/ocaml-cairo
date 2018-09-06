open Cairo

let () =
  let cr = create (Image.create Image.ARGB32 ~w:100 ~h:100) in
  let m = { xx = 1.; xy = 2.; yx = 3.; yy = 4.; x0 = 5.; y0 = 6. } in
  set_matrix cr m;
  assert(get_matrix cr = m)

(* Font *)
let () =
  let m1 = Matrix.init_identity() in
  let m2 = Matrix.init_translate 10. 20. in
  let ff = Font_face.create Upright Normal in
  let fo = Font_options.create() in
  let sf = Scaled_font.create ff m1 m2 fo in
  assert(Scaled_font.get_font_matrix sf = m1);
  assert(Scaled_font.get_ctm sf = Matrix.init_identity());
  assert(Scaled_font.get_font_options sf = fo)


(* Local Variables: *)
(* compile-command: "make -k -C.." *)
(* End: *)
