(* let _ = Cairo.(Error INVALID_RESTORE) *)

let () =
  let cr = Cairo.create(Cairo.PNG.create "curve_to.png") in
  Cairo.Surface.finish(Cairo.get_target cr)
