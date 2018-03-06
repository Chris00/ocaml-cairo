open Printf

let () =
  try
    let cr = Cairo.create(Cairo.PNG.create "curve_to.png") in
    Cairo.Surface.finish(Cairo.get_target cr);
    exit 1; (* should no reach this *)
  with e ->
    printf "As expected, raise the exception: %s\n" (Printexc.to_string e)
