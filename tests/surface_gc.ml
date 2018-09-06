open Printf
open Cairo

(* Test that using the ref-count of the surface to express its
   dependency on the context works. *)
let image_context () =
  let surf = Image.create Image.ARGB32 ~w:100 ~h:100 in
  Gc.finalise (fun _ -> eprintf "`surf' is collected by the GC.\n%!") surf;
  create surf

let () =
  let cr = image_context() in
  Gc.finalise (fun _ -> eprintf "`cr' is collected by the GC.\n%!") cr;
  printf "`surf' should be garbage collected but the surface still held \
	by `cr'.\n%!";
  Gc.compact();  Gc.compact();
  Surface.finish(get_target cr);
  printf "`cr' should be garbage collected.\n%!";
  Gc.compact();  Gc.compact()
