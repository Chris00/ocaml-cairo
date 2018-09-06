open Printf

let () =
  let surface = Cairo.Image.(create ARGB32 ~w:100 ~h:100) in
  let ctx = Cairo.create surface in

  Cairo.set_line_width ctx 1.;
  Cairo.move_to ctx 0. 0.;
  Cairo.line_to ctx 100. 100.;
  Cairo.stroke ctx;
  Cairo.Surface.finish surface;
  Cairo.line_to ctx 100. 0.;
  (* The following command should raise Error(SURFACE_FINISHED) but
     does not currently because of a bug in Cairo
     https://bugs.freedesktop.org/show_bug.cgi?id=68014 *)
  try Cairo.stroke ctx
  with Cairo.Error(Cairo.SURFACE_FINISHED) ->
    printf "OK, correct exception raised\n"
