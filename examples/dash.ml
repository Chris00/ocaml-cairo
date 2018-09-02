(* Example by Øyvind Kolås taken from http://cairographics.org/samples/ *)

open Cairo

let () =
  let cr = Cairo.create(Cairo.PDF.create "dash.pdf" ~w:400. ~h:300.) in

  let dashes = [| 50.0;  (* ink *)
                  10.0;  (* skip *)
                  10.0;  (* ink *)
                  10.0   (* skip*)
               |] in
  let ofs = -50.0 in

  set_dash cr dashes ~ofs;
  set_line_width cr 10.0;

  move_to cr 128.0 25.6;
  line_to cr 230.4 230.4;
  rel_line_to cr (-102.4) 0.0;
  curve_to cr 51.2 230.4 51.2 128.0 128.0 128.0;
  stroke cr;

  Surface.finish(get_target cr)
