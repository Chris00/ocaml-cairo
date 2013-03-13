(* Demo to show how one could achieve cairo drawing on a Graphics
   window. Note that using XLib or GTK would be *much* faster. *)

open Printf
open Bigarray
open Cairo

let pi2 = 8. *. atan 1.

let lastfps = ref (Unix.gettimeofday ())
let frames = ref 0
let fps = ref 0.

let update_fps () =
  let t = Unix.gettimeofday () in
  let dt = t -. !lastfps in
  if dt > 0.5 then (
    fps := float !frames /. dt;
    frames := 0;
    lastfps := t
  );
  incr frames

let draw cr width height x y =
  let x = x -. width *. 0.5 and y = y -. height *. 0.5 in
  let r = 0.5 *. sqrt (x *. x +. y *. y) in
  set_source_rgba cr 0. 1. 0. 0.5;
  arc cr (0.5 *. width) (0.35 *. height) r 0. pi2;
  fill cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  arc cr (0.35 *. width) (0.65 *. height) r 0. pi2;
  fill cr;
  set_source_rgba cr 0. 0. 1. 0.5;
  arc cr (0.65 *. width) (0.65 *. height) r 0. pi2;
  fill cr;
  set_source_rgba cr 1. 1. 0. 1.;
  move_to cr (0.05 *. width) (0.95 *. height);
  show_text cr (sprintf "%gx%g -- %.0f fps" width height !fps)

let expose () =
  let sx = Graphics.size_x ()
  and sy = Graphics.size_y ()
  and mx, my = Graphics.mouse_pos () in
  (* Create a cairo context from a cairo surface and do our drawings
     on it. Note: we may cache it between expose events for
     incremental drawings but its creation and initialization is not
     the time bottleneck here. *)
  let cr_img = Image.create Image.RGB24 sx sy in
  let cr = create cr_img in
  draw cr (float sx) (float sy) (float mx) (float my);
  (* Don't forget to flush the surface before using its content. *)
  Surface.flush cr_img;
  (* Now, access the surface data and convert it to a Graphics.image
     that can be drawn on the Graphics window. *)
  let data32 = Image.get_data32 cr_img in
  let data_img =
    Array.init sy
      (fun y -> Array.init sx (fun x -> Int32.to_int (data32.{y, x})))
  in
  Graphics.draw_image (Graphics.make_image data_img) 0 0;
  Graphics.synchronize ();
  (* Update our fps counter. *)
  update_fps ()

let () =
  Graphics.open_graph "";
  Graphics.auto_synchronize false;
  while true do
    expose ()
  done
