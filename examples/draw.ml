(* This file is part of the tutorial
   http://cairo.forge.ocamlcore.org/tutorial/
*)

let pi_4 = atan 1.
let two_pi = 8. *. pi_4

let draw_path_moveto cr =
  Cairo.set_line_width cr 0.1;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.move_to cr 0.25 0.25

let draw_path_lineto cr =
  draw_path_moveto cr;
  Cairo.line_to cr 0.5 0.375;
  Cairo.rel_line_to cr 0.25 (-0.125)

let draw_path_arcto cr =
  draw_path_lineto cr;
  Cairo.arc cr 0.5 0.5 ~r:(0.25 *. sqrt 2.) ~a1:(-. pi_4) ~a2:pi_4

let draw_path_curveto cr =
  draw_path_arcto cr;
  Cairo.rel_curve_to cr (-0.25) (-0.125) (-0.25) 0.125 (-0.5) 0.

let draw_path_close cr =
  draw_path_curveto cr;
  Cairo.Path.close cr


let draw_textextents cr =
  let text = "joy" in
  Cairo.set_font_size cr 0.5;

  (* Drawing code goes here *)
  Cairo.set_source_rgb cr 0.0 0.0 0.0;
  Cairo.select_font_face cr "Georgia" ~weight:Cairo.Bold;
  let ux, uy = Cairo.device_to_user_distance cr 1. 1. in
  let px = max ux uy in
  let fe = Cairo.font_extents cr in
  let te = Cairo.text_extents cr text in
  let x = 0.5 -. te.Cairo.x_bearing -. te.Cairo.width /. 2.
  and y = 0.5 -. fe.Cairo.descent +. fe.Cairo.baseline /. 2. in

  (* baseline, descent, ascent, height *)
  Cairo.set_line_width cr (4. *. px);
  Cairo.set_dash cr [| 9. *. px |];
  Cairo.set_source_rgba cr 0. 0.6 0. 0.5;
  let horizontal_line y =
    Cairo.move_to cr (x +. te.Cairo.x_bearing) y;
    Cairo.rel_line_to cr te.Cairo.width 0. in
  horizontal_line y;
  horizontal_line (y +. fe.Cairo.descent);
  horizontal_line (y -. fe.Cairo.ascent);
  horizontal_line (y -. fe.Cairo.baseline);
  Cairo.stroke cr;

  (* extents: width & height (in dashed blue) *)
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  Cairo.set_line_width cr px;
  Cairo.set_dash cr [| 3. *. px |];
  Cairo.rectangle cr (x +. te.Cairo.x_bearing)
    (y +. te.Cairo.y_bearing) ~w:te.Cairo.width ~h:te.Cairo.height;
  Cairo.stroke cr;

  (* text *)
  Cairo.move_to cr x y;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.show_text cr text;

  (* bearing (solid blue line) *)
  Cairo.set_dash cr [| |];
  Cairo.set_line_width cr (2. *. px);
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  Cairo.move_to cr x y;
  Cairo.rel_line_to cr te.Cairo.x_bearing te.Cairo.y_bearing;
  Cairo.stroke cr;

  (* text's advance (blue dot) *)
  Cairo.set_source_rgba cr 0. 0. 0.75 0.5;
  Cairo.arc cr (x +. te.Cairo.x_advance) (y +. te.Cairo.y_advance)
    ~r:(6. *. px) ~a1:0. ~a2:two_pi;
  Cairo.fill cr;

  (* reference point (x,y) (red dot) *)
  Cairo.arc cr x y ~r:(6. *. px) ~a1:0. ~a2:two_pi;
  Cairo.set_source_rgba cr 0.75 0. 0. 0.5;
  Cairo.fill cr
;;


let draw_setsourcegradient cr =
  let radpat = Cairo.Pattern.create_radial ~x0:0.25 ~y0:0.25 ~r0:0.1
                                           ~x1:0.5  ~y1:0.5  ~r1:0.5 in
  Cairo.Pattern.add_color_stop_rgb radpat 1.0 0.8 0.8;
  Cairo.Pattern.add_color_stop_rgb radpat ~ofs:1. 0.9 0.0 0.0;

  for i = 1 to 9 do
    for j = 1 to 9 do
      Cairo.rectangle cr (float i /. 10.0 -. 0.04) (float j /. 10.0 -. 0.04)
        ~w:0.08 ~h:0.08;
    done;
  done;
  Cairo.set_source cr radpat;
  Cairo.fill cr;

  let linpat = Cairo.Pattern.create_linear ~x0:0.25 ~y0:0.35
                                           ~x1:0.75 ~y1:0.65 in
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.00  1. 1. 1. 0.;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.25  0. 1. 0. 0.5;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.50  1. 1. 1. 0.;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:0.75  0. 0. 1. 0.5;
  Cairo.Pattern.add_color_stop_rgba linpat ~ofs:1.00  1. 1. 1. 0.;
  Cairo.rectangle cr 0.0 0.0 ~w:1. ~h:1.;
  Cairo.set_source cr linpat;
  Cairo.fill cr
;;

let get_point = function
  | Cairo.MOVE_TO(x,y) -> x,y
  | Cairo.LINE_TO(x,y) -> x,y
  | Cairo.CURVE_TO(x,y, _,_, _,_) -> x,y
  | Cairo.CLOSE_PATH -> failwith "get_point"

let path_diagram cr =
  let path = Cairo.Path.to_array(Cairo.Path.copy_flat cr) in
  let px, py = Cairo.device_to_user_distance cr 3. 3. in
  Cairo.set_line_width cr (max px py);
  Cairo.set_source_rgb cr 0. 0.6 0.;
  Cairo.stroke cr;

  (* Draw markers at the first and the last point of the path, but
     only if the path is not closed.

     If the last path manipulation was a Cairo.Path.close, then we
     can detect this at the end of the path array.  The [CLOSE_PATH]
     element will be followed by a [MOVE_TO] element (since cairo
     1.2.4), so we need to check position [Array.length path - 2].
     See the module [Path] for further explanations. *)
  let len = Array.length path in
  if len <= 1 || path.(len - 2) <> Cairo.CLOSE_PATH then (
    (* Get the first point in the path *)
    let x, y = get_point path.(0) in
    let px, py = Cairo.device_to_user_distance cr 5. 5. in
    let px = max px py in
    Cairo.arc cr x y ~r:px ~a1:0. ~a2:two_pi;
    Cairo.set_source_rgba cr 0.0 0.6 0.0 0.5;
    Cairo.fill cr;

    let x, y = get_point path.(len - 1) in
    Cairo.arc cr x y ~r:px ~a1:0. ~a2:two_pi;
    Cairo.set_source_rgba cr 0.0 0.0 0.75 0.5;
    Cairo.fill cr;
  )
;;

let draw_path_curveto_hints cr =
  Cairo.save cr;
  let px, py = Cairo.device_to_user_distance cr 3. 3. in
  let px = max px py in
  Cairo.set_source_rgba cr 0.5 0. 0. 0.5;
  Cairo.Path.sub cr;
  Cairo.arc cr 0.5 0.625 ~r:px ~a1:0. ~a2:two_pi;
  Cairo.fill cr;
  Cairo.arc cr 0.5 0.875 ~r:px ~a1:0. ~a2:two_pi;
  Cairo.fill cr;

  let px, py = Cairo.device_to_user_distance cr 2. 2. in
  let px = max px py in
  Cairo.set_line_width cr px;
  Cairo.set_source_rgba cr 0.5 0. 0. 0.25;
  Cairo.move_to cr 0.25 0.75;
  Cairo.rel_line_to cr 0.25 0.125;
  Cairo.stroke cr;

  Cairo.move_to cr 0.75 0.75;
  Cairo.rel_line_to cr (-0.25) (-0.125);
  Cairo.stroke cr;

  Cairo.restore cr
;;

let draw_setsourcergba cr =
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.move_to cr 0. 0.;
  Cairo.line_to cr 1. 1.;
  Cairo.move_to cr 1. 0.;
  Cairo.line_to cr 0. 1.;
  Cairo.set_line_width cr 0.2;
  Cairo.stroke cr;

  Cairo.rectangle cr 0. 0. ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 1. 0. 0. 0.80;
  Cairo.fill cr;

  Cairo.rectangle cr 0. 0.5 ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 0. 1. 0. 0.60;
  Cairo.fill cr;

  Cairo.rectangle cr 0.5 0. ~w:0.5 ~h:0.5;
  Cairo.set_source_rgba cr 0. 0. 1. 0.40;
  Cairo.fill cr
;;

let draw_diagram name cr =
  (match name with
   | "setsourcergba" -> draw_setsourcergba cr
   | "setsourcegradient" -> draw_setsourcegradient cr
   | "path-moveto" -> draw_path_moveto cr
   | "path-lineto" -> draw_path_lineto cr
   | "path-arcto" -> draw_path_arcto cr
   | "path-curveto" ->
       draw_path_curveto_hints cr;
       draw_path_curveto cr
   | "path-close" -> draw_path_close cr
   | "textextents" -> draw_textextents cr
   | _ -> assert false
  );
  if String.sub name 0 5 = "path-" then path_diagram cr

let diagram name =
  let width = 120. and height = 120. in
  let svg_filename = name ^ ".svg"
  and png_filename = name ^ ".png" in
  let surf = Cairo.SVG.create svg_filename ~w:width ~h:height in
  let cr = Cairo.create surf in

  Cairo.scale cr width height;
  Cairo.set_line_width cr 0.01;

  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.fill cr;

  draw_diagram name cr;

  let ux, uy = Cairo.device_to_user_distance cr 2. 2. in
  Cairo.set_line_width cr (max ux uy);
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0. 0. ~w:1. ~h:1.;
  Cairo.stroke cr;

  (* write output *)
  Cairo.PNG.write surf png_filename;
  Cairo.Surface.finish surf

let () =
  diagram "setsourcergba";
  diagram "setsourcegradient";
  diagram "path-moveto";
  diagram "path-lineto";
  diagram "path-arcto";
  diagram "path-curveto";
  diagram "path-close";
  diagram "textextents"
