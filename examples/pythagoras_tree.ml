(* Example inspired from
   http://www.ffconsultancy.com/products/fsharp_for_visualization/demo6.html
*)

open Cairo

let pi = acos(-1.)

let set_green cr = Cairo.set_source_rgb cr 0. 0.7 0.
let set_darkgreen cr = Cairo.set_source_rgb cr 0. 0.5 0.
let set_burlywood cr = Cairo.set_source_rgb cr 0.87 0.72 0.53

let transform_data m = function
  | MOVE_TO (x, y) -> let x, y = Matrix.transform_point m x y in
                      MOVE_TO (x, y)
  | LINE_TO (x, y) -> let x, y = Matrix.transform_point m x y in
                      LINE_TO (x, y)
  | CURVE_TO (x1,y1, x2,y2, x3,y3) ->
     let x1, y1 = Matrix.transform_point m x1 y1
     and x2, y2 = Matrix.transform_point m x2 y2
     and x3, y3 = Matrix.transform_point m x3 y3 in
     CURVE_TO (x1,y1, x2,y2, x3,y3)
  | CLOSE_PATH -> CLOSE_PATH

let transform m path = Array.map (transform_data m) path

(* Transform matrices (in "abstract" coordinates) *)
let m1 = Matrix.(let m = init_translate 0. 1. in (* last *)
                 scale m (4. /. 5.) (4. /. 5.);
                 rotate m (0.5 *. pi -. asin(4. /. 5.)); (* first *)
                 m)
let m2 = Matrix.(let m = init_translate 1. 1. in
                 scale m (3. /. 5.) (3. /. 5.);
                 rotate m (-0.5 *. pi +. asin(3. /. 5.));
                 translate m (-1.) 0.;
                 m)

let rec tree cr n square =
  if n = 0 then (
    set_darkgreen cr;
    Path.append cr (Path.of_array square);
    fill cr;
  )
  else (
    set_burlywood cr;
    Path.append cr (Path.of_array square);
    fill_preserve cr;
    set_green cr;
    stroke cr;
    (* Simple (but not very efficient) to ensure that all squares of a
       given level is drawn at the same time. *)
    let m = Array.append (transform m1 square) (transform m2 square) in
    tree cr (n - 1) m
  )


let () =
  let surface = Cairo.PDF.create "pythagoras_tree.pdf" ~w:300. ~h:250. in
  let cr = Cairo.create surface in
  translate cr 150. 220.;
  scale cr 45. (-45.);
  set_line_width cr 0.01; (* compensate scaling *)

  let square = [| MOVE_TO (0., 0.); LINE_TO (1., 0.); LINE_TO (1., 1.);
                  LINE_TO (0., 1.); CLOSE_PATH |] in
  tree cr 12 square;
  Cairo.Surface.finish surface
