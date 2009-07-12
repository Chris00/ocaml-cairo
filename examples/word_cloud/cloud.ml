open Printf
open Cairo

let neg_half_pi = -2. *. atan 1.


(* Whether two rectangles intersect. *)
let intersect r1 r2 =
  r1.x <= r2.x +. r2.w && r1.y <= r2.y +. r2.h
  && r2.x <= r1.x +. r1.w && r2.y <= r1.y +. r1.h

(* Whther [r] intersect the region defined as the union of rectangles
   in the list.  (This is pretty naive, ordering rectangles should
   allow to compite this much faster.) *)
let rec intersect_region r region = match region with
  | [] -> false
  | r' :: tl -> intersect r r' || intersect_region r tl

let outside r canvas =
  r.x < canvas.x || r.y < canvas.y
  || r.x +. r.w > canvas.x +. canvas.w || r.y +. r.h > canvas.y +. canvas.h

(* Return a random number between [-0.5 *. x] and [0.5 *. x]. *)
let rand x = Random.float x -. 0.5 *. x


(* Inspired by ideas of Jim Lund, jiml at uky dot edu,
   http://elegans.uky.edu/blog/?p=103 *)
let make cr canvas ?rotate:(rotp=0.) ~size ~color words =
  let region = ref [] in

  let rec find_pos target col word =
    let vert = Random.float 1. < rotp in
    let te = text_extents cr word in
    let width, height = (if vert then te.height, te.width
                         else te.width, te.height) in
    let dx = canvas.w -. width
    and dy = canvas.h -. height in
    let x = canvas.x +. 0.5 *. dx +. rand(dx /. target)
    and y = canvas.y +. 0.5 *. dy +. rand(dy /. target) in
    let r = { x = x;  y = y; w = width;  h = height } in
    if intersect_region r !region || outside r canvas  then
      find_pos (0.9995 *. target) col word
    else (
      region := r :: !region;
      set_source_rgba cr 0. 0. 0. 0.2;
      (* rectangle cr r.x r.y r.w r.h;  stroke cr; *)
      (let r, g, b, a = col in set_source_rgba cr r g b a);

      if vert then (
        translate cr (r.x -. te.y_bearing) (r.y +. height +. te.x_bearing);
        rotate cr neg_half_pi;
      )
      else
        move_to cr (r.x -. te.x_bearing) (r.y -. te.y_bearing);
      show_text cr word;
      stroke cr;
    )
  in

  List.iter begin fun (fq, word) ->
    save cr;
    set_font_size cr (size fq word);
    find_pos 2. (color fq word) word;
    restore cr
  end words;
