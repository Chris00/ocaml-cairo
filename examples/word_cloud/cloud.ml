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

(* Return a random number between [-0.5 *. x] and [0.5 *. x]. *)
let rand x = Random.float x -. 0.5 *. x


(* Inspired by ideas of Jim Lund, jiml at uky dot edu,
   http://elegans.uky.edu/blog/?p=103 *)
let make cr canvas ?(max_size=25.) ?rotate:(rotp=0.) words =
  let region = ref [] in

  let rec find_pos target vert word =
    let w = text_extents cr word in
    let width, height = (if vert then w.height, w.width
                         else w.width, w.height) in
    let dx = canvas.w -. width
    and dy = canvas.h -. height in
    let x = canvas.x +. 0.5 *. dx +. rand(dx /. target)
    and y = canvas.y +. 0.5 *. dy +. rand(dy /. target) in
    let r = (if vert then { x=x;  y = y;  w=width; h= height }
             else { x=x;  y= y +. w.y_bearing;  w=w.x_advance; h = height }) in
    if intersect_region r !region then
      find_pos (0.9995 *. target) vert word
    else (
      region := r :: !region;
(*       set_source_rgba cr 0. 0. 0. 0.2; *)
(*       rectangle cr r.x r.y r.w r.h;  stroke cr; *)
      if vert then (x -. w.y_bearing, y +. height) else (x, y)
    )
  in

  List.iter begin fun (freq, word) ->
    save cr;
    let vert = Random.float 1. < rotp in
    set_font_size cr (freq *. max_size);

    let x, y = find_pos 2. vert word in
    printf "%-35s: x=%g y=%g %s\n" word x y (if vert then "(r)" else "");
    if freq > 0.5 then
      set_source_rgb cr (0.2 +. Random.float 0.8) 0. (Random.float 0.5)
    else
      set_source_rgb cr (Random.float 0.5) 0. (0.2 +. Random.float 0.8);
    if vert then (
      translate cr x y;
      move_to cr 0. 0.;
      rotate cr neg_half_pi;
    )
    else move_to cr x y;
    show_text cr word;
    restore cr
  end words;
