(* File: cloud.ml

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

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
let make cr canvas ?rotate:(rotp=0.) ?(padding=0.02) ~size ~color words =
  let region = ref [] in

  let rec position target col word =
    let vert = Random.float 1. < rotp in
    let te = text_extents cr word in
    let width, height = (if vert then te.height, te.width
                         else te.width, te.height) in
    let dx = canvas.w -. width
    and dy = canvas.h -. height in
    let x = canvas.x +. 0.5 *. dx +. rand(dx /. target)
    and y = canvas.y +. 0.5 *. dy +. rand(dy /. target) in
    (* make the rectangle slightly larger than the word to leave some
       space aroubd. *)
    let padding' = 1. +. 2. *. padding in
    let r = { x = x -. padding *. width;  y = y -. padding *. height;
              w = padding' *. width;  h = padding' *. height } in
    if intersect_region r !region || outside r canvas  then
      position (0.9995 *. target) col word
    else (
      region := r :: !region;
      set_source_rgba cr 0. 0. 0. 0.2;
      (* rectangle cr r.x r.y r.w r.h;  stroke cr; *)
      (let r, g, b, a = col in set_source_rgba cr r g b a);

      if vert then (
        translate cr (x -. te.y_bearing) (y +. height +. te.x_bearing);
        rotate cr neg_half_pi;
      )
      else
        move_to cr (x -. te.x_bearing) (y -. te.y_bearing);
      show_text cr word;
      stroke cr;
    )
  in

  List.iter begin fun (fq, word) ->
    save cr;
    set_font_size cr (size fq word);
    position 2. (color fq word) word;
    restore cr
  end words;


(* References to check:
  http://www.cs.cmu.edu/~sleator/papers/2d-bin-packing.htm
   http://www.mat.ucsb.edu/projects/TagRiver/browser/src/algorithms2/PackingAlgorithm3.java
   http://github.com/ninajansen/cloud/blob/6bda2f886ec34643294de636d87a48a59a71b171/lib/cloud/cloud.rb

  See also http://www.bewitched.com/research.html for interesting
  visualization algorithms.
*)



module Palette =
struct
  type t = (float * float * float * float) array

  let random p = p.(Random.int (Array.length p))

  let color (r, g, b) =
    (float r /. 255., float g /. 255., float b /. 255., 1.)

  let mauve = Array.map color [|
    (190, 73, 232); (207, 119, 238); (223, 165, 244); (162, 62, 197);
    (143, 55, 174); (95, 37, 116); (48, 18, 58); (19, 7, 23) |]

  let metal_blue = Array.map color [|
    (51, 68, 51); (51, 102, 170); (102, 153, 170); (170, 187, 187);
    (119, 136, 119) |]

  let blue_green = Array.map color [|
    (0, 17, 0); (0, 102, 221); (10, 204, 221); (119, 170, 119) |]

  let brown = Array.map color [|
    (167, 70, 97); (189, 117, 137); (212, 163, 177);
    (233, 209, 215); (142, 60, 82); (125, 53, 73);
    (84, 35, 49); (42, 18, 24); (17, 7, 10) |]


  let rainbow = Array.map color [|
    (176, 43, 44); (209, 86, 0); (199, 152, 16); (115, 136, 10);
    (107, 186, 112); (63, 76, 107); (53, 106, 160); (208, 31, 60) |]

end
