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

open Cairo

type rgba = float * float * float * float

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

[@@@warning "-37"]
type position =
  | C    (* center around both x and y *)
  | R    (* right of (x,y) *)
  | L
  | U
  | D
  | RU
  | RD
  | LU
  | LD

module Text =
struct
  let size cr ?(vert=false) text =
    let te = text_extents cr text in
    if vert then te.height, te.width else te.width, te.height

  (* Return the box that the text will occupy if it is put at the
     position [pos]. *)
  let box cr ?vert ?(padding=0.02) pos x y text =
    let width, height = size cr ?vert text in
    let x0 = match pos with
      | C | U | D -> x -. 0.5 *. width
      | R | RU | RD -> x
      | L | LU | LD -> x -. width
    and y0 = match pos with
      | C | R | L -> y -. 0.5 *. height
      | U | RU | LU -> y -. height
      | D | RD | LD -> y  in
    let padding' = 1. +. 2. *. padding in
    { Cairo.x = x0 -. padding *. width;  y = y0 -. padding *. height;
      w = padding' *. width;  h = padding' *. height }
  ;;

  (* Display the [text] at position [pos] (vertically if [vert] is true). *)
  let show cr ?(vert=false) pos x y text =
    let te = text_extents cr text in
    let r = box cr ~vert ~padding:0. pos x y text in
    if vert then (
      translate cr (r.x -. te.y_bearing) (r.y +. r.h +. te.x_bearing);
      rotate cr neg_half_pi;
    )
    else
      move_to cr (r.x -. te.x_bearing) (r.y -. te.y_bearing);
    show_text cr text;
    stroke cr
end

(* ---------------------------------------------------------------------- *)
(* Inspired by ideas of Jim Lund, jiml at uky dot edu,
   http://elegans.uky.edu/blog/?p=103 *)

exception Failure

let make cr canvas ?rotate:(rotp=0.) ?padding ?(word_box=fun _ _ _ _ -> ())
    ~size ?(min_size=11.) ~color words =
  let region = ref [] in
  (* center of canvas *)
  let cx = canvas.x +. 0.5 *. canvas.w
  and cy = canvas.y +. 0.5 *. canvas.h in

  let rec position target sz fq word =
    let vert = Random.float 1. < rotp in
    let width, height = Text.size cr ~vert word in
    let x = cx +. rand((canvas.w -. width) /. target)
    and y = cy +. rand((canvas.h -. height) /. target) in
    let rect = Text.box cr ~vert ?padding C x y word in
    if intersect_region rect !region || outside rect canvas  then (
      let target = 0.9995 *. target in
      if target < 1. then (
        let sz = 0.9 *. sz in
        if sz < min_size then raise Failure;
        set_font_size cr sz;
        position 2. sz fq word
      )
      else position target sz fq word
    )
    else (
      region := rect :: !region;
      set_source_rgba cr 0. 0. 0. 0.2;
      (* rectangle cr r.x r.y r.w r.h;  stroke cr; *)
      let r, g, b, a = color fq word in set_source_rgba cr r g b a;
      Text.show cr ~vert C x y word;
      word_box sz (r,g,b,a) rect word;
    )
  in

  List.iter begin fun (fq, word) ->
    save cr;
    let sz = size fq word in
    set_font_size cr sz;
    position 2. sz fq word;
    restore cr
  end words
;;


(* ---------------------------------------------------------------------- *)
(* References to check:
   http://www.cs.cmu.edu/~sleator/papers/2d-bin-packing.htm
   http://www.mat.ucsb.edu/projects/TagRiver/browser/src/algorithms2/PackingAlgorithm3.java

   Another implementation using bin packing
   http://ninajansen.dk/2009/04/23/introducing-cloud-an-open-source-ruby-wordcloud-generator/
   (source git://github.com/ninajansen/cloud.git ); however the result
   did not look good enough for me so I did not implement it
   <http://www.scribd.com/tag/wordcloud>.

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

  let winter = Array.map color [|
    (0,0,0); (52, 83, 121); (64, 114, 176); (69, 134, 210);
    (155, 189, 246); (160, 189, 235) |]

  let heat = Array.map color [|
    (21, 0, 0); (119, 0, 0); (255, 0, 0); (203, 0, 0); (255, 66, 0) |]

  let blue_yellow = Array.map color [|
    (34, 68, 102); (102, 119, 136); (204, 170, 102); (136, 153, 170);
    (255, 238, 187) |]

  let clay = Array.map color [|
    (0,0,0); (113, 76, 63); (177, 88, 79); (212, 192, 196);
    (248, 214, 229); (188, 133, 136) |]

  let gray =
    let g x = (x, x, x, 1.) in [|  g 0.2;  g 0.4;  g 0.6; g 0.8 |]

  let light_gray =
    let g x = (x, x, x, 1.) in [|  g 0.5;  g 0.6; g 0.7; g 0.8 |]
end
