(* File: cairo.ml

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

(* Keep in sync with the C function caml_raise_Error *)
type status =
  (* Programmer error *)
  | INVALID_RESTORE
  | INVALID_POP_GROUP
  | NO_CURRENT_POINT
  | INVALID_MATRIX
  | INVALID_STATUS
  (* Language binding implementation *)
  | NULL_POINTER
  | INVALID_STRING
  | INVALID_PATH_DATA
  (* Other *)
  | READ_ERROR
  | WRITE_ERROR
  | SURFACE_FINISHED
  | SURFACE_TYPE_MISMATCH
  | PATTERN_TYPE_MISMATCH
  | INVALID_CONTENT
  | INVALID_FORMAT
  | INVALID_VISUAL
  | FILE_NOT_FOUND
  | INVALID_DASH
  | INVALID_DSC_COMMENT
  | INVALID_INDEX
  | CLIP_NOT_REPRESENTABLE
  | TEMP_FILE_ERROR
  | INVALID_STRIDE
  | FONT_TYPE_MISMATCH
  | USER_FONT_IMMUTABLE
  | USER_FONT_ERROR
  | NEGATIVE_COUNT
  | INVALID_CLUSTERS
  | INVALID_SLANT
  | INVALID_WEIGHT

exception Error of status
let () = Callback.register_exception "Cairo.Error" (Error INVALID_RESTORE)

external status_to_string  : status -> string = "caml_cairo_status_to_string"

type t
type surface
type surface_content = COLOR | ALPHA | COLOR_ALPHA
type 'a pattern
type any_pattern = [`Solid | `Surface | `Gradient | `Linear | `Radial] pattern
type glyph = { index: int;  x: float;  y: float }

external create : surface -> t = "caml_cairo_create"
external save : t -> unit = "caml_cairo_save"
external restore : t -> unit = "caml_cairo_restore"

external get_target : t -> surface = "caml_cairo_get_target"

module Group =
struct
  external push_group : t -> unit = "caml_cairo_push_group"
  external push_group_with_content : t -> surface_content -> unit
    = "caml_cairo_push_group_with_content"

  let push ?content cr =
    match content with
    | None -> push_group cr
    | Some c -> push_group_with_content cr c

  external pop : t -> any_pattern = "caml_cairo_pop_group"
  external pop_to_source : t -> unit = "caml_cairo_pop_group_to_source"

  external get_target : t -> surface = "caml_cairo_get_group_target"
end

external set_source_rgb : t -> r:float -> g:float -> b:float -> unit
  = "caml_cairo_set_source_rgb"

external set_source_rgba : t -> r:float -> g:float -> b:float -> a:float -> unit
  = "caml_cairo_set_source_rgba"

external set_source : t -> 'a pattern -> unit = "caml_cairo_set_source"

external get_source : t -> any_pattern = "caml_cairo_get_source"

type antialias =
  | ANTIALIAS_DEFAULT
  | ANTIALIAS_NONE
  | ANTIALIAS_GRAY
  | ANTIALIAS_SUBPIXEL

external set_antialias : t -> antialias -> unit = "caml_cairo_set_antialias"
external get_antialias : t -> antialias = "caml_cairo_get_antialias"

external set_dash_stub : t -> float array -> ofs:float -> unit
  = "caml_cairo_set_dash"

let set_dash cr ?(ofs=0.0) dashes = set_dash_stub cr dashes ~ofs

external get_dash : t -> float array * float = "caml_cairo_get_dash"

type fill_rule =
  | WINDING
  | EVEN_ODD

external set_fill_rule : t -> fill_rule -> unit = "caml_cairo_set_fill_rule"
external get_fill_rule : t -> fill_rule = "caml_cairo_get_fill_rule"

type line_cap =
  | BUTT
  | ROUND
  | SQUARE

external set_line_cap : t -> line_cap -> unit = "caml_cairo_set_line_cap"
external get_line_cap : t -> line_cap = "caml_cairo_get_line_cap"

type line_join =
  | JOIN_MITER
  | JOIN_ROUND
  | JOIN_BEVEL

external set_line_join : t -> line_join -> unit = "caml_cairo_set_line_join"
external get_line_join : t -> line_join = "caml_cairo_get_line_join"

external set_line_width : t -> float -> unit = "caml_cairo_set_line_width"
external get_line_width : t -> float = "caml_cairo_get_line_width"

external set_miter_limit : t -> float -> unit = "caml_cairo_set_miter_limit"
external get_miter_limit : t -> float = "caml_cairo_get_miter_limit"

type operator =
  | CLEAR
  | SOURCE
  | OVER
  | IN
  | OUT
  | ATOP
  | DEST
  | DEST_OVER
  | DEST_IN
  | DEST_OUT
  | DEST_ATOP
  | XOR
  | ADD
  | SATURATE

external set_operator : t -> operator -> unit = "caml_cairo_set_operator"
external get_operator : t -> operator = "caml_cairo_get_operator"

external set_tolerance : t -> float -> unit = "caml_cairo_set_tolerance"
external get_tolerance : t -> float = "caml_cairo_get_tolerance"

external clip_stub : t -> unit = "caml_cairo_clip"
external clip_preserve : t -> unit = "caml_cairo_clip_preserve"

let clip ?(preserve=false) cr =
  if preserve then clip_preserve cr else clip_stub cr

external clip_extents : t -> bounding_box = "caml_cairo_clip_extents"

external clip_reset : t -> unit = "caml_cairo_reset_clip"

external clip_rectangle_list : t -> rectangle list
  = "caml_cairo_copy_clip_rectangle_list"

external fill_stub : t -> unit = "caml_cairo_fill"
external fill_preserve : t -> unit = "caml_cairo_fill_preserve"

let fill ?(preserve=false) cr =
  if preserve then fill_preserve cr else fill_stub cr

external fill_extents : t -> bounding_box = "caml_cairo_fill_extents"

external in_fill : t -> x:float -> y:float -> bool = "caml_cairo_in_fill"

external mask : t -> 'a pattern -> unit = "caml_cairo_mask"
external mask_surface : t -> surface -> x:float -> y:float -> unit
  = "caml_cairo_mask_surface"

external paint_stub : t -> unit = "caml_cairo_paint"
external paint_with_alpha : t -> float -> unit = "caml_cairo_paint_with_alpha"

let paint ?alpha cr =
  match alpha with
  | None -> paint_stub cr
  | Some a -> paint_with_alpha cr a

external stroke_stub : t -> unit = "caml_cairo_stroke"
external stroke_preserve : t -> unit = "caml_cairo_stroke_preserve"

let stroke ?(preserve=false) cr =
  if preserve then stroke_preserve cr else stroke_stub cr

external stroke_extents : t -> bounding_box = "caml_cairo_stroke_extents"

external in_stroke : t -> x:float -> y:float -> bool = "caml_cairo_in_stroke"

external copy_page : t -> unit = "caml_cairo_copy_page"
external show_page : t -> unit = "caml_cairo_show_page"

(* ---------------------------------------------------------------------- *)

type path
type path_data =
  | MOVE_TO of float * float
  | LINE_TO of float * float
  | CURVE_TO of float * float * float * float * float * float
  | CLOSE_PATH

module Path =
struct
  external copy : t -> path = "caml_cairo_copy_path"
  external copy_flat : t -> path = "caml_cairo_copy_path_flat"
  external append : t -> path -> unit = "caml_cairo_append_path"
  external get_current_point : t -> float * float
    = "caml_cairo_get_current_point"
  external clear : t -> unit = "caml_cairo_new_path"
  external sub : t -> unit = "caml_cairo_new_sub_path"
  external close : t -> unit = "caml_cairo_close_path"

  external glyph : t -> glyph array -> unit = "caml_cairo_glyph_path"
  external text : t -> string -> unit = "caml_cairo_text_path"
  external extents : t -> bounding_box = "caml_cairo_path_extents"

  external fold : path -> ('a -> path_data -> 'a) -> 'a -> 'a
    = "caml_cairo_path_fold"
  external to_array : path -> path_data array = "caml_cairo_path_to_array"
  external of_array : path_data array -> path = "caml_cairo_path_of_array"
end


external arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float
  -> unit = "caml_cairo_arc_bc" "caml_cairo_arc"
external arc_negative : t -> x:float -> y:float -> r:float -> a1:float ->
  a2:float -> unit = "caml_cairo_arc_negative_bc" "caml_cairo_arc_negative"

external curve_to : t -> x1:float -> y1:float -> x2:float -> y2:float ->
  x3:float -> y3:float -> unit
  = "caml_cairo_curve_to_bc" "caml_cairo_curve_to"

external line_to : t -> x:float -> y:float -> unit = "caml_cairo_line_to"
external move_to : t -> x:float -> y:float -> unit = "caml_cairo_move_to"
external rectangle : t -> x:float -> y:float -> width:float -> height:float
  -> unit = "caml_cairo_rectangle"

external rel_curve_to : t -> x1:float -> y1:float -> x2:float -> y2:float ->
  x3:float -> y3:float -> unit
  = "caml_cairo_rel_curve_to_bc" "caml_cairo_rel_curve_to"

external rel_line_to : t -> x:float -> y:float -> unit
  = "caml_cairo_rel_line_to"
external rel_move_to : t -> x:float -> y:float -> unit
  = "caml_cairo_rel_move_to"


(* ---------------------------------------------------------------------- *)

module Surface =
struct
  type t = surface

  type content = surface_content = COLOR | ALPHA | COLOR_ALPHA
end

(* ---------------------------------------------------------------------- *)

module Pattern =
struct
  type 'a t = 'a pattern
  type any = any_pattern

  external add_color_stop_rgb_stub : [> `Gradient] t -> ofs:float ->
    r:float -> g:float -> b:float -> unit
    = "caml_cairo_pattern_add_color_stop_rgb" "noalloc"

  let add_color_stop_rgb cr ?(ofs=0.0) ~r ~g ~b =
    add_color_stop_rgb_stub cr ~ofs ~r ~g ~b

  external add_color_stop_rgba_stub : [> `Gradient] t -> ofs:float ->
    r:float -> g:float -> b:float -> a:float -> unit
    = "caml_cairo_pattern_add_color_stop_rgba_bc"
    "caml_cairo_pattern_add_color_stop_rgba" "noalloc"

  let add_color_stop_rgba cr ?(ofs=0.0) ~r ~g ~b =
    add_color_stop_rgba_stub cr ~ofs ~r ~g ~b

  external get_color_stop_count : [> `Gradient] t -> int
    = "caml_cairo_pattern_get_color_stop_count"

  external get_color_stop_rgba : [> `Gradient] t -> idx:int ->
    float * float * float * float * float
    = "caml_cairo_pattern_get_color_stop_rgba"
    (* FIXME: do we want to iterate over the colors instead ?? *)

  external create_rgb : r:float -> g:float -> b:float -> [`Solid] t
    = "caml_cairo_pattern_create_rgb"

  external create_rgba : r:float -> g:float -> b:float -> a:float -> [`Solid] t
    = "caml_cairo_pattern_create_rgba"

  external get_rgba : [> `Solid] t -> float * float * float * float
    = "caml_cairo_pattern_get_rgba"

  external create_for_surface : Surface.t -> [`Surface] t
    = "caml_cairo_pattern_create_for_surface"

  external get_surface : [`Surface] t -> Surface.t
    = "caml_cairo_pattern_get_surface"

  external create_linear : x0:float -> y0:float -> x1:float -> y1:float ->
    [`Linear | `Gradient] t = "caml_cairo_pattern_create_linear"

  external get_linear_points : [> `Linear|`Gradient] t
    -> float * float * float * float = "caml_cairo_pattern_get_linear_points"

  external create_radial : x0:float -> y0:float -> r0:float ->
    x1:float -> y1:float -> r1:float -> [`Radial | `Gradient] t
    = "caml_cairo_pattern_create_radial_bc" "caml_cairo_pattern_create_radial"

  external get_radial_circles : [> `Radial|`Gradient] t ->
    float * float * float * float * float * float
    = "caml_cairo_pattern_get_radial_circles"

  type extend =
    | NONE
    | REPEAT
    | REFLECT
    | PAD

  external set_extend : 'a t -> extend -> unit
    = "caml_cairo_pattern_set_extend" "noalloc"

  external get_extend : 'a t -> extend = "caml_cairo_pattern_get_extend"

  type filter =
    | FAST
    | GOOD
    | BEST
    | NEAREST
    | BILINEAR
    (* | GAUSSIAN *)

  external set_filter : 'a t -> filter -> unit
    = "caml_cairo_pattern_set_filter" "noalloc"

  external get_filter : 'a t -> filter = "caml_cairo_pattern_get_filter"

  external set_matrix : 'a t -> Matrix.t -> unit
    = "caml_cairo_pattern_set_matrix" "noalloc"

  external get_matrix : 'a t -> Matrix.t = "caml_cairo_pattern_get_matrix"
end

(* ---------------------------------------------------------------------- *)
(* Transformations - Manipulating the current transformation matrix  *)

external translate : t -> tx:float -> ty:float -> unit = "caml_cairo_translate"
external scale : t -> sx:float -> sy:float -> unit = "caml_cairo_scale"
external rotate : t -> angle:float -> unit = "caml_cairo_rotate"

external transform : t -> Matrix.t -> unit = "caml_cairo_transform" "noalloc"
external set_matrix : t -> Matrix.t -> unit = "caml_cairo_set_matrix" "noalloc"

external get_matrix : t -> Matrix.t = "caml_cairo_get_matrix"

external identity_matrix : t -> unit = "caml_cairo_identity_matrix"

external user_to_device : t -> x:float -> y:float -> float * float
  = "caml_cairo_user_to_device"
external user_to_device_distance : t -> x:float -> y:float -> float * float
  = "caml_cairo_user_to_device_distance"
external device_to_user : t -> x:float -> y:float -> float * float
  = "caml_cairo_device_to_user"
external device_to_user_distance : t -> x:float -> y:float -> float * float
  = "caml_cairo_device_to_user_distance"


(* ---------------------------------------------------------------------- *)

module Glyph =
struct
  type t = glyph = { index: int;  x: float;  y: float }

end
