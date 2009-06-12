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

type status =
  | NO_MEMORY
  | INVALID_RESTORE
  | INVALID_POP_GROUP
  | NO_CURRENT_POINT
  | INVALID_MATRIX
  | INVALID_STATUS
  | NULL_POINTER
  | INVALID_STRING
  | INVALID_PATH_DATA
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
type pattern

external create : surface -> t = "caml_cairo_create"
external save : t -> unit = "caml_cairo_save"
external restore : t -> unit = "caml_cairo_restore"

external get_target : t -> surface = "caml_cairo_get_target"

module Group =
struct
  external push_group : t -> unit = "caml_cairo_push_group"
  external push_group_with_content : t -> content -> unit
    = "caml_cairo_push_group_with_content"

  let push ?content cr =
    match content with
    | None -> push_group_stub cr
    | Some c -> push_group_with_content cr c

  external pop : t -> pattern = "caml_cairo_pop_group"
  external pop_to_source : t -> unit = "caml_cairo_pop_group_to_source"

  external get_target : t -> surface = "caml_cairo_get_group_target"
end

external set_source_rgb : t -> r:float -> g:float -> b:float -> unit
  = "caml_cairo_set_source_rgb"

external set_source_rgba : t -> r:float -> g:float -> b:float -> a:float -> unit
  = "caml_cairo_set_source_rgba"

external set_source : t -> Pattern.t -> unit = "caml_cairo_set_source"

external get_source : t -> Pattern.t = "caml_cairo_get_source"

type antialias =
  | ANTIALIAS_DEFAULT
  | ANTIALIAS_NONE
  | ANTIALIAS_GRAY
  | ANTIALIAS_SUBPIXEL

external set_antialias : t -> antialias -> unit = "caml_cairo_set_antialias"
external get_antialias : t -> antialias = "caml_cairo_get_antialias"

external set_dash_stub : t -> float array -> osf:float -> unit
  = "caml_cairo_set_dash"

let set_dash cr ?(ofs=0.0) dashes = set_dash_stub cr ofs dashes

external get_dash : t -> float array * float = "caml_cairo_get_dash"





(* ---------------------------------------------------------------------- *)

module Surface =
struct
  type t = surface

  type content = COLOR | ALPHA | COLOR_ALPHA
end

(* ---------------------------------------------------------------------- *)

module Pattern =
struct
  type t = pattern


end
