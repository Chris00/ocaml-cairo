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

(* Keep in sync with the C function caml_cairo_raise_Error *)
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
  | INVALID_SIZE
  | USER_FONT_NOT_IMPLEMENTED
  | DEVICE_TYPE_MISMATCH
  | DEVICE_ERROR
  | INVALID_MESH_CONSTRUCTION
  | DEVICE_FINISHED
  | JBIG2_GLOBAL_MISSING

exception Error of status
let () = Callback.register_exception "Cairo.Error" (Error INVALID_RESTORE)

let error_of_status = function
  | INVALID_RESTORE        -> "Cairo.Error(INVALID_RESTORE)"
  | INVALID_POP_GROUP      -> "Cairo.Error(INVALID_POP_GROUP)"
  | NO_CURRENT_POINT       -> "Cairo.Error(NO_CURRENT_POINT)"
  | INVALID_MATRIX         -> "Cairo.Error(INVALID_MATRIX)"
  | INVALID_STATUS         -> "Cairo.Error(INVALID_STATUS)"
  | NULL_POINTER           -> "Cairo.Error(NULL_POINTER)"
  | INVALID_STRING         -> "Cairo.Error(INVALID_STRING)"
  | INVALID_PATH_DATA      -> "Cairo.Error(INVALID_PATH_DATA)"
  | READ_ERROR             -> "Cairo.Error(READ_ERROR)"
  | WRITE_ERROR            -> "Cairo.Error(WRITE_ERROR)"
  | SURFACE_FINISHED       -> "Cairo.Error(SURFACE_FINISHED)"
  | SURFACE_TYPE_MISMATCH  -> "Cairo.Error(SURFACE_TYPE_MISMATCH)"
  | PATTERN_TYPE_MISMATCH  -> "Cairo.Error(PATTERN_TYPE_MISMATCH)"
  | INVALID_CONTENT        -> "Cairo.Error(INVALID_CONTENT)"
  | INVALID_FORMAT         -> "Cairo.Error(INVALID_FORMAT)"
  | INVALID_VISUAL         -> "Cairo.Error(INVALID_VISUAL)"
  | FILE_NOT_FOUND         -> "Cairo.Error(FILE_NOT_FOUND)"
  | INVALID_DASH           -> "Cairo.Error(INVALID_DASH)"
  | INVALID_DSC_COMMENT    -> "Cairo.Error(INVALID_DSC_COMMENT)"
  | INVALID_INDEX          -> "Cairo.Error(INVALID_INDEX)"
  | CLIP_NOT_REPRESENTABLE -> "Cairo.Error(CLIP_NOT_REPRESENTABLE)"
  | TEMP_FILE_ERROR        -> "Cairo.Error(TEMP_FILE_ERROR)"
  | INVALID_STRIDE         -> "Cairo.Error(INVALID_STRIDE)"
  | FONT_TYPE_MISMATCH     -> "Cairo.Error(FONT_TYPE_MISMATCH)"
  | USER_FONT_IMMUTABLE    -> "Cairo.Error(USER_FONT_IMMUTABLE)"
  | USER_FONT_ERROR        -> "Cairo.Error(USER_FONT_ERROR)"
  | NEGATIVE_COUNT         -> "Cairo.Error(NEGATIVE_COUNT)"
  | INVALID_CLUSTERS       -> "Cairo.Error(INVALID_CLUSTERS)"
  | INVALID_SLANT          -> "Cairo.Error(INVALID_SLANT)"
  | INVALID_WEIGHT         -> "Cairo.Error(INVALID_WEIGHT)"
  | INVALID_SIZE           -> "Cairo.Error(INVALID_SIZE)"
  | USER_FONT_NOT_IMPLEMENTED -> "Cairo.Error(USER_FONT_NOT_IMPLEMENTED)"
  | DEVICE_TYPE_MISMATCH   -> "Cairo.Error(DEVICE_TYPE_MISMATCH)"
  | DEVICE_ERROR           -> "Cairo.Error(DEVICE_ERROR)"
  | INVALID_MESH_CONSTRUCTION -> "Cairo.Error(INVALID_MESH_CONSTRUCTION)"
  | DEVICE_FINISHED        -> "Cairo.Error(DEVICE_FINISHED)"
  | JBIG2_GLOBAL_MISSING   -> "Cairo.Error(JBIG2_GLOBAL_MISSING)"

let () =
  Printexc.register_printer (function
  | Error s -> Some(error_of_status s)
  | _ -> None)

external status_to_string  : status -> string = "caml_cairo_status_to_string"

exception Unavailable
let () = Callback.register_exception "Cairo.Unavailable" Unavailable

type context
type surface
type content = COLOR | ALPHA | COLOR_ALPHA
type 'a pattern
  constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]
type any_pattern = [`Solid | `Surface | `Gradient | `Linear | `Radial] pattern
type glyph = { index: int;  x: float;  y: float }

external create : surface -> context = "caml_cairo_create"
external save : context -> unit = "caml_cairo_save"
external restore : context -> unit = "caml_cairo_restore"

external get_target : context -> surface = "caml_cairo_get_target"

module Group =
struct
  external push_group : context -> unit = "caml_cairo_push_group"
  external push_group_with_content : context -> content -> unit
    = "caml_cairo_push_group_with_content"

  let push ?content cr =
    match content with
    | None -> push_group cr
    | Some c -> push_group_with_content cr c

  external pop : context -> any_pattern = "caml_cairo_pop_group"
  external pop_to_source : context -> unit = "caml_cairo_pop_group_to_source"

  external get_target : context -> surface = "caml_cairo_get_group_target"
end

external set_source_rgb : context -> float -> float -> float -> unit
  = "caml_cairo_set_source_rgb"

external set_source_rgba :
  context -> float -> float -> float -> float -> unit
  = "caml_cairo_set_source_rgba"

external set_source : context -> 'a pattern -> unit = "caml_cairo_set_source"

external set_source_surface : context -> surface -> x:float -> y:float -> unit
  = "caml_cairo_set_source_surface"

external get_source : context -> any_pattern = "caml_cairo_get_source"

type antialias =
  | ANTIALIAS_DEFAULT
  | ANTIALIAS_NONE
  | ANTIALIAS_GRAY
  | ANTIALIAS_SUBPIXEL

external set_antialias : context -> antialias -> unit
  = "caml_cairo_set_antialias"
external get_antialias : context -> antialias = "caml_cairo_get_antialias"

external set_dash_stub : context -> float array -> ofs:float -> unit
  = "caml_cairo_set_dash"

let set_dash cr ?(ofs=0.0) dashes = set_dash_stub cr dashes ~ofs

external get_dash : context -> float array * float = "caml_cairo_get_dash"

type fill_rule =
  | WINDING
  | EVEN_ODD

external set_fill_rule : context -> fill_rule -> unit
  = "caml_cairo_set_fill_rule"
external get_fill_rule : context -> fill_rule = "caml_cairo_get_fill_rule"

type line_cap =
  | BUTT
  | ROUND
  | SQUARE

external set_line_cap : context -> line_cap -> unit = "caml_cairo_set_line_cap"
external get_line_cap : context -> line_cap = "caml_cairo_get_line_cap"

type line_join =
  | JOIN_MITER
  | JOIN_ROUND
  | JOIN_BEVEL

external set_line_join : context -> line_join -> unit
  = "caml_cairo_set_line_join"
external get_line_join : context -> line_join = "caml_cairo_get_line_join"

external set_line_width : context -> float -> unit = "caml_cairo_set_line_width"
external get_line_width : context -> float = "caml_cairo_get_line_width"

external set_miter_limit : context -> float -> unit
  = "caml_cairo_set_miter_limit"
external get_miter_limit : context -> float = "caml_cairo_get_miter_limit"

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

external set_operator : context -> operator -> unit = "caml_cairo_set_operator"
external get_operator : context -> operator = "caml_cairo_get_operator"

external set_tolerance : context -> float -> unit = "caml_cairo_set_tolerance"
external get_tolerance : context -> float = "caml_cairo_get_tolerance"

external clip : context -> unit = "caml_cairo_clip"
external clip_preserve : context -> unit = "caml_cairo_clip_preserve"

type rectangle = {
  x:float;
  y:float;
  w:float;
  h:float
}

external clip_extents : context -> rectangle = "caml_cairo_clip_extents"

external clip_reset : context -> unit = "caml_cairo_reset_clip"

external clip_rectangle_list : context -> rectangle list
  = "caml_cairo_copy_clip_rectangle_list"

external fill : context -> unit = "caml_cairo_fill"
external fill_preserve : context -> unit = "caml_cairo_fill_preserve"

external fill_extents : context -> rectangle = "caml_cairo_fill_extents"

external in_fill : context -> float -> float -> bool = "caml_cairo_in_fill"

external mask : context -> 'a pattern -> unit = "caml_cairo_mask"
external mask_surface : context -> surface -> x:float -> y:float -> unit
  = "caml_cairo_mask_surface"

external paint_stub : context -> unit = "caml_cairo_paint"
external paint_with_alpha : context -> float -> unit
  = "caml_cairo_paint_with_alpha"

let paint ?alpha cr =
  match alpha with
  | None -> paint_stub cr
  | Some a -> paint_with_alpha cr a

external stroke : context -> unit = "caml_cairo_stroke"
external stroke_preserve : context -> unit = "caml_cairo_stroke_preserve"

external stroke_extents : context -> rectangle = "caml_cairo_stroke_extents"

external in_stroke : context -> float -> float -> bool
  = "caml_cairo_in_stroke"

external copy_page : context -> unit = "caml_cairo_copy_page"
external show_page : context -> unit = "caml_cairo_show_page"

(* ---------------------------------------------------------------------- *)

type path_data =
  | MOVE_TO of float * float
  | LINE_TO of float * float
  | CURVE_TO of float * float * float * float * float * float
  | CLOSE_PATH

module Path =
struct
  type t

  external copy : context -> t = "caml_cairo_copy_path"
  external copy_flat : context -> t = "caml_cairo_copy_path_flat"
  external append : context -> t -> unit = "caml_cairo_append_path"
  external get_current_point : context -> float * float
    = "caml_cairo_get_current_point"
  external clear : context -> unit = "caml_cairo_new_path"
  external sub : context -> unit = "caml_cairo_new_sub_path"
  external close : context -> unit = "caml_cairo_close_path"

  external glyph : context -> glyph array -> unit = "caml_cairo_glyph_path"
  external text : context -> string -> unit = "caml_cairo_text_path"
  external extents : context -> rectangle = "caml_cairo_path_extents"

  external fold : t -> ('a -> path_data -> 'a) -> 'a -> 'a
    = "caml_cairo_path_fold"
  external to_array : t -> path_data array = "caml_cairo_path_to_array"
  external of_array : path_data array -> t = "caml_cairo_path_of_array"
end


external arc : context -> float -> float -> r:float -> a1:float -> a2:float
  -> unit = "caml_cairo_arc_bc" "caml_cairo_arc"
external arc_negative : context -> float -> float -> r:float -> a1:float ->
  a2:float -> unit = "caml_cairo_arc_negative_bc" "caml_cairo_arc_negative"

external curve_to : context -> float -> float -> float -> float ->
                    float -> float -> unit
  = "caml_cairo_curve_to_bc" "caml_cairo_curve_to"

external line_to : context -> float -> float -> unit = "caml_cairo_line_to"
external move_to : context -> float -> float -> unit = "caml_cairo_move_to"
external rectangle :
  context -> float -> float -> w:float -> h:float -> unit
  = "caml_cairo_rectangle"

external rel_curve_to : context -> float -> float ->
                        float -> float -> float -> float -> unit
  = "caml_cairo_rel_curve_to_bc" "caml_cairo_rel_curve_to"

external rel_line_to : context -> float -> float -> unit
  = "caml_cairo_rel_line_to"
external rel_move_to : context -> float -> float -> unit
  = "caml_cairo_rel_move_to"


(* ---------------------------------------------------------------------- *)

type matrix = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float }

module Matrix =
struct
  type t = matrix
      (*     x_new = xx *. x +. xy *. y +. x0;
             y_new = yx *. x +. yy *. y +. y0;  *)

  let init_identity () = { xx=1.; yx=0.; xy=0.; yy=1.; x0=0.; y0=0. }

  let init_translate x y =
    { xx=1.; yx=0.; xy=0.; yy=1.;  x0=x;  y0=y }

  let init_scale x y =
    { xx=x; yx=0.; xy=0.; yy=y;  x0=0.;  y0=0. }

  let init_rotate angle =
    { xx=cos(angle);    yx=sin(angle);
      xy= -. sin(angle); yy=cos(angle);  x0=0.;  y0=0. }

  let translate m x y =
    m.x0 <- m.x0 +. m.xx *. x +. m.xy *. y;
    m.y0 <- m.y0 +. m.yx *. x +. m.yy *. y

  let scale m x y =
    m.xx <- m.xx *. x;
    m.yx <- m.yx *. x;
    m.xy <- m.xy *. y;
    m.yy <- m.yy *. y

  let rotate m angle =
    let cosa = cos angle and sina = sin angle in
    let xx = m.xx in
    m.xx <- xx *. cosa +. m.xy *. sina;
    m.xy <- m.xy *. cosa -. xx *. sina;
    let yx = m.yx in
    m.yx <- yx *. cosa +. m.yy *. sina;
    m.yy <- m.yy *. cosa -. yx *. sina

  let invert m =
    (* Optimize for scaling|translation matrices just like cairo... *)
    if m.xy = 0. && m.yx = 0. then (
      m.x0 <- -. m.x0;
      m.y0 <- -. m.y0;
      if m.xx <> 1. then (
        if m.xx = 0. then raise(Error INVALID_MATRIX);
        m.xx <- 1. /. m.xx;
        m.x0 <- m.x0 *. m.xx;
      );
      if m.yy <> 1. then (
        if m.yy = 0. then raise(Error INVALID_MATRIX);
        m.yy <- 1. /. m.yy;
        m.y0 <- m.y0 *. m.yy;
      );
    )
    else
      let det = m.xx *. m.yy -. m.yx *. m.xy in
      if det = 0. || 1. /. det = 0. (* infinite det *) then
        raise(Error INVALID_MATRIX);
      let yy = m.xx /. det in
      m.xx <- m.yy /. det;
      m.xy <- -. m.xy /. det;
      m.yx <- -. m.yx /. det;
      m.yy <- yy;
      let y0 = -. m.yx *. m.x0 -. yy *. m.y0 in
      m.x0 <- -. m.xx *. m.x0 -. m.xy *. m.y0;
      m.y0 <- y0

  let multiply a b =
    { xx = b.xx *. a.xx +. b.xy *. a.yx;
      xy = b.xx *. a.xy +. b.xy *. a.yy;
      yx = b.yx *. a.xx +. b.yy *. a.yx;
      yy = b.yx *. a.xy +. b.yy *. a.yy;
      x0 = b.xx *. a.x0 +. b.xy *. a.y0 +. b.x0;
      y0 = b.yx *. a.x0 +. b.yy *. a.y0 +. b.y0; }

  let transform_distance m ~dx ~dy =
    (m.xx *. dx +. m.xy *. dy,  m.yx *. dx +. m.yy *. dy)

  let transform_point m x y =
    (m.xx *. x +. m.xy *. y +. m.x0,  m.yx *. x +. m.yy *. y +. m.y0)

end

(* ---------------------------------------------------------------------- *)
(* Rendering text and glyphs *)

type text_extents = {
  x_bearing : float;
  y_bearing : float;
  width : float;
  height : float;
  x_advance : float;
  y_advance : float;
}

type subpixel_order =
  | SUBPIXEL_ORDER_DEFAULT
  | SUBPIXEL_ORDER_RGB
  | SUBPIXEL_ORDER_BGR
  | SUBPIXEL_ORDER_VRGB
  | SUBPIXEL_ORDER_VBGR

type hint_style =
  | HINT_STYLE_DEFAULT
  | HINT_STYLE_NONE
  | HINT_STYLE_SLIGHT
  | HINT_STYLE_MEDIUM
  | HINT_STYLE_FULL

type hint_metrics =
  | HINT_METRICS_DEFAULT
  | HINT_METRICS_OFF
  | HINT_METRICS_ON

module Font_options =
struct
  type t

  external set : context -> t -> unit = "caml_cairo_set_font_options"
  external get : context -> t = "caml_cairo_get_font_options"
  external create : unit -> t = "caml_cairo_font_options_create"
  external copy : t -> t = "caml_cairo_font_options_copy"
  external merge : t -> t -> unit = "caml_cairo_font_options_merge"
  external set_antialias : t -> antialias -> unit
    = "caml_cairo_font_options_set_antialias"
  external get_antialias : t -> antialias
    = "caml_cairo_font_options_get_antialias"
  external set_subpixel_order : t -> subpixel_order -> unit
    = "caml_cairo_font_options_set_subpixel_order"
  external get_subpixel_order : t -> subpixel_order
    = "caml_cairo_font_options_get_subpixel_order"
  external set_hint_style : t -> hint_style -> unit
    = "caml_cairo_font_options_set_hint_style"
  external get_hint_style : t -> hint_style
    = "caml_cairo_font_options_get_hint_style"
  external set_hint_metrics : t -> hint_metrics -> unit
    = "caml_cairo_font_options_set_hint_metrics"
  external get_hint_metrics : t -> hint_metrics
    = "caml_cairo_font_options_get_hint_metrics"

  let make ?(antialias=ANTIALIAS_DEFAULT)
      ?(subpixel_order=SUBPIXEL_ORDER_DEFAULT)
      ?(hint_style=HINT_STYLE_DEFAULT) ?(hint_metrics=HINT_METRICS_DEFAULT) () =
    let fo = create() in
    set_antialias fo antialias;
    set_subpixel_order fo subpixel_order;
    set_hint_style fo hint_style;
    set_hint_metrics fo hint_metrics;
    fo
end

type slant = Upright | Italic | Oblique
type weight = Normal | Bold
type font_type =
    [ `Toy
    | `Ft
    | `Win32
    | `Quartz
    | `User
    ]

external font_type_init : unit -> unit = "caml_cairo_font_type_init" [@@noalloc]
let () = font_type_init()

module Font_face =
struct
  type 'a t

  external set : context -> _ t -> unit = "caml_cairo_set_font_face"
  external get : context -> font_type t = "caml_cairo_get_font_face"

  external get_type : 'a t -> font_type = "caml_cairo_font_face_get_type"

  external create_stub : family:string -> slant -> weight -> [`Toy] t
    = "caml_cairo_toy_font_face_create"

  let create ?(family="") slant weight =
    create_stub ~family slant weight

  external get_family : [`Toy] t -> string
    = "caml_cairo_toy_font_face_get_family"
  external get_slant : [`Toy] t -> slant
    = "caml_cairo_toy_font_face_get_slant"
  external get_weight : [`Toy] t -> weight
    = "caml_cairo_toy_font_face_get_weight"
end

module Glyph =
struct
(*   type array (\* FIXME: abstract type for cairo_glyph_t* ? *\) *)
  type t = glyph = { index: int;  x: float;  y: float }

  type cluster = {
    num_bytes : int;
    num_glyphs : int;
  }

  type cluster_flags =
    | BACKWARD

  external extents : context -> t array -> text_extents
    = "caml_cairo_glyph_extents"
  external show : context -> t array -> unit = "caml_cairo_show_glyphs"
  external show_text : context -> string -> t array ->
    cluster array -> cluster_flags -> unit = "caml_cairo_show_text_glyphs"
end

type font_extents = {
  ascent : float;
  descent : float;
  baseline : float;
  max_x_advance : float;
  max_y_advance : float;
}

module Scaled_font =
struct
  type 'a t

  external set : context -> _ t -> unit = "caml_cairo_set_scaled_font"
  external get : context -> _ t = "caml_cairo_get_scaled_font"

  external create : 'a Font_face.t -> Matrix.t -> Matrix.t -> Font_options.t
    -> 'a t = "caml_cairo_scaled_font_create"


  external extents : _ t -> font_extents = "caml_cairo_scaled_font_extents"

  external text_extents : _ t -> string -> text_extents
    = "caml_cairo_scaled_font_text_extents"
  external glyph_extents : _ t -> Glyph.t array -> text_extents
    = "caml_cairo_scaled_font_glyph_extents"

  external text_to_glyphs : _ t -> x:float -> y:float -> string
    -> Glyph.t array * Glyph.cluster array * Glyph.cluster_flags
    = "caml_cairo_scaled_font_text_to_glyphs"

  external get_font_face : 'a t -> 'a Font_face.t
    = "caml_cairo_scaled_font_get_font_face"
  external get_font_options : _ t -> Font_options.t
    = "caml_cairo_scaled_font_get_font_options"

  external get_font_matrix : _ t -> Matrix.t
    = "caml_cairo_scaled_font_get_font_matrix"
  external get_ctm : _ t -> Matrix.t = "caml_cairo_scaled_font_get_ctm"
  external get_scale_matrix : _ t -> Matrix.t
    = "caml_cairo_scaled_font_get_scale_matrix"

  external get_type : _ t -> font_type = "caml_cairo_scaled_font_get_type"
end

module Ft = struct
  type face

  type library

  let ft_library = ref None
  (* FIXME: is it important to have to possibility to create more than
     one library resource? *)

  external init_freetype : unit -> library = "caml_cairo_Ft_init_FreeType"

  let get_ft_library () = match !ft_library with
    | None -> let ft = init_freetype() in
              ft_library := Some ft;
              ft
    | Some ft -> ft

  external new_face : library -> string -> int -> face
    = "caml_cairo_Ft_new_face"

  let face ?library ?(index=0) pathname =
    let ft = match library with
      | Some l -> l
      | None -> get_ft_library() in
    new_face ft pathname index

  external create_for_ft_face_ :
    face -> vertical:bool -> autohint:bool -> [`Ft] Font_face.t
    = "caml_cairo_ft_create_for_ft_face"

  type flag = [`Vertical_layout | `Force_autohint]

  let create_for_ft_face ?(flags=[]) face =
    let vertical = ref false in
    let autohint = ref false in
    List.iter (function `Vertical_layout -> vertical := true
                      | `Force_autohint -> autohint := true) flags;
    create_for_ft_face_ face ~vertical:!vertical ~autohint:!autohint

  external create_for_pattern :
    ?options:Font_options.t -> string -> [`Ft] Font_face.t
    = "caml_cairo_ft_create_for_pattern"

  external scaled_font_lock_face : [`Ft] Scaled_font.t -> face
    = "caml_cairo_ft_scaled_font_lock_face"
  external scaled_font_unlock_face : [`Ft] Scaled_font.t -> unit
    = "caml_cairo_ft_scaled_font_unlock_face"

  module Synthesize = struct
    type t = { bold: bool;
               oblique: bool }

    external get : [`Ft] Font_face.t -> t
      = "caml_cairo_ft_synthesize_get"
    external set_ : [`Ft] Font_face.t -> bold:bool -> oblique:bool -> unit
      = "caml_cairo_ft_synthesize_set"
    external unset_ : [`Ft] Font_face.t -> bold:bool -> oblique:bool -> unit
      = "caml_cairo_ft_synthesize_unset"

    let set ?(bold=false) ?(oblique=false) ff = set_ ff ~bold ~oblique

    let unset ?(bold=false) ?(oblique=false) ff = unset_ ff ~bold ~oblique
  end
end


external select_font_face : context -> slant -> weight -> string -> unit
  = "caml_cairo_select_font_face"

let select_font_face cr ?(slant=Upright) ?(weight=Normal) family =
  select_font_face cr slant weight family

external set_font_size : context -> float -> unit
  = "caml_cairo_set_font_size"

external set_font_matrix : context -> Matrix.t -> unit
  = "caml_cairo_set_font_matrix"

external get_font_matrix : context -> Matrix.t = "caml_cairo_get_font_matrix"

external show_text : context -> string -> unit = "caml_cairo_show_text"

external font_extents : context -> font_extents
  = "caml_cairo_font_extents"

external text_extents : context -> string -> text_extents
  = "caml_cairo_text_extents"

(* ---------------------------------------------------------------------- *)

module Surface =
struct
  type t = surface

  external create_similar : t -> content -> w:int -> h:int -> t
    = "caml_cairo_surface_create_similar"
  external finish : t -> unit = "caml_cairo_surface_finish"
  external flush : t -> unit = "caml_cairo_surface_flush"
  external get_font_options : t -> Font_options.t
    = "caml_cairo_surface_get_font_options"
  external get_content : t -> content = "caml_cairo_surface_get_content"
  external mark_dirty : t -> unit = "caml_cairo_surface_mark_dirty"
  external mark_dirty_rectangle : t -> int -> int -> w:int -> h:int -> unit
    = "caml_cairo_surface_mark_dirty_rectangle"
  external set_device_offset : t -> float -> float -> unit
    = "caml_cairo_surface_set_device_offset"
  external get_device_offset : t -> float * float
    = "caml_cairo_surface_get_device_offset"
  external set_fallback_resolution : t -> x:float -> y:float -> unit
    = "caml_cairo_surface_set_fallback_resolution"
  external get_fallback_resolution : t -> float * float
    = "caml_cairo_surface_get_fallback_resolution"

  type kind =
      [ `Image
      | `PDF
      | `PS
      | `XLib
      | `XCB
      | `GLITZ
      | `Quartz
      | `Win32
      | `BEOS
      | `DirectFB
      | `SVG
      | `OS2
      | `Win32_printing
      | `Quartz_image
      | `Recording
      ]
  external init : unit -> unit = "caml_cairo_surface_kind_init"
  let () = init()

  external get_type : t -> kind = "caml_cairo_surface_get_type"

  external copy_page : t -> unit = "caml_cairo_surface_copy_page"
  external show_page : t -> unit = "caml_cairo_surface_show_page"
  external has_show_text_glyphs : t -> bool
    = "caml_cairo_surface_has_show_text_glyphs"
end

module Image =
struct
  type format =
    | ARGB32
    | RGB24
    | A8
    | A1

  external create : format -> w:int -> h:int -> Surface.t
    = "caml_cairo_image_surface_create"

  external get_format : Surface.t -> format
    = "caml_cairo_image_surface_get_format"
  external get_width : Surface.t -> int = "caml_cairo_image_surface_get_width"
  external get_height : Surface.t -> int = "caml_cairo_image_surface_get_height"
  external get_stride : Surface.t -> int = "caml_cairo_image_surface_get_stride"

  external stride_for_width : format -> int -> int
    = "caml_cairo_format_stride_for_width" [@@noalloc]

  open Bigarray
  type data8 =
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type data32 =
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

  (* These direct bindings assume that the bigarray is large enough *)
  external create_for_data8_unsafe : data8 ->
    format -> w:int -> h:int -> stride:int -> Surface.t
    = "caml_cairo_image_surface_create_for_data8"
  external create_for_data32_unsafe : data32 ->
    format -> w:int -> h:int -> stride:int -> Surface.t
    = "caml_cairo_image_surface_create_for_data32"

  let create_for_data8 data ?stride format ~w ~h =
    if w <= 0 then invalid_arg "Cairo.Image.create_for_data8: width <= 0";
    if h <= 0 then invalid_arg "Cairo.Image.create_for_data8: height <= 0";
    let stride = match stride with
      | None -> stride_for_width format w
      | Some s ->
          if s < w (* thus if s <= 0 *) then raise(Error INVALID_STRIDE);
          s in
    if stride * h > Array1.dim data then
      invalid_arg(Printf.sprintf "Cairo.Image.create_for_data8: bigarray too \
	small for the required stride=%i and height=%i" stride h);
    create_for_data8_unsafe data format ~w ~h ~stride

  let create_for_data32 ?w ?h ?(alpha=true) data =
    let width = match w with
      | None -> Array2.dim2 data
      | Some w ->
          if w < 0 then
            invalid_arg "Cairo.Image.create_for_data32: width < 0";
          if w > Array2.dim2 data then
            invalid_arg "Cairo.Image.create_for_data32: given width too large";
          w in
    let height = match h with
      | None -> Array2.dim1 data
      | Some h ->
          if h < 0 then
            invalid_arg "Cairo.Image.create_for_data32: height < 0";
          if h > Array2.dim1 data then
            invalid_arg "Cairo.Image.create_for_data32: given height too large";
          h in
    let format = if alpha then ARGB32 else RGB24 in
    (* Both format use 32 bits = 4 bytes *)
    create_for_data32_unsafe data format ~w:width ~h:height
      ~stride:(4 * Array2.dim2 data)

  external get_data8 : Surface.t -> (int, int8_unsigned_elt, c_layout) Array1.t
    = "caml_cairo_image_surface_get_UINT8"
  external get_data32 : Surface.t -> (int32, int32_elt, c_layout) Array2.t
    = "caml_cairo_image_surface_get_INT32"

  let get_data32 surface =
    let format = get_format surface in
    if format <> ARGB32 && format <> RGB24 then
      invalid_arg "Cairo.Image.get_data32: image format must be \
		   ARGB32 or RGB24";
    get_data32 surface

  let output_ppm fh ?w ?h (data: data32) =
    let width = match w with
      | None -> Array2.dim1 data
      | Some w ->
          if w > Array2.dim1 data then
            invalid_arg "Cairo.Image.output_ppm: width > Array2.dim1 data";
          if w <= 0 then
            invalid_arg "Cairo.Image.output_ppm: width <= 0";
          w in
    let height = match h with
      | None -> Array2.dim2 data
      | Some h ->
          if h > Array2.dim2 data then
            invalid_arg "Cairo.Image.output_ppm: height > Array2.dim2 data";
          if h <= 0 then
            invalid_arg "Cairo.Image.output_ppm: height <= 0";
          h in
    Printf.fprintf fh "P6 %d %d 255\n" width height;
    for i = 0 to width - 1 do
      for j = 0 to height - 1 do
        (* Output pixel RGB *)
        let p = Int32.to_int data.{i, j} in
        output_byte fh ((p lsr 16) land 0xFF);
        output_byte fh ((p lsr 8) land 0xFF);
        output_byte fh (p land 0xFF)
      done
    done
      (* flush fh ?? *)
end

module PDF =
struct
  external create_for_stream : (string -> unit) ->
                               w:float -> h:float -> Surface.t
    = "caml_cairo_pdf_surface_create_for_stream"

  external create : string -> w:float -> h:float -> Surface.t
    = "caml_cairo_pdf_surface_create"
    (* Do we want to implement it in terms of [create_for_stream]?
       The "problem" is the absence of close function... *)

  external set_size : Surface.t -> w:float -> h:float -> unit
    = "caml_cairo_pdf_surface_set_size" [@@noalloc]
end

module PNG =
struct
  external create : string -> Surface.t
    = "caml_cairo_image_surface_create_from_png"

  external create_from_stream : input:(string -> int -> unit) -> Surface.t
    = "caml_cairo_image_surface_create_from_png_stream"
    (* FIXME: must hold the input function to avoid it is being
       reclaimed before the surface? *)

  external write : Surface.t -> string -> unit
    = "caml_cairo_surface_write_to_png"
  external write_to_stream : Surface.t -> (string -> unit) -> unit
    = "caml_cairo_surface_write_to_png_stream"
end

module PS =
struct
  external create_for_stream : (string -> unit) ->
    w:float -> h:float -> Surface.t
    = "caml_cairo_ps_surface_create_for_stream"
  external create : string -> w:float -> h:float -> Surface.t
    = "caml_cairo_ps_surface_create"

  type level = LEVEL_2 | LEVEL_3

  external restrict_to_level : Surface.t -> level -> unit
    = "caml_cairo_ps_surface_restrict_to_level"
  external get_levels : unit -> level list
    = "caml_cairo_ps_get_levels"
  external level_to_string : level -> string = "caml_cairo_ps_level_to_string"
  external set_eps : Surface.t -> eps:bool -> unit
    = "caml_cairo_ps_surface_set_eps"
  external get_eps : Surface.t -> bool = "caml_cairo_ps_surface_get_eps"

  external set_size : Surface.t -> w:float -> h:float -> unit
    = "caml_cairo_ps_surface_set_size"

  module Dsc =
  struct
    external begin_setup : Surface.t -> unit
      = "caml_cairo_ps_surface_dsc_begin_setup"
    external begin_page_setup : Surface.t -> unit
      = "caml_cairo_ps_surface_dsc_begin_page_setup"
    external comment : Surface.t -> string -> unit
      = "caml_cairo_ps_surface_dsc_comment"
  end
end

module SVG =
struct
  external create : string -> w:float -> h:float -> Surface.t
    = "caml_cairo_svg_surface_create"

  external create_for_stream : (string -> unit) ->
    w:float -> h:float -> Surface.t
    = "caml_cairo_svg_surface_create_for_stream"

  type version = VERSION_1_1 | VERSION_1_2

  external restrict_to_version : Surface.t -> version -> unit
    = "caml_cairo_svg_surface_restrict_to_version"
  external get_versions : unit -> version list
    = "caml_cairo_svg_get_versions"
  external version_to_string : version -> string
    = "caml_cairo_svg_version_to_string"
end

module Recording =
struct
  external create : ?extents:rectangle -> content -> Surface.t
    = "caml_cairo_recording_surface_create"

  external ink_extents : Surface.t -> rectangle
    = "caml_cairo_recording_surface_ink_extents"
end


(* ---------------------------------------------------------------------- *)

module Pattern =
struct
  type 'a t = 'a pattern
  type any = any_pattern

  external add_color_stop_rgb_stub : [> `Gradient] t -> ofs:float ->
                                     float -> float -> float -> unit
    = "caml_cairo_pattern_add_color_stop_rgb" [@@noalloc]

  let add_color_stop_rgb cr ?(ofs=0.0) r g b =
    add_color_stop_rgb_stub cr ~ofs r g b

  external add_color_stop_rgba_stub : [> `Gradient] t -> ofs:float ->
                                      float -> float -> float -> float -> unit
    = "caml_cairo_pattern_add_color_stop_rgba_bc"
    "caml_cairo_pattern_add_color_stop_rgba" [@@noalloc]

  let add_color_stop_rgba cr ?(ofs=0.0) r g b a =
    add_color_stop_rgba_stub cr ~ofs r g b a

  external get_color_stop_count : [> `Gradient] t -> int
    = "caml_cairo_pattern_get_color_stop_count"

  external get_color_stop_rgba : [> `Gradient] t -> idx:int ->
    float * float * float * float * float
    = "caml_cairo_pattern_get_color_stop_rgba"
    (* FIXME: do we want to iterate over the colors instead ?? *)

  external create_rgb : float -> float -> float -> [`Solid] t
    = "caml_cairo_pattern_create_rgb"

  external create_rgba : float -> float -> float -> float -> [`Solid] t
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
    = "caml_cairo_pattern_set_extend" [@@noalloc]

  external get_extend : 'a t -> extend = "caml_cairo_pattern_get_extend"

  type filter =
    | FAST
    | GOOD
    | BEST
    | NEAREST
    | BILINEAR
    (* | GAUSSIAN *)

  external set_filter : 'a t -> filter -> unit
    = "caml_cairo_pattern_set_filter" [@@noalloc]

  external get_filter : 'a t -> filter = "caml_cairo_pattern_get_filter"

  external set_matrix : 'a t -> Matrix.t -> unit
    = "caml_cairo_pattern_set_matrix" [@@noalloc]

  external get_matrix : 'a t -> Matrix.t = "caml_cairo_pattern_get_matrix"
end

(* ---------------------------------------------------------------------- *)
(* Transformations - Manipulating the current transformation matrix  *)

external translate : context -> float -> float -> unit
  = "caml_cairo_translate"
external scale : context -> float -> float -> unit = "caml_cairo_scale"
external rotate : context -> float -> unit = "caml_cairo_rotate"

external transform : context -> Matrix.t -> unit
  = "caml_cairo_transform" [@@noalloc]
external set_matrix : context -> Matrix.t -> unit
  = "caml_cairo_set_matrix" [@@noalloc]

external get_matrix : context -> Matrix.t = "caml_cairo_get_matrix"

external identity_matrix : context -> unit = "caml_cairo_identity_matrix"

external user_to_device : context -> float -> float -> float * float
  = "caml_cairo_user_to_device"
external user_to_device_distance :
  context -> float -> float -> float * float
  = "caml_cairo_user_to_device_distance"
external device_to_user : context -> float -> float -> float * float
  = "caml_cairo_device_to_user"
external device_to_user_distance :
  context -> float -> float -> float * float
  = "caml_cairo_device_to_user_distance"
