(* File: cairo.mli

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
  (** [Error status]: raised by functions of this module to indicate
      a cause of failure. *)

external status_to_string  : status -> string = "caml_cairo_status_to_string"
    (** Provides a human-readable description of a status. *)

(* ---------------------------------------------------------------------- *)

module Surface :
sig
  type t

  type content = COLOR | ALPHA | COLOR_ALPHA
end

(* ---------------------------------------------------------------------- *)

module Pattern :
sig
  type t


end

(* ---------------------------------------------------------------------- *)
(** {2 Cairo_t} *)

type t
  (** The cairo drawing context.  This is the main object used when
      drawing with cairo.  To draw with cairo, you create a [t], set
      the target surface, and drawing options for the [t], create
      shapes with functions like {!Cairo.move_to} and
      {!Cairo.line_to}, and then draw shapes with {!Cairo.stroke} or
      {!Cairo.fill}.  *)

external create : Surface.t -> t = "caml_cairo_create"
 (** [create target] creates a new context with all graphics state
     parameters set to default values and with [target] as a target
     surface. The target surface should be constructed with a
     backend-specific function such as {!Cairo.Image.surface_create}
     (or any other [..._surface_create] variant).

     @raise Out_of_memory if the context could not be allocated. *)

external save : t -> unit = "caml_cairo_save"
  (** [save cr] makes a copy of the current state of [cr] and saves it
      on an internal stack of saved states for [cr].  When [restore]
      is called, [cr] will be restored to the saved state.  Multiple
      calls to [save] and [restore] can be nested; each call to
      [restore] restores the state from the matching paired [save].  *)
external restore : t -> unit = "caml_cairo_restore"
  (** [restore cr] restores [cr] to the state saved by a preceding
      call to [save] and removes that state from the stack of saved
      states. *)

external get_target : t -> surface = "caml_cairo_get_target"
  (** Gets the target surface for the cairo context as passed to [create]. *)

module Group :
sig
  val push : ?content:Surface.content -> t -> unit
    (** Temporarily redirects drawing to an intermediate surface known
        as a group.  The redirection lasts until the group is completed
        by a call to {!Cairo.pop_group} or {!Cairo.pop_group_to_source}.
        These calls provide the result of any drawing to the group as a
        pattern, (either as an explicit object, or set as the source
        pattern).

        This group functionality can be convenient for performing
        intermediate compositing. One common use of a group is to render
        objects as opaque within the group, (so that they occlude each
        other), and then blend the result with translucence onto the
        destination.

        Groups can be nested arbitrarily deep by making balanced calls
        to [Group.push]/[Group.pop]. Each call pushes/pops the new
        target group onto/from a stack.

        The [Group.push] function calls [save] so that any changes
        to the graphics state will not be visible outside the group,
        (the [Group.pop] function call [restore]).

        @param content The content type of the group.  By default the
        intermediate group will have a content type of
        CAIRO_CONTENT_COLOR_ALPHA.  *)

  external pop : t -> Pattern.t = "caml_cairo_pop_group"
    (** Terminates the redirection begun by a call to
        {!Cairo.Group.push} and returns a new pattern containing the
        results of all drawing operations performed to the group.

        The [Group.pop] function calls {!Cairo.restore}, (balancing a
        call to {!Cairo.save} by the [Group.push] function), so that any
        changes to the graphics state will not be visible outside the
        group.

        @return a newly created (surface) pattern containing the results
        of all drawing operations performed to the group.  *)

  external pop_to_source : t -> unit = "caml_cairo_pop_group_to_source"
    (** Terminates the redirection begun by a call to [Group.push] and
        installs the resulting pattern as the source pattern in the
        given cairo context.

        The behavior of this function is equivalent to the sequence of
        operations:
        {[
        let group = Cairo.Group.pop cr in
        Cairo.set_source cr group;
        ]}
    *)

  external get_target : t -> Surface.t = "caml_cairo_get_group_target"
    (** Gets the current destination surface for the context.  This is
        either the original target surface as passed to [create] or
        the target surface for the current group as started by the most
        recent call to [Group.push]. *)
end

external set_source_rgb : t -> r:float -> g:float -> b:float -> unit
  = "caml_cairo_set_source_rgb"
  (** [set_source_rgb cr r g b] sets the source pattern within [cr] to
      an opaque color.  This opaque color will then be used for any
      subsequent drawing operation until a new source pattern is set.

      The color components are floating point numbers in the range 0 to
      1. If the values passed in are outside that range, they will be
      clamped.

      The default source pattern is opaque black, (that is, it is
      equivalent to [set_source_rgb cr 0. 0. 0.]). *)

external set_source_rgba : t -> r:float -> g:float -> b:float -> a:float -> unit
  = "caml_cairo_set_source_rgba"
  (** [set_source_rgba cr r g b a] sets the source pattern within [cr]
      to a translucent color.  This color will then be used for any
      subsequent drawing operation until a new source pattern is set.

      The color and alpha components are floating point numbers in the
      range 0 to 1. If the values passed in are outside that range,
      they will be clamped.

      The default source pattern is opaque black, (that is, it is
      equivalent to [set_source_rgba cr 0. 0. 0. 1.0]). *)

external set_source : t -> Pattern.t -> unit = "caml_cairo_set_source"
  (** [set_source cr source] sets the source pattern within [cr] to
      [source].  This pattern will then be used for any subsequent
      drawing operation until a new source pattern is set.

      Note: The pattern's transformation matrix will be locked to the
      user space in effect at the time of [set_source].  This means
      that further modifications of the current transformation matrix
      will not affect the source pattern. See {!Pattern.set_matrix}.

      The default source pattern is a solid pattern that is opaque
      black (that is, it is equivalent to [set_source_rgb cr 0. 0. 0.]). *)

val set_source_surface : t -> Surface.t -> x:float -> y:float -> unit
  (** [set_source_surface cr surface x y] is a convenience for
      creating a pattern from surface and setting it as the source in [cr]
      with [set_source].

      The [x] and [y] parameters give the user-space coordinate at
      which the surface origin should appear.  (The surface origin is
      its upper-left corner before any transformation has been
      applied.)  The x and y patterns are negated and then set as
      translation values in the pattern matrix.

      Other than the initial translation pattern matrix, as described
      above, all other pattern attributes, (such as its extend mode),
      are set to the default values as in
      {!Pattern.create_for_surface}.  The resulting pattern can be
      queried with {!Cairo.get_source} so that these attributes can be
      modified if desired (e.g. to create a repeating pattern with
      {!Cairo.Pattern.set_extend}). *)

val get_source : t -> Pattern.t
  (** [get_source cr] gets the current source pattern for [cr]. *)

(** Specifies the type of antialiasing to do when rendering text or shapes. *)
type antialias =
  | ANTIALIAS_DEFAULT  (** Use the default antialiasing for the
                           subsystem and target device *)
  | ANTIALIAS_NONE     (** Use a bilevel alpha mask  *)
  | ANTIALIAS_GRAY     (** Perform single-color antialiasing (using
                           shades of gray for black text on a white
                           background, for example). *)
  | ANTIALIAS_SUBPIXEL (** Perform antialiasing by taking advantage of
                           the order of subpixel elements on devices
                           such as LCD panels *)

external set_antialias : t -> antialias -> unit = "caml_cairo_set_antialias"
    (** Set the antialiasing mode of the rasterizer used for drawing
        shapes.  This value is a hint, and a particular backend may or
        may not support a particular value.  At the current time, no
        backend supports [ANTIALIAS_SUBPIXEL] when drawing shapes.

        Note that this option does not affect text rendering, instead
        see {!Cairo.Font.set_antialias}. *)

external get_antialias : t -> antialias = "caml_cairo_get_antialias"
  (** Gets the current shape antialiasing mode, as set by
      {!Cairo.set_antialias}. *)

val set_dash : t -> ?ofs:float -> float array -> unit
  (** [set_dash cr dashes] sets the dash pattern to be used by
      {!Cairo.stroke}.  A dash pattern is specified by dashes, an
      array of positive values.  Each value provides the length of
      alternate "on" and "off" portions of the stroke.  The offset
      [ofs] specifies an offset into the pattern at which the stroke
      begins (default: [0.]).

      [set_dash [| |]] disable dashing.  [set_dash [|l|]] sets a
      symmetric pattern with alternating on and off portions of the
      size [l].

      Each "on" segment will have caps applied as if the segment were
      a separate sub-path. In particular, it is valid to use an "on"
      length of 0.0 with {!Cairo.line_cap} being [ROUND] or [SQUARE]
      in order to distributed dots or squares along a path.

      Note: The length values are in user-space units as evaluated at
      the time of stroking.  This is not necessarily the same as the
      user space at the time of [set_dash].  *)

external get_dash : t -> float array * float = "caml_cairo_get_dash"
  (** Gets the current dash array ([( [| |], 0.)] if dashing is not
      currently in effect). *)


(** Used to select how paths are filled. For both fill rules, whether
    or not a point is included in the fill is determined by taking a
    ray from that point to infinity and looking at intersections with the
    path. The ray can be in any direction, as long as it doesn't pass
    through the end point of a segment or have a tricky intersection
    such as intersecting tangent to the path. (Note that filling is
    not actually implemented in this way. This is just a description
    of the rule that is applied.)

    The default fill rule is [WINDING].   *)
type fill_rule =
  | WINDING  (** If the path crosses the ray from left-to-right,
                 counts +1. If the path crosses the ray from right to
                 left, counts -1. (Left and right are determined from
                 the perspective of looking along the ray from the
                 starting point.) If the total count is non-zero, the
                 point will be filled.  *)
  | EVEN_ODD (** Counts the total number of intersections, without
                 regard to the orientation of the contour. If the
                 total number of intersections is odd, the point will
                 be filled. *)

val set_fill_rule : t -> fill_rule -> unit
val get_fill_rule : t -> fill_rule

(** Specifies how to render the endpoints of the path when stroking.
    The default line cap style is [BUTT].  *)
type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

val set_line_cap : t -> line_cap -> unit
val get_line_cap : t -> line_cap

(** Specifies how to render the junction of two lines when stroking.
    The default line join style is [MITER]. *)
type line_join =
  | MITER (** use a sharp (angled) corner, see {!Cairo.set_miter_limit} *)
  | ROUND (** use a rounded join, the center of the circle is the
              joint point *)
  | BEVEL (** use a cut-off join, the join is cut off at half the line
              width from the joint point *)

val set_line_join : t -> line_join -> unit
val get_line_join : t -> line_join

val set_line_width : t -> float -> unit
val get_line_width : t -> float

val set_miter_limit : t -> float -> unit
val get_miter_limit : t -> float

(** Compositing operator for all cairo drawing operations.

    The default operator is [Cairo.Operator.OVER].

    The operators marked as unbounded modify their destination even
    outside of the mask layer (that is, their effect is not bound by the
    mask layer). However, their effect can still be limited by way of
    clipping.

    To keep things simple, the operator descriptions here document
    the behavior for when both source and destination are either
    fully transparent or fully opaque. The actual implementation
    works for translucent layers too. For a more detailed
    explanation of the effects of each operator, including the
    mathematical definitions, see
    http://cairographics.org/operators/ *)
type operator =
  | CLEAR (** clear destination layer (bounded)  *)
  | SOURCE (** replace destination layer (bounded) *)
  | OVER (** draw source layer on top of destination layer (bounded) *)
  | IN  (** draw source where there was destination content (unbounded) *)
  | OUT (** draw source where there was no destination content (unbounded) *)
  | ATOP (** draw source on top of destination content and only there *)
  | DEST (** ignore the source *)
  | DEST_OVER (** draw destination on top of source *)
  | DEST_IN (** leave destination only where there was source content
                (unbounded) *)
  | DEST_OUT (** leave destination only where there was no source content *)
  | DEST_ATOP (** leave destination on top of source content and
                  only there (unbounded) *)
  | XOR (** source and destination are shown where there is only one
            of them *)
  | ADD (** source and destination layers are accumulated *)
  | SATURATE (** like over, but assuming source and dest are
                 disjoint geometries *)

val set_operator : t -> operator -> unit
val get_operator : t -> operator

val set_tolerance : t -> float -> unit
val get_tolerance : t -> float


val clip : ?preserve:bool -> t -> unit
  (** Establishes a new clip region by intersecting the current clip
      region with the current path as it would be filled by
      {!Cairo.fill} and according to the current fill rule (see
      {!Cairo.set_fill_rule}).

      After [clip], the current path will be cleared from the cairo
      context unless [preserve] is [true] (default: [false]).

      Use {!Cairo.save} and {!Cairo.restore} around [clip] is a robust
      means of temporarily restricting the clip region.
  *)

val clip_extents : t -> float * float * float * float
  (** Computes a bounding box [(x1, y1, x2, y2)] in user coordinates
      covering the area inside the current clip.  *)

type rectangle = { x:float; y:float; width:float; height:float }

val clip_rectangle_list : t -> rectangle list


val fill : ?preserve:bool -> t -> unit

val fill_extents : t -> float * float * float * float

val in_fill : t -> x:float -> y:float -> bool
  (** Tests whether the given point is inside the area that would be
      affected by a [fill] operation given the current path and
      filling parameters.  Surface dimensions and clipping are not
      taken into account.  *)

val mask : t -> Pattern.t -> unit
val mask_surface : t -> Surface.t -> x:float -> y:float -> unit

val paint : ?alpha:float -> t -> unit
  (** A drawing operator that paints the current source everywhere
      within the current clip region.  If [alpha] is set, the drawing
      is faded out using the alpha value.

      @param alpha  alpha value, between 0 (transparent) and 1 (opaque)  *)

val stroke : ?perserve:bool -> t -> unit
val stroke_extents : t -> float * float * float * float

val in_stroke : t -> x:float -> y:float -> bool


val copy_page : t -> unit
val show_page : t -> unit


(* set_user_data *)
(* get_user_data *)
