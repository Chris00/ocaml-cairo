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
(** {2 Cairo.t: The cairo drawing context} *)

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

external set_fill_rule : t -> fill_rule -> unit = "caml_cairo_set_fill_rule"
    (** [set_fill_rule cr fill_rule] sets the current fill rule within
        the cairo context [cr].  The fill rule is used to determine
        which regions are inside or outside a complex (potentially
        self-intersecting) path. The current fill rule affects both
        {!Cairo.fill} and {!Cairo.clip}.  See {!Cairo.fill_rule} for
        details on the semantics of each available fill rule.  *)

external get_fill_rule : t -> fill_rule = "caml_cairo_get_fill_rule"
    (** Gets the current fill rule, as set by [set_fill_rule]. *)


(** Specifies how to render the endpoints of the path when stroking.
    The default line cap style is [BUTT].  *)
type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

external set_line_cap : t -> line_cap -> unit = "caml_cairo_set_line_cap"
    (** [set_line_cap cr line_cap] sets the current line cap style
        within the cairo context [cr].  See {!Cairo.line_cap} for
        details about how the available line cap styles are drawn.

        As with the other stroke parameters, the current line cap
        style is examined by {!Cairo.stroke}, {!Cairo.stroke_extents},
        and {!Cairo.stroke_to_path}, but does not have any effect
        during path construction.

        The default line cap style is [BUTT].  *)
external get_line_cap : t -> line_cap = "caml_cairo_get_line_cap"
    (** Gets the current line cap style, as set by {!Cairo.set_line_cap}. *)


(** Specifies how to render the junction of two lines when stroking.
    The default line join style is [MITER]. *)
type line_join =
  | JOIN_MITER (** use a sharp (angled) corner, see {!Cairo.set_miter_limit} *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

external set_line_join : t -> line_join -> unit = "caml_cairo_set_line_join"
    (** Sets the current line join style within the cairo context.
        See {!Cairo.line_join} for details about how the available
        line join styles are drawn.

        As with the other stroke parameters, the current line join
        style is examined by {!Cairo.stroke}, {!Cairo.stroke_extents},
        and {!Cairo.stroke_to_path}, but does not have any effect
        during path construction.

        The default line join style is [MITER]. *)
external get_line_join : t -> line_join = "caml_cairo_get_line_join"
    (** Gets the current line join style, as set by {!Cairo.set_line_join}. *)


external set_line_width : t -> float -> unit = "caml_cairo_set_line_width"
    (** Sets the current line width within the cairo context. The line
        width value specifies the diameter of a pen that is circular in
        user space, (though device-space pen may be an ellipse in general
        due to scaling/shear/rotation of the CTM).

        Note: When the description above refers to user space and CTM
        it refers to the user space and CTM in effect at the time of
        the stroking operation, not the user space and CTM in effect
        at the time of the call to [set_line_width].  The simplest
        usage makes both of these spaces identical.  That is, if there
        is no change to the CTM between a call to [set_line_with] and
        the stroking operation, then one can just pass user-space
        values to [set_line_width] and ignore this note.

        As with the other stroke parameters, the current line width is
        examined by {!Cairo.stroke}, {!Cairo.stroke_extents}, and
        {!Cairo.stroke_to_path}, but does not have any effect during
        path construction.

        The default line width value is [2.0].  *)
external get_line_width : t -> float = "caml_cairo_get_line_width"
    (** This function returns the current line width value exactly as
        set by {!Cairo.set_line_width}.  Note that the value is
        unchanged even if the CTM has changed between the calls to
        [set_line_width] and [get_line_width]. *)

external set_miter_limit : t -> float -> unit = "caml_cairo_set_miter_limit"
    (** Sets the current miter limit within the cairo context.

        If the current line join style is set to [MITER] (see
        {!Cairo.set_line_join}), the miter limit is used to determine
        whether the lines should be joined with a bevel instead of a
        miter.  Cairo divides the length of the miter by the line
        width.  If the result is greater than the miter limit, the
        style is converted to a bevel.

        As with the other stroke parameters, the current line miter
        limit is examined by {!Cairo.stroke}, {!Cairo.stroke_extents},
        and {!Cairo.stroke_to_path}, but does not have any effect
        during path construction.

        The default miter limit value is [10.0], which will convert
        joins with interior angles less than 11 degrees to bevels
        instead of miters.  For reference, a miter limit of 2.0 makes
        the miter cutoff at 60 degrees, and a miter limit of 1.414
        makes the cutoff at 90 degrees.

        A miter limit for a desired angle can be computed as: miter
        limit = 1/sin(angle/2).  *)

external get_miter_limit : t -> float = "caml_cairo_get_miter_limit"
    (** Gets the current miter limit, as set by {!Cairo.set_miter_limit}. *)


(** {3 Drawing operations} *)

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
    mathematical definitions, see http://cairographics.org/operators/ *)
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

external set_operator : t -> operator -> unit = "caml_cairo_set_operator"
    (** Sets the compositing operator to be used for all drawing
        operations.  See {!Cairo.operator} for details on the
        semantics of each available compositing operator.

        The default operator is [OVER]. *)

external get_operator : t -> operator = "caml_cairo_get_operator"
    (** Gets the current compositing operator for a cairo context.  *)

external set_tolerance : t -> float -> unit = "caml_cairo_set_tolerance"
    (** Sets the tolerance used when converting paths into trapezoids.
        Curved segments of the path will be subdivided until the
        maximum deviation between the original path and the polygonal
        approximation is less than tolerance.  The default value is
        [0.1].  A larger value will give better performance, a smaller
        value, better appearance.  (Reducing the value from the
        default value of [0.1] is unlikely to improve appearance
        significantly.)  *)
external get_tolerance : t -> float = "caml_cairo_get_tolerance"
    (** Gets the current tolerance value, as set by {!Cairo.set_tolerance}. *)

(** Bounding box (aka extents). *)
type bounding_box = {
  x1: float;      (** left of the resulting extents *)
  y1: float;      (** top of the resulting extents *)
  x2: float;      (** right of the resulting extents  *)
  y2: float;      (** bottom of the resulting extents  *)
}

type rectangle = { x:float; y:float; width:float; height:float }

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

external clip_extents : t -> bounding_box = "caml_cairo_clip_extents"
  (** Computes a bounding box in user coordinates covering the area
      inside the current clip.  *)

external clip_reset : t -> unit = "caml_cairo_reset_clip"
  (** Reset the current clip region to its original, unrestricted
      state.  That is, set the clip region to an infinitely large
      shape containing the target surface.  Equivalently, if infinity is too
      hard to grasp, one can imagine the clip region being reset to
      the exact bounds of the target surface.

      Note that code meant to be reusable should not call [clip_reset]
      as it will cause results unexpected by higher-level code which
      calls {!Cairo.clip}.  Consider using {!Cairo.save} and
      {!Cairo.restore} around {!Cairo.clip} as a more robust means of
      temporarily restricting the clip region. *)

external clip_rectangle_list : t -> rectangle list
  = "caml_cairo_copy_clip_rectangle_list"
  (** Gets the current clip region as a list of rectangles in user
      coordinates.

      Raises [Error(CLIP_NOT_REPRESENTABLE)] to indicate that the clip
      region cannot be represented as a list of user-space rectangles.  *)


val fill : ?preserve:bool -> t -> unit
  (** A drawing operator that fills the current path according to the
      current fill rule, (each sub-path is implicitly closed before
      being filled).  After [fill], the current path will be cleared
      from the cairo context unless [preserve] is [true] (default: [false]).

      See also {!Cairo.set_fill_rule}. *)

external fill_extents : t -> bounding_box = "caml_cairo_fill_extents"
  (** Computes a bounding box in user coordinates covering the area
      that would be affected, (the "inked" area), by a [fill]
      operation given the current path and fill parameters.  If the
      current path is empty, returns an empty rectangle [{ x1=0.;
      y1=0.; x2=0.; y2=0. }].  Surface dimensions and clipping are not
      taken into account.

      Contrast with {!Cairo.path_extents}, which is similar, but
      returns non-zero extents for some paths with no inked area,
      (such as a simple line segment).

      Note that [fill_extents] must necessarily do more work to
      compute the precise inked areas in light of the fill rule, so
      {!Cairo.path_extents} may be more desirable for sake of
      performance if the non-inked path extents are desired.

      See {!Cairo.fill} and {!Cairo.set_fill_rule}. *)

external in_fill : t -> x:float -> y:float -> bool = "caml_cairo_in_fill"
  (** Tests whether the given point is inside the area that would be
      affected by a [fill] operation given the current path and
      filling parameters.  Surface dimensions and clipping are not
      taken into account.

      See also {!Cairo.fill} and {!Cairo.set_fill_rule}.  *)

external mask : t -> Pattern.t -> unit = "caml_cairo_mask"
  (** [mask cr pattern]: a drawing operator that paints the current
      source using the alpha channel of [pattern] as a mask.  (Opaque
      areas of [pattern] are painted with the source, transparent
      areas are not painted.) *)
external mask_surface : t -> Surface.t -> x:float -> y:float -> unit
  = "caml_cairo_mask_surface"
  (** [mask_surface cr surface x y]: a drawing operator that paints
      the current source using the alpha channel of [surface] as a
      mask.  (Opaque areas of [surface] are painted with the source,
      transparent areas are not painted.)

      @param x  X coordinate at which to place the origin of [surface].
      @param y  Y coordinate at which to place the origin of [surface]. *)

val paint : ?alpha:float -> t -> unit
  (** A drawing operator that paints the current source everywhere
      within the current clip region.  If [alpha] is set, the drawing
      is faded out using the alpha value.

      @param alpha  alpha value, between 0 (transparent) and 1 (opaque).  *)

val stroke : ?perserve:bool -> t -> unit
  (** A drawing operator that strokes the current path according to
      the current line width, line join, line cap, and dash settings.
      After [stroke], the current path will be cleared from the cairo
      context unless [preserve] is [true] (default: [false]).  See
      {!Cairo.set_line_width}, {!Cairo.set_line_join},
      {!Cairo.set_line_cap}, and {!Cairo.set_dash}.

      Note: Degenerate segments and sub-paths are treated specially
      and provide a useful result.  These can result in two different
      situations:

      1. Zero-length "on" segments set in {!Cairo.set_dash}.  If the
      cap style is [ROUND] or [SQUARE] then these segments will be
      drawn as circular dots or squares respectively.  In the case of
      [SQUARE], the orientation of the squares is determined by the
      direction of the underlying path.

      2. A sub-path created by {!Cairo.move_to} followed by either a
      {!Cairo.close_path} or one or more calls to {!Cairo.line_to} to
      the same coordinate as the {!Cairo.move_to}.  If the cap style
      is [ROUND] then these sub-paths will be drawn as circular dots.
      Note that in the case of [SQUARE] line cap, a degenerate
      sub-path will not be drawn at all, (since the correct
      orientation is indeterminate).

      In no case will a cap style of [BUTT] cause anything to be drawn
      in the case of either degenerate segments or sub-paths. *)

val stroke_extents : t -> bounding_box
  (** Computes a bounding box in user coordinates covering the area
      that would be affected, (the "inked" area), by a {!Cairo.stroke}
      operation operation given the current path and stroke
      parameters.  If the current path is empty, returns an empty
      rectangle [{ x1=0.; y1=0.; x2=0.; y2=0. }].  Surface dimensions
      and clipping are not taken into account.

      Note that if the line width is set to exactly zero, then
      [stroke_extents] will return an empty rectangle.  Contrast with
      {!Cairo.path_extents} which can be used to compute the non-empty
      bounds as the line width approaches zero.

      Note that [stroke_extents] must necessarily do more work to
      compute the precise inked areas in light of the stroke
      parameters, so {!Cairo.path_extents} may be more desirable for
      sake of performance if non-inked path extents are desired.

      See {!Cairo.stroke}, {!Cairo.set_line_width}, {!Cairo.set_line_join},
      {!Cairo.set_line_cap}, and {!Cairo.set_dash}. *)

external in_stroke : t -> x:float -> y:float -> bool = "caml_cairo_in_stroke"
  (** Tests whether the given point is inside the area that would be
      affected by a {!Cairo.stroke} operation given the current path
      and stroking parameters. Surface dimensions and clipping are not
      taken into account.  *)

external copy_page : t -> unit = "caml_cairo_copy_page"
  (** [copy_page cr] emits the current page for backends that support
      multiple pages, but doesn't clear it, so, the contents of the
      current page will be retained for the next page too.  Use
      {!Cairo.show_page} if you want to get an empty page after the
      emission.

      This is a convenience function that simply calls
      {!Cairo.Surface.copy_page} on [cr]'s target. *)
external show_page : t -> unit = "caml_cairo_show_page"
  (** [show_page cr] emits and clears the current page for backends
      that support multiple pages.  Use {!Cairo.copy_page} if you
      don't want to clear the page.

      This is a convenience function that simply calls
      {!Cairo.Surface.show_page} on [cr]'s target. *)


(* ---------------------------------------------------------------------- *)
(** {2 Creating paths and manipulating path data} *)

(* set_user_data *)
(* get_user_data *)
