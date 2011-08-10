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


(** Cairo: A Vector Graphics Library (bindings).

    {b Drawing:}
    - {!cairo_t}: The cairo drawing context
    - {!paths}: Creating paths and manipulating path data
    - {!Pattern}: Sources for drawing.
    - {!transformations}: Manipulating the current transformation matrix.
    - {!text}: Rendering text and glyphs.

    {b Fonts:}
    - {!Font_face}: Base module for font faces.
    - {!Scaled_font}: Font face at particular size and options.
    - {!Font_options}: How a font should be rendered.
    - {!Ft}: FreeType Fonts -- Font support for FreeType.
    - {!Win32_font}: Win32 Fonts -- Font support for Microsoft Windows.
    - {!Quartz_font}: Quartz (CGFont) Fonts -- Font support via CGFont on OS X.
    - {!User_font}: Font support with font data provided by the user.

    {b {!surfaces}} (platform independent {!surface_backends} and others):
    - {!Surface}: Base module for surfaces.
    - {!Image}: Image Surfaces -- Rendering to memory buffers.
    - {!PDF}: PDF Surfaces -- Rendering PDF documents.
    - {!PNG}: PNG Support -- Reading and writing PNG images.
    - {!PS}: PostScript Surfaces -- Rendering PostScript documents.
    - {!SVG}: SVG Surfaces -- Rendering SVG documents.
    - {!XLib}: XLib Surfaces -- X Window System rendering using XLib.
    - {!Win32}: Win32 Surfaces -- Microsoft Windows surface support.
    - {!Quartz}: Quartz Surfaces -- Rendering to Quartz surfaces.


    In order to get acquainted with Cairo's concepts we recommend that
    you read the {{:http://archimedes.forge.ocamlcore.org/cairo/}
    Cairo OCaml tutorial}.

    @author Christophe Troestler
*)

type status =
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
  | PATTERN_TYPE_MISMATCH  (* should not be raised -- fobidden by types *)
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

exception Unavailable
  (** Exception raised by functions of backend modules when they are
      not available in the installed Cairo library. *)

type context
  (** The cairo drawing context.  This is the main object used when
      drawing with cairo.  To draw with cairo, you create a [t], set
      the target surface, and drawing options for the [t], create
      shapes with functions like {!Cairo.move_to} and
      {!Cairo.line_to}, and then draw shapes with {!Cairo.stroke} or
      {!Cairo.fill}.  *)

(* ---------------------------------------------------------------------- *)
(** {2:matrix  Generic matrix operations} *)

(** Holds an affine transformation, such as a scale, rotation, shear,
    or a combination of those. The transformation of a point (x, y) is
    given by:
    {[
    x_new = xx *. x +. xy *. y +. x0;
    y_new = yx *. x +. yy *. y +. y0;
    ]} *)
type matrix = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float; }

(** This is used throughout cairo to convert between different
    coordinate spaces. *)
module Matrix :
sig
  type t = matrix

  val init_identity : unit -> t
    (** [init_identity()] returns the identity transformation. *)

  val init_translate : x:float -> y:float -> t
    (** [init_translate tx ty] return a transformation that translates
        by [tx] and [ty] in the X and Y dimensions, respectively. *)

  val init_scale : x:float -> y:float -> t
    (** [init_scale sx sy] return a transformation that scales by [sx]
        and [sy] in the X and Y dimensions, respectively. *)

  val init_rotate : angle:float -> t
    (** [init_rotate radians] returns a a transformation that rotates
        by [radians]. *)

  val translate : t -> x:float -> y:float -> unit
    (** [translate matrix tx ty] applies a translation by [tx], [ty]
        to the transformation in [matrix].  The effect of the new
        transformation is to first translate the coordinates by [tx]
        and [ty], then apply the original transformation to the
        coordinates. *)

  val scale : t -> x:float -> y:float -> unit
    (** [scale matrix sx sy] applies scaling by [sx], [sy] to the
        transformation in [matrix].  The effect of the new
        transformation is to first scale the coordinates by [sx] and [sy],
        then apply the original transformation to the coordinates. *)

  val rotate : t -> angle:float -> unit
    (** [rotate matrix radians] applies rotation by [radians] to the
        transformation in [matrix].  The effect of the new
        transformation is to first rotate the coordinates by [radians],
        then apply the original transformation to the coordinates. *)

  val invert : t -> unit
    (** [invert matrix] changes [matrix] to be the inverse of it's
        original value.  Not all transformation matrices have
        inverses; if the matrix collapses points together (it is
        degenerate), then it has no inverse and this function will
        fail. *)

  val multiply : t -> t -> t
    (** [multiply a b] multiplies the affine transformations in [a]
        and [b] together and return the result.  The effect of the
        resulting transformation is to first apply the transformation
        in [a] to the coordinates and then apply the transformation in
        [b] to the coordinates.  *)

  val transform_distance : t -> dx:float -> dy:float -> float * float
    (** [transform_distance matrix dx dy] transforms the distance
        vector ([dx],[dy]) by [matrix].  This is similar to
        {!Cairo.Matrix.transform_point} except that the translation
        components of the transformation are ignored.  The calculation
        of the returned vector is as follows:
        {[
        dx2 = dx1 * a + dy1 * c;
        dy2 = dx1 * b + dy1 * d;
        ]}
        Affine transformations are position invariant, so the same
        vector always transforms to the same vector.  If (x1,y1)
        transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
        (x1+dx2,y1+dy2) for all values of x1 and x2.  *)

  val transform_point : t -> x:float -> y:float -> float * float
    (** [transform_point matrix x y] transforms the point ([x], [y])
        by [matrix]. *)
end

(* ---------------------------------------------------------------------- *)
(** {2:text  Rendering text and glyphs} *)

(** The [Cairo.text_extents] structure stores the extents of a single
    glyph or a string of glyphs in user-space coordinates.  Because text
    extents are in user-space coordinates, they are mostly, but not
    entirely, independent of the current transformation matrix.  If you
    call {!Cairo.scale}[cr 2.0 2.0], text will be drawn twice as big, but
    the reported text extents will not be doubled.  They will change
    slightly due to hinting (so you can't assume that metrics are
    independent of the transformation matrix), but otherwise will remain
    unchanged. *)
type text_extents = {
  x_bearing : float;
  (** The horizontal distance from the origin of the text to the
      leftmost part of the glyphs as drawn.  Positive if the glyphs
      lie entirely to the right of the origin. *)
  y_bearing : float;
  (** The vertical distance from the origin to the topmost part of the
      glyphs as drawn.  Positive only if the glyphs lie completely below
      the origin; will usually be negative.  *)
  width : float; (** width of the glyphs as drawn *)
  height : float; (** height of the glyphs as drawn *)
  x_advance : float;
  (** Distance to advance in the X direction after drawing these glyphs. *)
  y_advance : float;
  (** Distance to advance in the Y direction after drawing these
      glyphs. Will typically be zero except for vertical text layout
      as found in East-Asian languages. *)
}

(** {3 Low-level text API} *)

(** This is Cairo low-level text API.  The low-level API relies on the
    user to convert text to a set of glyph indexes and positions. This is
    a very hard problem and is best handled by external libraries, like
    the pangocairo that is part of the Pango text layout and rendering
    library.  Pango is available from http://www.pango.org/

    See also the {!text_toy}.  *)
module Glyph :
sig
  (** The [Glyph.t] structure holds information about a single glyph
      when drawing or measuring text.  A font is (in simple terms) a
      collection of shapes used to draw text.  A glyph is one of these
      shapes. There can be multiple glyphs for a single character
      (alternates to be used in different contexts, for example), or a
      glyph can be a ligature of multiple characters.  Cairo doesn't
      expose any way of converting input text into glyphs, so in order
      to use the Cairo interfaces that take arrays of glyphs, you must
      directly access the appropriate underlying font system.

      Note that the offsets given by x and y are not cumulative.  When
      drawing or measuring text, each glyph is individually positioned
      with respect to the overall origin. *)
  type t = {
    index: int; (** glyph index in the font. The exact interpretation
                    of the glyph index depends on the font technology
                    being used. *)
    x: float; (** the offset in the X direction between the origin
                  used for drawing or measuring the string and the
                  origin of this glyph.  *)
    y: float; (** the offset in the Y direction between the origin
                  used for drawing or measuring the string and the
                  origin of this glyph. *)
  }

  (** The [cluster] record holds information about a single {i text
      cluster}.  A text cluster is a minimal mapping of some glyphs
      corresponding to some UTF-8 text.

      For a cluster to be valid, both [num_bytes] and [num_glyphs]
      should be non-negative, and at least one should be non-zero.
      Note that clusters with zero glyphs are not as well supported as
      normal clusters.  For example, PDF rendering applications
      typically ignore those clusters when PDF text is being selected.

      See {!Cairo.Glyph.show_text} for how clusters are used in
      advanced text operations. *)
  type cluster = {
    num_bytes : int; (** the number of bytes of UTF-8 text covered by cluster *)
    num_glyphs : int; (** the number of glyphs covered by cluster *)
  }

  (** Specifies properties of a text cluster mapping. *)
  type cluster_flags =
    | BACKWARD (** The clusters in the cluster array map to glyphs in
                   the glyph array from end to start. *)

  external extents : context -> t array -> text_extents
    = "caml_cairo_glyph_extents"
    (** Gets the extents for an array of glyphs. The extents describe a
        user-space rectangle that encloses the "inked" portion of the
        glyphs (as they would be drawn by {!Cairo.Glyph.show}).
        Additionally, the [x_advance] and [y_advance] values indicate
        the amount by which the current point would be advanced by
        {!Cairo.Glyph.show}.

        Note that whitespace glyphs do not contribute to the size of the
        rectangle (extents.width and extents.height). *)

  external show : context -> t array -> unit = "caml_cairo_show_glyphs"
    (** A drawing operator that generates the shape from an array of
        glyphs, rendered according to the current font face, font size (font
        matrix), and font options. *)

  external show_text : context -> string -> t array -> cluster array ->
    cluster_flags -> unit  = "caml_cairo_show_text_glyphs"
    (** [show_text cr utf8 glyphs clusters cluster_flags]: This
        operation has rendering effects similar to
        {!Cairo.Glyph.show} but, if the target surface supports it,
        uses the provided text and cluster mapping to embed the text
        for the glyphs shown in the output.  If the target does not
        support the extended attributes, this function acts like the
        basic {!Cairo.Glyph.show} as if it had been passed [glyphs].

        The mapping between [utf8] and [glyphs] is provided by an
        array of [clusters].  Each cluster covers a number of text bytes
        and glyphs, and neighboring clusters cover neighboring areas
        of [utf8] and [glyphs].  The clusters should collectively cover
        [utf8] and [glyphs] in entirety.

        The first cluster always covers bytes from the beginning of
        [utf8].  If [cluster_flags] do not have the [BACKWARD] set,
        the first cluster also covers the beginning of glyphs,
        otherwise it covers the end of the glyphs array and following
        clusters move backward.

        See {!Cairo.Glyph.cluster} for constraints on valid clusters. *)

end


(** {3:text_toy "Toy" text API}

    This is cairo's toy text API.  The toy API takes UTF-8 encoded text
    and is limited in its functionality to rendering simple
    left-to-right text with no advanced features.  That means for
    example that most complex scripts like Hebrew, Arabic, and Indic
    scripts are out of question.  No kerning or correct positioning of
    diacritical marks either.  The font selection is pretty limited
    too and doesn't handle the case that the selected font does not
    cover the characters in the text.  This set of functions are
    really that, a toy text API, for testing and demonstration
    purposes. Any serious application should avoid them.

    See the {!Glyph} module for the low-level text API.   *)

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

(** The subpixel order specifies the order of color elements within
    each pixel on the display device when rendering with an
    antialiasing mode of [ANTIALIAS_SUBPIXEL] (see {!Cairo.antialias}). *)
type subpixel_order =
  | SUBPIXEL_ORDER_DEFAULT (** Use the default subpixel order for for
                               the target device *)
  | SUBPIXEL_ORDER_RGB (** Subpixel elements are arranged horizontally
                           with red at the left *)
  | SUBPIXEL_ORDER_BGR (** Subpixel elements are arranged horizontally
                           with blue at the left *)
  | SUBPIXEL_ORDER_VRGB (** Subpixel elements are arranged vertically
                            with red at the top *)
  | SUBPIXEL_ORDER_VBGR (** Subpixel elements are arranged vertically
                            with blue at the top *)

(** Specifies the type of hinting to do on font outlines.  Hinting is
    the process of fitting outlines to the pixel grid in order to improve
    the appearance of the result.  Since hinting outlines involves
    distorting them, it also reduces the faithfulness to the original
    outline shapes.  Not all of the outline hinting styles are supported by
    all font backends. *)
type hint_style =
  | HINT_STYLE_DEFAULT (** Use the default hint style for font backend
                           and target device *)
  | HINT_STYLE_NONE (** Do not hint outlines *)
  | HINT_STYLE_SLIGHT (** Hint outlines slightly to improve contrast
                          while retaining good fidelity to the
                          original shapes. *)
  | HINT_STYLE_MEDIUM (** Hint outlines with medium strength giving a
                          compromise between fidelity to the original
                          shapes and contrast. *)
  | HINT_STYLE_FULL (** Hint outlines to maximize contrast. *)

(** Specifies whether to hint font metrics; hinting font metrics means
    quantizing them so that they are integer values in device space.
    Doing this improves the consistency of letter and line spacing,
    however it also means that text will be laid out differently at
    different zoom factors. *)
type hint_metrics =
  | HINT_METRICS_DEFAULT (** Hint metrics in the default manner for
                             the font backend and target device. *)
  | HINT_METRICS_OFF (** Do not hint font metrics. *)
  | HINT_METRICS_ON (** Hint font metrics. *)


(** The font options specify how fonts should be rendered.  Most of the
    time the font options implied by a surface are just right and do not
    need any changes, but for pixel-based targets tweaking font options
    may result in superior output on a particular display.  *)
module Font_options :
sig
  type t
    (** An opaque type holding all options that are used when rendering
        fonts.

        Individual features of a [Cairo.Font_options.t] can be set or
        accessed using functions below, like
        {!Cairo.Font_options.set_antialias} and
        {!Cairo.Font_options.get_antialias}.

        New features may be added to a [font_options] in the future. For
        this reason, {!Cairo.Font_options.copy} and
        {!Cairo.Font_options.merge} should be used to copy or merge of
        [Cairo.Font_options.t] values. *)

  external set : context -> t -> unit = "caml_cairo_set_font_options"
    (** [set_font_options cr opt] sets a set of custom font rendering
        options for [cr].  Rendering options are derived by merging
        these options with the options derived from underlying
        surface; if the value in options has a default value (like
        [ANTIALIAS_DEFAULT]), then the value from the surface is used.  *)

  external get : context -> t = "caml_cairo_get_font_options"
    (** Retrieves font rendering options set via
        {!Cairo.Font_options.set}.  Note that the returned options do
        not include any options derived from the underlying surface;
        they are literally the options passed to
        {!Cairo.Font_options.set}. *)

  external create : unit -> t = "caml_cairo_font_options_create"
    (** Allocates a new font options object with all options initialized
        to default values.  *)

  val make : ?antialias:antialias -> ?subpixel_order:subpixel_order ->
    ?hint_style:hint_style -> ?hint_metrics:hint_metrics -> unit -> t
    (** Convenience function to create an options object with properties set.
        @param antialias default: [ANTIALIAS_DEFAULT]
        @param subpixel_order default: [SUBPIXEL_ORDER_DEFAULT]
        @param hint_style default: [HINT_STYLE_DEFAULT]
        @param hint_metrics default: [HINT_METRICS_DEFAULT]
    *)

  external copy : t -> t = "caml_cairo_font_options_copy"
    (** [copy original] allocates a new font options object copying
        the option values from [original]. *)

  external merge : t -> t -> unit = "caml_cairo_font_options_merge"
    (** [merge options other] merges non-default options from other
        into options, replacing existing values.  This operation can
        be thought of as somewhat similar to compositing other onto
        options with the operation of [OVER] (see {!Cairo.operator}). *)

  external set_antialias : t -> antialias -> unit
    = "caml_cairo_font_options_set_antialias"
    (** Sets the antialiasing mode for the font options object.  This
        specifies the type of antialiasing to do when rendering text. *)

  external get_antialias : t -> antialias
    = "caml_cairo_font_options_get_antialias"
    (** Gets the antialiasing mode for the font options object. *)

  external set_subpixel_order : t -> subpixel_order -> unit
    = "caml_cairo_font_options_set_subpixel_order"
    (** Sets the subpixel order for the font options object.  The
        subpixel order specifies the order of color elements within
        each pixel on the display device when rendering with an
        antialiasing mode of [ANTIALIAS_SUBPIXEL] (see
        {!Cairo.antialias}).  See the documentation for
        {!Cairo.subpixel_order} for full details. *)

  external get_subpixel_order : t -> subpixel_order
    = "caml_cairo_font_options_get_subpixel_order"
    (** Gets the subpixel order for the font options object. See the
        documentation for {!Cairo.subpixel_order} for full details. *)

  external set_hint_style : t -> hint_style -> unit
    = "caml_cairo_font_options_set_hint_style"
    (** Sets the hint style for font outlines for the font options
        object. This controls whether to fit font outlines to the
        pixel grid, and if so, whether to optimize for fidelity or
        contrast.  See the documentation for {!Cairo.hint_style} for
        full details. *)

  external get_hint_style : t -> hint_style
    = "caml_cairo_font_options_get_hint_style"
    (** Gets the hint style for font outlines for the font options
        object. See the documentation for {!Cairo.hint_style} for full
        details. *)

  external set_hint_metrics : t -> hint_metrics -> unit
    = "caml_cairo_font_options_set_hint_metrics"
    (** Sets the metrics hinting mode for the font options
        object. This controls whether metrics are quantized to integer
        values in device units. See the documentation for
        {!Cairo.hint_metrics} for full details. *)

  external get_hint_metrics : t -> hint_metrics
    = "caml_cairo_font_options_get_hint_metrics"
    (** Gets the metrics hinting mode for the font options object. See
        the documentation for {!Cairo.hint_metrics} for full details. *)
end

(** Specifies variants of a font face based on their slant. *)
type slant = Upright | Italic | Oblique

(** Specifies variants of a font face based on their weight. *)
type weight = Normal | Bold

(** {!Cairo.font_type} is used to describe the type of a given font
    face or scaled font.  The font types are also known as "font
    backends" within cairo.

    The type of a font face is determined by the function used to
    create it, which will generally be of the form
    [Cairo.*.font_face_create].  The font face type can be queried
    with {!Cairo.Font_face.get_type}

    The various {!Cairo.Font_face} functions can be used with a font
    face of any type.

    The type of a scaled font is determined by the type of the font
    face passed to {!Cairo.Scaled_font.create}.  The scaled font type
    can be queried with {!Cairo.Scaled_font.get_type}.

    The various {!Cairo.Scaled_font} functions can be used with scaled
    fonts of any type, but some font backends also provide
    type-specific functions (such as {!Cairo.Ft.scaled_font_lock_face})
    that must only be called with a scaled font of the appropriate type.

    FIXME: The behavior of calling a type-specific function with a scaled
    font of the wrong type is undefined. *)
type font_type =
  [ `Toy (** The font was created using cairo's toy font api *)
  | `Ft (** The font is of type FreeType *)
  | `Win32 (** The font is of type Win32 *)
  | `Quartz (** The font is of type Quartz *)
  | `User (** The font was create using cairo's user font api *)
  ]

(** {!Cairo.Font_face.t} represents a particular font at a particular
    weight, slant, and other characteristic but no size,
    transformation, or size. *)
module Font_face :
sig
  type 'a t
    (** A {!Cairo.Font_face.t} specifies all aspects of a font other
        than the size or font matrix (a font matrix is used to distort
        a font by sheering it or scaling it unequally in the two
        directions).  A font face can be set on a {!Cairo.context} by using
        {!Cairo.Font_face.set}; the size and font matrix are set with
        {!Cairo.set_font_size} and {!Cairo.set_font_matrix}.

        Font faces are created using font-backend-specific constructors,
        or implicitly using the toy text API by way of
        {!Cairo.select_font_face}.

        There are various types of font faces, depending on the font
        backend they use.  The type of a font face can be queried using
        {!Cairo.Font_face.get_type}.  *)

  external set : context -> _ t -> unit = "caml_cairo_set_font_face"
      (** Replaces the current {!Cairo.Font_face.t} object in the
          {!Cairo.context} with font_face. *)

  external get : context -> font_type t = "caml_cairo_get_font_face"
      (** Gets the current font face for a {!Cairo.context}. *)

  external get_type : 'a t -> font_type = "caml_cairo_font_face_get_type"
      (** This function returns the type of the backend used to create a
          font face. See {!Cairo.font_type} for available types.  If ['a]
          contains a single variant, it will be the returned value. *)

  val create : ?family:string -> slant -> weight -> [`Toy] t
    (** [create family slant weight] creates a font face from a
        triplet of family, slant, and weight.  These font faces are
        used in implementation of the the cairo "toy" font API.

        If family is not given or is the zero-length string "", the
        platform-specific default family is assumed.  The default
        family then can be queried using {!Cairo.Font_face.get_family}.

        The {!Cairo.select_font_face} function uses this to create font
        faces. See that function for limitations of toy font faces. *)

  external get_family : [`Toy] t -> string
    = "caml_cairo_toy_font_face_get_family"
    (** Gets the familly name of a toy font. *)

  external get_slant : [`Toy] t -> slant
    = "caml_cairo_toy_font_face_get_slant"
    (** Gets the slant a toy font. *)

  external get_weight : [`Toy] t -> weight
    = "caml_cairo_toy_font_face_get_weight"
    (** Gets the weight a toy font. *)
end

(** The [Cairo.font_extents] structure stores metric information for a
    font. Values are given in the current user-space coordinate
    system.

    Because font metrics are in user-space coordinates, they are
    mostly, but not entirely, independent of the current
    transformation matrix.  If you call {!Cairo.scale}[cr 2.0 2.0],
    text will be drawn twice as big, but the reported text extents
    will not be doubled. They will change slightly due to hinting
    (so you can't assume that metrics are independent of the
    transformation matrix), but otherwise will remain unchanged. *)
type font_extents = {
  ascent : float;
  (** the distance that the font extends above the baseline. Note that
      this is not always exactly equal to the maximum of the extents
      of all the glyphs in the font, but rather is picked to express
      the font designer's intent as to how the font should align with
      elements above it.  *)
  descent : float;
  (** the distance that the font extends below the baseline. This
      value is positive for typical fonts that include portions
      below the baseline. Note that this is not always exactly equal
      to the maximum of the extents of all the glyphs in the font,
      but rather is picked to express the font designer's intent as
      to how the the font should align with elements below it.  *)
  baseline : float;
  (** the recommended vertical distance between baselines when
      setting consecutive lines of text with the font. This is greater
      than ascent+descent by a quantity known as the line spacing or
      external leading. When space is at a premium, most fonts can be
      set with only a distance of ascent+descent between lines.  *)
  max_x_advance : float;
  (** the maximum distance in the X direction that the the origin is
      advanced for any glyph in the font.  *)
  max_y_advance : float;
  (** the maximum distance in the Y direction that the the origin is
      advanced for any glyph in the font. this will be zero for normal
      fonts used for horizontal writing. (The scripts of East Asia are
      sometimes written vertically.)  *)
}


(** {!Cairo.Scaled_font.t} represents a realization of a font face at
    a particular size and transformation and a certain set of font
    options.  *)
module Scaled_font :
sig
  type 'a t
    (** A [Cairo.Scaled_font.t] is a font scaled to a particular size
        and device resolution.  It is most useful for low-level font
        usage where a library or application wants to cache a
        reference to a scaled font to speed up the computation of metrics.

        There are various types of scaled fonts, depending on the font
        backend they use.  The type of a scaled font can be queried
        using {!Cairo.Scaled_font.get_type}.  *)

  external set : context -> _ t -> unit = "caml_cairo_set_scaled_font"
    (** Replaces the current font face, font matrix, and font options in
        the {Cairo.context} with those of the {!Cairo.Scaled_font.t}.  Except
        for some translation, the current CTM of the {!Cairo.context} should be
        the same as that of the {!Cairo.Scaled_font.t}, which can be
        accessed using {!Cairo.Scaled_font.get_ctm}. *)

  external get : context -> 'a t = "caml_cairo_get_scaled_font"
    (** Gets the current scaled font for a cairo_t. *)

  val create : 'a Font_face.t -> Matrix.t -> Matrix.t -> Font_options.t -> 'a t
    (** [create font_face font_matrix ctm options] creates a
        {!Cairo.Scaled_font.t} object from a font face and matrices that
        describe the size of the font and the environment in which it
        will be used. *)

  external extents : _ t -> font_extents = "caml_cairo_scaled_font_extents"
    (** [extents sf] gets the metrics for [sf]. *)

  external text_extents : _ t -> string -> text_extents
    = "caml_cairo_scaled_font_text_extents"
      (** [text_extents scaled_font utf8] gets the [extents] for a string of
          text. The extents describe a user-space rectangle that encloses
          the "inked" portion of the text drawn at the origin (0,0) (as it
          would be drawn by {!Cairo.show_text} if the cairo graphics state
          were set to the same font_face, font_matrix, ctm, and
          font_options as [scaled_font]). Additionally, the x_advance and
          y_advance values indicate the amount by which the current point
          would be advanced by {!Cairo.show_text}.  The string [utf8]
          should not contain ['\000'] characters.

          Note that whitespace characters do not directly contribute
          to the size of the rectangle ([extents.width] and
          [extents.height]).  They do contribute indirectly by changing
          the position of non-whitespace characters.  In particular,
          trailing whitespace characters are likely to not affect the
          size of the rectangle, though they will affect the [x_advance]
          and [y_advance] values. *)

  external glyph_extents : _ t -> Glyph.t array -> text_extents
    = "caml_cairo_scaled_font_glyph_extents"
    (** [glyph_extents scaled_font glyphs] gets the [extents] for an
        array of glyphs. The extents describe a user-space rectangle
        that encloses the "inked" portion of the glyphs (as they
        would be drawn by {!Cairo.Glyph.show} if the cairo graphics
        state were set to the same font_face, font_matrix, ctm, and
        font_options as [scaled_font]).  Additionally, the [x_advance] and
        [y_advance] values indicate the amount by which the current
        point would be advanced by {!Cairo.Glyph.show}.

        Note that whitespace glyphs do not contribute to the size of the
        rectangle ([extents.width] and [extents.height]). *)

  external text_to_glyphs : _ t -> x:float -> y:float -> string
    -> Glyph.t array * Glyph.cluster array * Glyph.cluster_flags
    = "caml_cairo_scaled_font_text_to_glyphs"
    (** [text_to_glyphs scaled_font x y utf8] converts UTF-8 text to
        an array of glyphs, optionally with cluster mapping, that can
        be used to render later using [scaled_font].
        See {!Cairo.Glyph.show_text}. *)

  external get_font_face : 'a t -> 'a Font_face.t
    = "caml_cairo_scaled_font_get_font_face"
    (** Gets the font face that this scaled font was created for. *)

  external get_font_options : _ t -> Font_options.t
    = "caml_cairo_scaled_font_get_font_options"
    (** [get_font_options scaled_font] returns the font options with
        which [scaled_font] was created.  *)

  external get_font_matrix : _ t -> Matrix.t
    = "caml_cairo_scaled_font_get_font_matrix"
    (** [get_font_matrix scaled_font] return the font matrix with
        which [scaled_font] was created. *)

  external get_ctm : _ t -> Matrix.t = "caml_cairo_scaled_font_get_ctm"
    (** [get_ctm scaled_font] returns the CTM with which [scaled_font]
        was created. *)

  external get_scale_matrix : _ t -> Matrix.t
    = "caml_cairo_scaled_font_get_scale_matrix"
    (** [get_scale_matrix scaled_font] returns the scale matrix of
        [scaled_font].  The scale matrix is product of the font matrix
        and the ctm associated with the scaled font, and hence is the
        matrix mapping from font space to device space. *)

  external get_type : 'a t -> font_type = "caml_cairo_scaled_font_get_type"
    (** This function returns the type of the backend used to create a
        scaled font.  See {!Cairo.font_type} for available types. *)
end


val select_font_face : context -> ?slant:slant -> ?weight:weight -> string ->
  unit
  (** [select_font_face cr family ?slant ?weight] selects a family
      and style of font from a simplified description as a family
      name, slant and weight. Cairo provides no operation to list
      available family names on the system (this is a "toy",
      remember), but the standard CSS2 generic family names,
      ("serif", "sans-serif", "cursive", "fantasy", "monospace"),
      are likely to work as expected.

      @param slant default [Upright].
      @param weight default [Normal].

      For "real" font selection, see the font-backend-specific
      [font_face_create] functions for the font backend you are
      using.  (For example, if you are using the freetype-based
      cairo-ft font backend, see
      {!Cairo.Ft.font_face_create_for_ft_face} or
      {!Cairo.Ft.font_face_create_for_pattern}.)  The resulting font
      face could then be used with {!Cairo.Scaled_font.create} and
      {!Cairo.Scaled_font.set}.

      Similarly, when using the "real" font support, you can call
      directly into the underlying font system (such as fontconfig
      or freetype), for operations such as listing available fonts, etc.

      It is expected that most applications will need to use a more
      comprehensive font handling and text layout library (for
      example, pango) in conjunction with cairo.

      If text is drawn without a call to {!Cairo.select_font_face},
      (nor {!Cairo.Font_face.set} nor {!Cairo.Scaled_font.set}), the
      default family is platform-specific, but is essentially
      "sans-serif".  Default slant is [Upright], and default weight is
      [Normal].  *)

external set_font_size : context -> float -> unit
  = "caml_cairo_set_font_size"
  (** [set_font_size cr size] sets the current font matrix to a
      scale by a factor of size, replacing any font matrix previously
      set with [set_font_size] or {!Cairo.set_font_matrix}.  This
      results in a font size of size user space units.  (More precisely,
      this matrix will result in the font's em-square being a size by
      size square in user space.)

      If text is drawn without a call to [set_font_size] (nor
      {!Cairo.set_font_matrix}, nor {!Cairo.Scaled_font.set}),
      the default font size is 10.0. *)

external set_font_matrix : context -> Matrix.t -> unit
  = "caml_cairo_set_font_matrix"
  (** [set_font_matrix cr matrix] sets the current font matrix to
      [matrix].  The font matrix gives a transformation from the
      design space of the font (in this space, the em-square is 1
      unit by 1 unit) to user space.  Normally, a simple scale is
      used (see {!Cairo.set_font_size}), but a more complex font
      matrix can be used to shear the font or stretch it unequally
      along the two axes.  *)

external get_font_matrix : context -> Matrix.t = "caml_cairo_get_font_matrix"
  (** Returns the current font matrix.  See {!Cairo.set_font_matrix}. *)

external show_text : context -> string -> unit = "caml_cairo_show_text"
  (** A drawing operator that generates the shape from a string of
      UTF-8 characters, rendered according to the current [font_face],
      [font_size] (font_matrix), and [font_options].

      This function first computes a set of glyphs for the string of
      text. The first glyph is placed so that its origin is at the
      current point. The origin of each subsequent glyph is offset
      from that of the previous glyph by the advance values of the
      previous glyph.

      After this call the current point is moved to the origin of
      where the next glyph would be placed in this same progression.
      That is, the current point will be at the origin of the final
      glyph offset by its advance values. This allows for easy display
      of a single logical string with multiple calls to [show_text].  *)

external font_extents : context -> font_extents
  = "caml_cairo_font_extents"
  (** Gets the font extents for the currently selected font. *)

external text_extents : context -> string -> text_extents
  = "caml_cairo_text_extents"
  (** [text_extents cr utf8] gets the extents for a string of text.
      The extents describe a user-space rectangle that encloses the
      "inked" portion of the text (as it would be drawn by
      {!Cairo.show_text}).  Additionally, the [x_advance] and
      [y_advance] values indicate the amount by which the current
      point would be advanced by {!Cairo.show_text}.

      Note that whitespace characters do not directly contribute to
      the size of the rectangle ([extents.width] and
      [extents.height]).  They do contribute indirectly by changing
      the position of non-whitespace characters.  In particular,
      trailing whitespace characters are likely to not affect the size
      of the rectangle, though they will affect the [x_advance] and
      [y_advance] values. *)


(* ---------------------------------------------------------------------- *)

(** {2:surfaces  Surfaces} *)

(** A data structure for holding a rectangle. *)
type rectangle = {
  x:float;   (** X coordinate of the left side of the rectangle *)
  y:float;   (** Y coordinate of the the top side of the rectangle  *)
  w:float;   (** width of the rectangle *)
  h:float;   (** height of the rectangle  *)
}

(** {3 Base module for surfaces} *)

(** This is used to describe the content that a surface will contain,
    whether color information, alpha information (translucence
    vs. opacity), or both.  *)
type content = COLOR | ALPHA | COLOR_ALPHA

(** Abstract representation of all different drawing targets that
    cairo can render to; the actual drawings are performed using a
    cairo context.  *)
module Surface :
sig
  type t
    (** A {!Cairo.Surface.t} represents an image, either as the
        destination of a drawing operation or as source when drawing onto
        another surface.  To draw to a {!Cairo.Surface.t}, create a cairo
        context with the surface as the target, using {!Cairo.create}.

        There are different subtypes of {!Cairo.Surface.t} for
        different drawing backends; for example, {!Cairo.Image.create}
        creates a bitmap image in memory.  The type of a surface can
        be queried with {!Cairo.Surface.get_type}.  *)

  external create_similar : t -> content -> width:int -> height:int -> t
    = "caml_cairo_surface_create_similar"
      (** [create_similar other content width height] create a new
          surface that is as compatible as possible with the existing
          surface [other].  For example the new surface will have the
          same fallback resolution and font options as [other].
          Generally, the new surface will also use the same backend as
          other, unless that is not possible for some reason. The type
          of the returned surface may be examined with
          {!Cairo.Surface.get_type}.

          Initially the surface contents are all 0 (transparent if
          contents have transparency, black otherwise.) *)

  external finish : t -> unit = "caml_cairo_surface_finish"
      (** This function finishes the surface and drops all references
          to external resources. For example, for the Xlib backend it
          means that cairo will no longer access the drawable.  After
          calling {!Cairo.Surface.finish} the only valid operations on
          a surface are flushing and finishing it.  Further drawing to
          the surface will not affect the surface but will instead
          raise [Error(SURFACE_FINISHED)].  *)

  external flush : t -> unit = "caml_cairo_surface_flush"
      (** Do any pending drawing for the surface and also restore any
          temporary modification's cairo has made to the surface's
          state.  This function must be called before switching from
          drawing on the surface with cairo to drawing on it directly
          with native APIs.  If the surface doesn't support direct
          access, then this function does nothing.  *)

  external get_font_options : t -> Font_options.t
    = "caml_cairo_surface_get_font_options"
      (** [get_font_options surface] retrieves the default font
          rendering options for the [surface].  This allows display
          surfaces to report the correct subpixel order for rendering
          on them, print surfaces to disable hinting of metrics and so
          forth.  The result can then be used with
          {!Cairo.Scaled_font.create}. *)

  external get_content : t -> content = "caml_cairo_surface_get_content"
      (** This function returns the content type of surface which
          indicates whether the surface contains color and/or alpha
          information. See {!Cairo.content}. *)

  external mark_dirty : t -> unit = "caml_cairo_surface_mark_dirty"
      (** Tells cairo that drawing has been done to surface using
          means other than cairo, and that cairo should reread any
          cached areas.  Note that you must call {!Cairo.Surface.flush}
          before doing such drawing.  *)

  external mark_dirty_rectangle : t ->
    x:int -> y:int -> w:int -> h:int -> unit
    = "caml_cairo_surface_mark_dirty_rectangle"
      (** Like {!Cairo.Surface.mark_dirty}, but drawing has been done
          only to the specified rectangle, so that cairo can retain cached
          contents for other parts of the surface.

          Any cached clip set on the surface will be reset by this
          function, to make sure that future cairo calls have the clip
          set that they expect. *)

  external set_device_offset : t -> x:float -> y:float -> unit
    = "caml_cairo_surface_set_device_offset"
      (** Sets an offset that is added to the device coordinates
          determined by the CTM when drawing to surface. One use case for
          this function is when we want to create a {!Cairo.Surface.t} that
          redirects drawing for a portion of an onscreen surface to an
          offscreen surface in a way that is completely invisible to the
          user of the cairo API.  Setting a transformation via
          {!Cairo.translate} isn't sufficient to do this, since functions
          like {!Cairo.device_to_user} will expose the hidden offset.

          Note that the offset affects drawing to the surface as well
          as using the surface in a source pattern.

          @param x the offset in the X direction, in device units.
          @param y the offset in the Y direction, in device units. *)

  external get_device_offset : t -> float * float
    = "caml_cairo_surface_get_device_offset"
      (** This function returns the previous device offset set by
          {!Cairo.Surface.set_device_offset}. *)

  external set_fallback_resolution : t -> x:float -> y:float -> unit
    = "caml_cairo_surface_set_fallback_resolution"
      (** [set_fallback_resolution surface x_pixels_per_inch
          y_pixels_per_inch] sets the horizontal and vertical
          resolution for image fallbacks.

          When certain operations aren't supported natively by a
          backend, cairo will fallback by rendering operations to an
          image and then overlaying that image onto the output.  For
          backends that are natively vector-oriented, this function
          can be used to set the resolution used for these image
          fallbacks (larger values will result in more detailed
          images, but also larger file sizes).

          Some examples of natively vector-oriented backends are the
          ps, pdf, and svg backends.

          For backends that are natively raster-oriented, image
          fallbacks are still possible, but they are always performed
          at the native device resolution. So this function has no
          effect on those backends.

          Note: The fallback resolution only takes effect at the time
          of completing a page (with {!Cairo.show_page} or
          {!Cairo.copy_page}) so there is currently no way to have more
          than one fallback resolution in effect on a single page.

          The default fallback resoultion is 300 pixels per inch in
          both dimensions. *)

  external get_fallback_resolution : t -> float * float
    = "caml_cairo_surface_get_fallback_resolution"
      (** This function returns the previous fallback resolution set
          by {!Cairo.Surface.set_fallback_resolution}, or default fallback
          resolution if never set. *)

  (** This is used to describe the type of a given surface. The
      surface types are also known as "backends" or "surface backends"
      within cairo.

      The type of a surface is determined by the function used to
      create it, which will generally be of the form [cairo_]{i
      type}[_surface_create] (though see {!Cairo.Surface.create_similar}
      as well).  *)
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

  external get_type : t -> kind = "caml_cairo_surface_get_type"
      (** This function returns the type of the backend used to create
          a surface.  See {!Cairo.Surface.kind} for available types. *)

  external copy_page : t -> unit = "caml_cairo_surface_copy_page"
      (** Emits the current page for backends that support multiple
          pages, but doesn't clear it, so that the contents of the
          current page will be retained for the next page.  Use
          {!Cairo.Surface.show_page} if you want to get an empty page
          after the emission.

          There is a convenience function for this that takes a
          {Cairo.context}, namely {!Cairo.copy_page}. *)

  external show_page : t -> unit = "caml_cairo_surface_show_page"
      (** Emits and clears the current page for backends that support
          multiple pages.  Use {!Cairo.Surface.copy_page} if you don't
          want to clear the page.

          There is a convenience function for this that takes a
          {!Cairo.context}, namely {!Cairo.show_page}.  *)

  external has_show_text_glyphs : t -> bool
    = "caml_cairo_surface_has_show_text_glyphs"
      (** Returns whether the surface supports sophisticated
          {!Cairo.Glyph.show_text} operations.  That is, whether it
          actually uses the provided text and cluster data to a
          {!Cairo.Glyph.show_text} call.

          Note: Even if this function returns [false], a
          {!Cairo.Glyph.show_text} operation targeted at surface will
          still succeed.  It just will act like a {!Cairo.Glyph.show}
          operation.  Users can use this function to avoid computing
          UTF-8 text and cluster mapping if the target surface does
          not use it.  *)
end

(** {3:surface_backends   Surface backends}

    Below are the surface backends that do not depend of a particular
    platform.  {!XLib}, {!Win32}, and {!Quartz} are defined in their
    own modules.
*)

(** Image surfaces provide the ability to render to memory buffers
    either allocated by cairo or by the calling code. The supported image
    formats are those defined in {!Cairo.Image.format}.  *)
module Image :
sig
  (** This is used to identify the memory format of image data.  *)
  type format =
    | ARGB32 (** each pixel is a 32-bit quantity, with alpha in the
                 upper 8 bits, then red, then green, then blue. The
                 32-bit quantities are stored native-endian.
                 Pre-multiplied alpha is used.  (That is, 50%
                 transparent red is 0x80800000, not 0x80ff0000.) *)
    | RGB24 (** each pixel is a 32-bit quantity, with the upper 8 bits
                unused. Red, Green, and Blue are stored in the
                remaining 24 bits in that order. *)
    | A8 (** each pixel is a 8-bit quantity holding an alpha value. *)
    | A1 (** each pixel is a 1-bit quantity holding an alpha
             value. Pixels are packed together into 32-bit quantities.
             The ordering of the bits matches the endianess of the
             platform.  On a big-endian machine, the first pixel is in
             the uppermost bit, on a little-endian machine the first
             pixel is in the least-significant bit. *)

  external create : format -> width:int -> height:int -> Surface.t
    = "caml_cairo_image_surface_create"
      (** Creates an image surface of the specified format and
          dimensions. Initially the surface contents are all 0.
          (Specifically, within each pixel, each color or alpha
          channel belonging to format will be 0. The contents of bits
          within a pixel, but not belonging to the given format are
          undefined). *)

  type data8 =
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        (** Images represented as an array of 8 bytes values. *)

  type data32 =
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
        (** Images represented as an array of 32 bytes (RGB or RGBA) values. *)

  val create_for_data8 : data8 ->
    format -> ?stride:int -> int -> int -> Surface.t
    (** [create_for_data8 data format ?stride width height] creates an
        image surface for the provided pixel data.  The initial
        contents of buffer will be used as the initial image contents;
        you must explicitly clear the buffer, using, for example,
        {!Cairo.rectangle} and {!Cairo.fill} if you want it
        cleared. *)

  val create_for_data32 : ?width:int -> ?height:int -> ?alpha:bool ->
    data32 -> Surface.t
    (** [create_for_data32 ?width ?height ?alpha data] same as
        {!Cairo.Image.create_for_data8} except that the stride will
        necessarily be according to the bigarray 1st dimension (so
        that matrix coordinates correspond to pixels) and the [width]
        and [height] will be by default taken from the bigarray 1st
        and 2nd dimensions respectively.  If [alpha] is true (default),
        the [ARGB32] format is selected, otherwise [RGB24] is used. *)

  val get_data8 : Surface.t -> data8
    (** Get the data of the image surface (shared), for direct
        inspection or modification. *)

  val get_data32 : Surface.t -> data32
    (** Get the data of the image surface (shared), for direct
        inspection or modification.  The 1st (resp. 2nd) dimension of
        the bigarray correspond to the width (resp. height) of the
        surface.

        @raise Invalid_argument if the format is not [ARGB32] or
        [RGB24] because the array dimensions would not reflect the
        image coordinates.  *)

  external get_format : Surface.t -> format
    = "caml_cairo_image_surface_get_format"
    (** Get the format of the surface. *)

  external get_width : Surface.t -> int = "caml_cairo_image_surface_get_width"
      (** Get the width of the image surface in pixels. *)

  external get_height : Surface.t -> int = "caml_cairo_image_surface_get_height"
      (** Get the height of the image surface in pixels. *)

  external get_stride : Surface.t -> int = "caml_cairo_image_surface_get_stride"
      (** Get the stride of the image surface in bytes. *)

  val output_ppm : out_channel -> ?width:int -> ?height:int -> data32 -> unit
    (** Convenience function to write the subarray of size ([width],
        [height]) representing an image to the PPM format.  The
        possible alpha channel is ignored. *)
end

(** The PDF surface is used to render cairo graphics to Adobe PDF
    files and is a multi-page vector surface backend. *)
module PDF :
sig
  val create : fname:string -> width:float -> height:float -> Surface.t
    (** Creates a PDF surface of the specified size in points to be
        written to [fname].

        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  val create_for_stream : output:(string -> unit) ->
    width:float -> height:float -> Surface.t
    (** Creates a PDF surface of the specified size in points to be
        written incrementally to the stream represented by [output].
        Any exception that [output] raises is considered as a write
        error.

        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  external set_size : Surface.t -> width:float -> height:float -> unit
    = "caml_cairo_pdf_surface_set_size" "noalloc"
    (** Changes the size of a PDF surface for the current (and
        subsequent) pages.
        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)

        This function should only be called before any drawing
        operations have been performed on the current page.  The
        simplest way to do this is to call this function immediately
        after creating the surface or immediately after completing a
        page with either {!Cairo.show_page} or {!Cairo.copy_page}. *)
end

(** The PNG functions allow reading PNG images into image surfaces,
    and writing any surface to a PNG file.  *)
module PNG :
sig
  external create : string -> Surface.t
    = "caml_cairo_image_surface_create_from_png"
    (** [create filename] creates a new image surface and initializes
        the contents to the given PNG file. *)

  external create_from_stream : input:(string -> int -> unit) -> Surface.t
    = "caml_cairo_image_surface_create_from_png_stream"
    (** Creates a new image surface from PNG data read incrementally
        via the [input] function.  The [input s l] function receives a
        string [s] whose first [l] bytes must be filled with PNG data.
        Any exception raised by [input] is considered as a read
        error.  *)

  external write : Surface.t -> string -> unit
    = "caml_cairo_surface_write_to_png"
    (** [write surface filename] writes the contents of [surface] to a
        new file [filename] as a PNG image. *)

  external write_to_stream : Surface.t -> output:(string -> unit) -> unit
    = "caml_cairo_surface_write_to_png_stream"
    (** Writes the image surface using the [output] function. *)
end

(** The PostScript surface is used to render cairo graphics to Adobe
    PostScript files and is a multi-page vector surface backend.  *)
module PS :
sig
  external create : fname:string -> width:float -> height:float -> Surface.t
    = "caml_cairo_ps_surface_create"
    (** Creates a PostScript surface of the specified size in points
        to be written to [fname].
        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  external create_for_stream : output:(string -> unit) ->
    width:float -> height:float -> Surface.t
    = "caml_cairo_ps_surface_create_for_stream"
    (** Creates a PostScript surface of the specified size in points to be
        written incrementally to the stream represented by [output].
        Any exception that [output] raises is considered as a write
        error.

        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  (** Describe the language level of the PostScript Language Reference
      that a generated PostScript file will conform to. *)
  type level = LEVEL_2 | LEVEL_3

  external restrict_to_level : Surface.t -> level -> unit
    = "caml_cairo_ps_surface_restrict_to_level"
      (** [restrict_to_level level] restricts the generated PostSript
          file to [level].  See {!Cairo.PS.get_levels} for a list of
          available level values that can be used here.

          This function should only be called before any drawing
          operations have been performed on the given surface. The
          simplest way to do this is to call this function immediately
          after creating the surface.  *)

  external get_levels : unit -> level list
    = "caml_cairo_ps_get_levels"
      (** Retrieves the list of supported levels. *)

  external level_to_string : level -> string = "caml_cairo_ps_level_to_string"
      (** [level_to_string level] return the string representation of
          the given [level] id. *)

  external set_eps : Surface.t -> eps:bool -> unit
    = "caml_cairo_ps_surface_set_eps"
      (** If [eps] is [true], the PostScript surface will output
          Encapsulated PostScript.

          This function should only be called before any drawing
          operations have been performed on the current page. The
          simplest way to do this is to call this function immediately
          after creating the surface. An Encapsulated PostScript file
          should never contain more than one page. *)

  external get_eps : Surface.t -> bool = "caml_cairo_ps_surface_get_eps"
      (** Check whether the PostScript surface will output
          Encapsulated PostScript.  *)

  external set_size : Surface.t -> width:float -> height:float -> unit
    = "caml_cairo_ps_surface_set_size"
      (** Changes the size of a PostScript surface for the current
          (and subsequent) pages.

          This function should only be called before any drawing
          operations have been performed on the current page. The
          simplest way to do this is to call this function immediately
          after creating the surface or immediately after completing a
          page with either {!Cairo.show_page} or {!Cairo.copy_page}. *)

  (** PostScript comments. *)
  module Dsc :
  sig
    external begin_setup : Surface.t -> unit
      = "caml_cairo_ps_surface_dsc_begin_setup"
      (** This function indicates that subsequent calls to
          {!Cairo.PS.Dsc.comment} should direct comments to the Setup
          section of the PostScript output.

          This function should be called at most once per surface, and
          must be called before any call to
          {!Cairo.PS.Dsc.begin_page_setup} and before any drawing is
          performed to the surface.

          See {!Cairo.PS.Dsc.comment} for more details. *)

    external begin_page_setup : Surface.t -> unit
      = "caml_cairo_ps_surface_dsc_begin_page_setup"
      (** This function indicates that subsequent calls to
          {!Cairo.PS.Dsc.comment} should direct comments to the
          PageSetup section of the PostScript output.

          This function call is only needed for the first page of a
          surface.  It should be called after any call to
          {!Cairo.PS.Dsc.begin_setup} and before any drawing is
          performed to the surface.

          See {!Cairo.PS.Dsc.comment} for more details. *)

    external comment : Surface.t -> string -> unit
      = "caml_cairo_ps_surface_dsc_comment"
      (** Emit a comment into the PostScript output for the given surface.

          The comment is expected to conform to the PostScript
          Language Document Structuring Conventions (DSC). Please see
          that manual for details on the available comments and their
          meanings. In particular, the [%IncludeFeature] comment allows
          a device-independent means of controlling printer device
          features. So the PostScript Printer Description Files
          Specification will also be a useful reference.

          The comment string must begin with a percent character (%)
          and the total length of the string (including any initial
          percent characters) must not exceed 255 characters.
          Violating either of these conditions will raise en
          exception.  But beyond these two conditions, this function
          will not enforce conformance of the comment with any
          particular specification.

          The comment string should not have a trailing newline.

          The DSC specifies different sections in which particular
          comments can appear. This function provides for comments to
          be emitted within three sections: the header, the Setup
          section, and the PageSetup section. Comments appearing in
          the first two sections apply to the entire document while
          comments in the BeginPageSetup section apply only to a
          single page.

          For comments to appear in the header section, this function
          should be called after the surface is created, but before a
          call to {!Cairo.PS.Dsc.begin_setup}.

          For comments to appear in the Setup section, this function
          should be called after a call to {!Cairo.PS.Dsc.begin_setup}
          but before a call to {!Cairo.PS.Dsc.begin_page_setup}.

          For comments to appear in the PageSetup section, this
          function should be called after a call to
          {!Cairo.PS.Dsc.begin_page_setup}.

          Note that it is only necessary to call
          {!Cairo.PS.Dsc.begin_page_setup} for the first page of
          any surface.  After a call to {!Cairo.show_page} or
          {!Cairo.copy_page} comments are unambiguously directed to the
          PageSetup section of the current page.  But it doesn't hurt
          to call this function at the beginning of every page as that
          consistency may make the calling code simpler.

          As a final note, cairo automatically generates several
          comments on its own.  As such, applications must not
          manually generate any of the following comments:

          - Header section: %!PS-Adobe-3.0, %Creator, %CreationDate, %Pages,
            %BoundingBox, %DocumentData, %LanguageLevel, %EndComments.
          - Setup section: %BeginSetup, %EndSetup
          - PageSetup section: %BeginPageSetup, %PageBoundingBox,
            %EndPageSetup.
          - Other sections: %BeginProlog, %EndProlog, %Page, %Trailer, %EOF

          Here is an example sequence showing how this function might
          be used:
          {[
          let surface = Cairo.PS.create filename width height in
          ...
          Cairo.PS.Dsc.comment surface "%%Title: My excellent document";
          Cairo.PS.Dsc.comment surface
            "%%Copyright: Copyright (C) 2006 Cairo Lover";
          ...
          Cairo.PS.Dsc.begin_setup surface;
          Cairo.PS.Dsc.comment surface "%%IncludeFeature: *MediaColor White";
          ...
          Cairo.PS.Dsc.begin_page_setup surface;
          Cairo.PS.Dsc.comment surface "%%IncludeFeature: *PageSize A3";
          Cairo.PS.Dsc.comment surface
            "%%IncludeFeature: *InputSlot LargeCapacity";
          Cairo.PS.Dsc.comment surface "%%IncludeFeature: *MediaType Glossy";
          Cairo.PS.Dsc.comment surface "%%IncludeFeature: *MediaColor Blue";
          ... (* draw to first page here *) ...
          Cairo.show_page cr;
          ...
          Cairo.PS.Dsc.comment surface "%%IncludeFeature: *PageSize A5";
          ...
          ]}
      *)
  end
end

(** The SVG surface is used to render cairo graphics to SVG files and
    is a multi-page vector surface backend.  *)
module SVG :
sig
  external create : fname:string -> width:float -> height:float -> Surface.t
    = "caml_cairo_svg_surface_create"
    (** Creates a SVG surface of the specified size in points to be
        written to [fname].
        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  external create_for_stream : output:(string -> unit) ->
    width:float -> height:float -> Surface.t
    = "caml_cairo_svg_surface_create_for_stream"
    (** Creates a SVG surface of the specified size in points to be
        written incrementally to the stream represented by [output].
        Any exception that [output] raises is considered as a write
        error.

        @param width width of the surface, in points (1 point = 1/72.0 inch)
        @param height height of the surface, in points (1 point = 1/72.0 inch)
    *)

  (** The version number of the SVG specification that a generated SVG
      file will conform to. *)
  type version = VERSION_1_1 | VERSION_1_2

  external restrict_to_version : Surface.t -> version -> unit
    = "caml_cairo_svg_surface_restrict_to_version"
      (** Restricts the generated SVG file to version. See
          {!Cairo.SVG.get_versions} for a list of available version
          values that can be used here.

          This function should only be called before any drawing
          operations have been performed on the given surface. The
          simplest way to do this is to call this function immediately
          after creating the surface. *)

  external get_versions : unit -> version list
    = "caml_cairo_svg_get_versions"
      (** Retrieve the list of supported versions.  *)

  external version_to_string : version -> string
    = "caml_cairo_svg_version_to_string"
      (** Get the string representation of the given version id. *)
end

(** The recording surface is a surface that records all drawing operations at
    the highest level of the surface backend interface.  The surface can then
    be "replayed" against any target surface by using it as a source
    surface.

    A recording surface is logically unbounded, i.e. it has no
    implicit constraint on the size of the drawing surface.  *)
module Recording :
sig
  external create : ?extents:rectangle -> content -> Surface.t
    = "caml_cairo_recording_surface_create"
  (** Creates a recording surface with the specified [content].  It
      can record all drawing operations at the highest level (that is,
      the level of {!paint}, {!mask}, {!stroke}, {!fill} and
      {!Glyph.show_text}).  The surface is unbounded (no constraints
      on the size of the drawing surface) unless the [extents]
      argument is provided.  Copying to another surface (see
      {!set_source_surface}) will be more efficient (and sometime
      required) if the extents are specified when the recording
      surface is created. *)

  external ink_extents : Surface.t -> rectangle
    = "caml_cairo_recording_surface_ink_extents"
  (** Measures the extents of the operations stored within the recording
      surface.  This is useful to compute the required size of another
      drawing surface into which to replay the full sequence of drawing
      operations. *)
end


(* ---------------------------------------------------------------------- *)
(** {2 Sources for drawing} *)

(** Paint (and also mask and brush) with which cairo draws and
    associated function. *)
module Pattern :
sig
  type 'a t constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]
    (** This is the paint with which cairo draws.  The primary use of
        patterns is as the source for all cairo drawing operations,
        although they can also be used as masks, that is, as the brush
        too.

        A cairo pattern is created by using one of the many functions,
        of the form [Cairo.Pattern.create_type] or implicitly through
        [Cairo.set_source_*] functions.  *)

  type any = [`Solid | `Surface | `Gradient | `Linear | `Radial] t
      (** {!Cairo.Group.pop} and {!Cairo.get_source} retrieve patterns
          whose properties we do not know.  In this case, we can only
          assume the pattern has potentially all properties and the
          functions below may raise an exception if it turns out that
          the needed property is not present. *)

  val add_color_stop_rgb : [> `Gradient] t -> ?ofs:float ->
    float -> float -> float -> unit
    (** Adds an opaque color stop to a gradient pattern.  The offset
        [ofs] specifies the location along the gradient's control
        vector (default: [0.0]).  For example, a linear gradient's
        control vector is from (x0,y0) to (x1,y1) while a radial
        gradient's control vector is from any point on the start
        circle to the corresponding point on the end circle.

        The color is specified in the same way as in {!Cairo.set_source_rgb}.

        If two (or more) stops are specified with identical offset
        values, they will be sorted according to the order in which
        the stops are added (stops added earlier will compare less
        than stops added later).  This can be useful for reliably
        making sharp color transitions instead of the typical blend. *)

  val add_color_stop_rgba : [> `Gradient] t -> ?ofs:float ->
    float -> float -> float -> float -> unit
    (** Adds a translucent color stop to a gradient pattern. The
        offset specifies the location along the gradient's control
        vector. For example, a linear gradient's control vector is from
        (x0,y0) to (x1,y1) while a radial gradient's control vector is
        from any point on the start circle to the corresponding point on
        the end circle.

        The color is specified in the same way as in {!Cairo.set_source_rgba}.

        If two (or more) stops are specified with identical offset
        values, they will be sorted according to the order in which
        the stops are added (stops added earlier will compare less
        than stops added later). This can be useful for reliably
        making sharp color transitions instead of the typical
        blend.  *)

  external get_color_stop_count : [> `Gradient] t -> int
    = "caml_cairo_pattern_get_color_stop_count"
    (** Return the number of color stops specified in the given
        gradient pattern. *)

  val get_color_stop_rgba : [> `Gradient] t -> idx:int ->
    float * float * float * float * float
      (** Gets the color and offset information at the given index for
          a gradient pattern. Values of index are 0 to 1 less than the
          number returned by {!Cairo.Pattern.get_color_stop_count}.

          @return (offset, red, green, blue, alpha) *)

  external create_rgb : r:float -> g:float -> b:float -> [`Solid] t
    = "caml_cairo_pattern_create_rgb"
    (** Creates a new {!Cairo.Pattern.t} corresponding to an opaque
        color. The color components are floating point numbers in the
        range 0 to 1. If the values passed in are outside that range,
        they will be clamped. *)

  external create_rgba : r:float -> g:float -> b:float -> a:float -> [`Solid] t
    = "caml_cairo_pattern_create_rgba"
    (** Creates a new {!Cairo.Pattern.t} corresponding to a
        translucent color.  The color components are floating point
        numbers in the range 0 to 1.  If the values passed in are
        outside that range, they will be clamped. *)

  external get_rgba : [> `Solid] t -> float * float * float * float
    = "caml_cairo_pattern_get_rgba"
    (** Return the solid color for a solid color pattern.

    @return (red, green, blue, alpha) *)

  external create_for_surface : Surface.t -> [`Surface] t
    = "caml_cairo_pattern_create_for_surface"
    (** Create a new {!Cairo.Pattern.t} for the given surface. *)

  external get_surface : [`Surface] t -> Surface.t
    = "caml_cairo_pattern_get_surface"
    (** Gets the surface of a surface pattern.  *)

  external create_linear : x0:float -> y0:float -> x1:float -> y1:float ->
    [`Linear | `Gradient] t = "caml_cairo_pattern_create_linear"
      (** Create a new linear gradient {!Cairo.Pattern.t} along the
          line defined by (x0, y0) and (x1, y1).  Before using the
          gradient pattern, a number of color stops should be defined
          using {!Cairo.Pattern.add_color_stop_rgb} or
          {!Cairo.Pattern.add_color_stop_rgba}.

          Note: The coordinates here are in pattern space. For a new
          pattern, pattern space is identical to user space, but the
          relationship between the spaces can be changed with
          {!Cairo.Pattern.set_matrix}.  *)

  external get_linear_points : [> `Linear|`Gradient] t
    -> float * float * float * float = "caml_cairo_pattern_get_linear_points"
    (** Gets the gradient endpoints for a linear gradient.
        @return (x0, y0, x1, y1). *)

  external create_radial : x0:float -> y0:float -> r0:float ->
    x1:float -> y1:float -> r1:float -> [`Radial | `Gradient] t
    = "caml_cairo_pattern_create_radial_bc" "caml_cairo_pattern_create_radial"
    (** Creates a new radial gradient {!Cairo.Pattern.t} between the
        two circles defined by (cx0, cy0, radius0) and (cx1, cy1,
        radius1). Before using the gradient pattern, a number of color
        stops should be defined using {!Cairo.Pattern.add_color_stop_rgb} or
        {!Cairo.Pattern.add_color_stop_rgba}.

        Note: The coordinates here are in pattern space. For a new
        pattern, pattern space is identical to user space, but the
        relationship between the spaces can be changed with
        {!Cairo.Pattern.set_matrix}. *)

  external get_radial_circles : [> `Radial|`Gradient] t ->
    float * float * float * float * float * float
    = "caml_cairo_pattern_get_radial_circles"
      (** Gets the gradient endpoint circles for a radial gradient,
          each specified as a center coordinate and a radius.
          @return (x0, y0, r0, x1, y1, r1).      *)

  (** This is used to describe how pattern color/alpha will be
      determined for areas "outside" the pattern's natural area (for
      example, outside the surface bounds or outside the gradient
      geometry).  *)
  type extend =
    | NONE    (** pixels outside of the source pattern are fully transparent. *)
    | REPEAT  (** the pattern is tiled by repeating. *)
    | REFLECT (** the pattern is tiled by reflecting at the edges. *)
    | PAD (** pixels outside of the pattern copy the closest pixel
              from the source. *)

  external set_extend : 'a t -> extend -> unit
    = "caml_cairo_pattern_set_extend" "noalloc"
    (** Sets the mode to be used for drawing outside the area of a
        pattern.  See {!Cairo.Pattern.extend} for details on the
        semantics of each extend strategy.

        The default extend mode is [NONE] for surface patterns and
        [PAD] for gradient patterns. *)

  external get_extend : 'a t -> extend = "caml_cairo_pattern_get_extend"
    (** Gets the current extend mode for a pattern. See
        {!Cairo.Pattern.extend} for details on the semantics of each
        extend strategy. *)

  (** This is used to indicate what filtering should be applied when
      reading pixel values from patterns.  See
      {!Cairo.Pattern.set_filter} for indicating the desired filter to
      be used with a particular pattern. *)
  type filter =
    | FAST (** A high-performance filter, with quality similar to NEAREST *)
    | GOOD (** A reasonable-performance filter, with quality similar
               to BILINEAR *)
    | BEST (** The highest-quality available, performance may not be
               suitable for interactive use. *)
    | NEAREST (** Nearest-neighbor filtering *)
    | BILINEAR (** Linear interpolation in two dimensions *)
    (* | GAUSSIAN *)

  external set_filter : 'a t -> filter -> unit
    = "caml_cairo_pattern_set_filter" "noalloc"
    (** Sets the filter to be used for resizing when using this
        pattern. See {!Cairo.Pattern.filter} for details on each
        filter.

        Note that you might want to control filtering even when you do
        not have an explicit {!Cairo.Pattern.t} value (for example
        when using {!Cairo.set_source_surface}).  In these cases, it
        is convenient to use {!Cairo.get_source} to get access to the
        pattern that cairo creates implicitly. For example:
        {[
        Cairo.set_source_surface cr image x y;
        Cairo.Pattern.set_filter (Cairo.get_source cr) Cairo.Pattern.NEAREST;
        ]} *)

  external get_filter : 'a t -> filter = "caml_cairo_pattern_get_filter"
    (** Gets the current filter for a pattern.  See
        {!Cairo.Pattern.filter} for details on each filter. *)

  external set_matrix : 'a t -> Matrix.t -> unit
    = "caml_cairo_pattern_set_matrix" "noalloc"
    (** Sets the pattern's transformation matrix to matrix. This
        matrix is a transformation from user space to pattern space.

        When a pattern is first created it always has the identity
        matrix for its transformation matrix, which means that pattern
        space is initially identical to user space.

        Important: Please note that the direction of this
        transformation matrix is from user space to pattern
        space. This means that if you imagine the flow from a pattern
        to user space (and on to device space), then coordinates in
        that flow will be transformed by the inverse of the pattern
        matrix.

        For example, if you want to make a pattern appear twice as
        large as it does by default the correct code to use is:
        {[
        let matrix = Cairo.Matrix.init_scale 0.5 0.5 in
        Cairo.Pattern.set_matrix pattern matrix;
        ]} *)

  external get_matrix : 'a t -> Matrix.t = "caml_cairo_pattern_get_matrix"
    (** Returns the pattern's transformation matrix.  *)

  (* FIXME: is get_type needed ? *)
end


(* ---------------------------------------------------------------------- *)
(** {2:cairo_t The cairo drawing context functions} *)

external create : Surface.t -> context = "caml_cairo_create"
  (** [create target] creates a new context with all graphics state
      parameters set to default values and with [target] as a target
      surface. The target surface should be constructed with a
      backend-specific function such as {!Cairo.Image.create} (or any
      other {i Backend}[.create] variant).  For many backends, you
      should not forget to call {!Cairo.Surface.finish} for the data
      to be completely outputted.

      @raise Out_of_memory if the context could not be allocated. *)

external save : context -> unit = "caml_cairo_save"
  (** [save cr] makes a copy of the current state of [cr] and saves it
      on an internal stack of saved states for [cr].  When [restore]
      is called, [cr] will be restored to the saved state.  Multiple
      calls to [save] and [restore] can be nested; each call to
      [restore] restores the state from the matching paired [save].  *)
external restore : context -> unit = "caml_cairo_restore"
  (** [restore cr] restores [cr] to the state saved by a preceding
      call to [save] and removes that state from the stack of saved
      states. *)

external get_target : context -> Surface.t = "caml_cairo_get_target"
  (** Gets the target surface for the cairo context as passed to [create]. *)

(** Temporary redirection of drawing commands to intermediate
    surfaces. *)
module Group :
sig
  val push : ?content:content -> context -> unit
    (** Temporarily redirects drawing to an intermediate surface known
        as a group.  The redirection lasts until the group is completed
        by a call to {!Cairo.Group.pop} or {!Cairo.Group.pop_to_source}.
        These calls provide the result of any drawing to the group as a
        pattern (either as an explicit object, or set as the source
        pattern).

        This group functionality can be convenient for performing
        intermediate compositing. One common use of a group is to render
        objects as opaque within the group (so that they occlude each
        other) and then blend the result with translucence onto the
        destination.

        Groups can be nested arbitrarily deep by making balanced calls
        to [Group.push]/[Group.pop]. Each call pushes/pops the new
        target group onto/from a stack.

        The [Group.push] function calls [save] so that any changes
        to the graphics state will not be visible outside the group,
        (the [Group.pop] function call [restore]).

        @param content The content type of the group.  By default the
        intermediate group will have a content type of [COLOR_ALPHA]
        (see {!Cairo.content}).  *)

  external pop : context -> Pattern.any = "caml_cairo_pop_group"
    (** Terminates the redirection begun by a call to
        {!Cairo.Group.push} and returns a new pattern containing the
        results of all drawing operations performed to the group.

        The [Group.pop] function calls {!Cairo.restore} (balancing a
        call to {!Cairo.save} by the [Group.push] function), so that any
        changes to the graphics state will not be visible outside the
        group.

        @return a newly created (surface) pattern containing the results
        of all drawing operations performed to the group.  *)

  external pop_to_source : context -> unit = "caml_cairo_pop_group_to_source"
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

  external get_target : context -> Surface.t = "caml_cairo_get_group_target"
    (** Gets the current destination surface for the context.  This is
        either the original target surface as passed to [create] or
        the target surface for the current group as started by the most
        recent call to [Group.push]. *)
end

external set_source_rgb : context -> r:float -> g:float -> b:float -> unit
  = "caml_cairo_set_source_rgb"
  (** [set_source_rgb cr r g b] sets the source pattern within [cr] to
      an opaque color.  This opaque color will then be used for any
      subsequent drawing operation until a new source pattern is set.

      The color components are floating point numbers in the range 0 to
      1. If the values passed in are outside that range, they will be
      clamped.

      The default source pattern is opaque black (that is, it is
      equivalent to [set_source_rgb cr 0. 0. 0.]). *)

external set_source_rgba : context ->
  r:float -> g:float -> b:float -> a:float -> unit
  = "caml_cairo_set_source_rgba"
  (** [set_source_rgba cr r g b a] sets the source pattern within [cr]
      to a translucent color.  This color will then be used for any
      subsequent drawing operation until a new source pattern is set.

      The color and alpha components are floating point numbers in the
      range 0 to 1. If the values passed in are outside that range,
      they will be clamped.

      The default source pattern is opaque black (that is, it is
      equivalent to [set_source_rgba cr 0. 0. 0. 1.0]). *)

external set_source : context -> 'a Pattern.t -> unit = "caml_cairo_set_source"
  (** [set_source cr source] sets the source pattern within [cr] to
      [source].  This pattern will then be used for any subsequent
      drawing operation until a new source pattern is set.

      Note: The pattern's transformation matrix will be locked to the
      user space in effect at the time of [set_source].  This means
      that further modifications of the current transformation matrix
      will not affect the source pattern. See {!Pattern.set_matrix}.

      The default source pattern is a solid pattern that is opaque
      black (that is, it is equivalent to [set_source_rgb cr 0. 0. 0.]). *)

external set_source_surface : context -> Surface.t -> x:float -> y:float -> unit
  = "caml_cairo_set_source_surface"
  (** [set_source_surface cr surface x y] is a convenience for
      creating a pattern from surface and setting it as the source in [cr]
      with [set_source].

      The [x] and [y] parameters give the user-space coordinate at
      which the surface origin should appear.  (The surface origin is
      its upper-left corner before any transformation has been
      applied.)  The x and y patterns are negated and then set as
      translation values in the pattern matrix.

      Other than the initial translation pattern matrix, as described
      above, all other pattern attributes (such as its extend mode)
      are set to the default values as in {!Pattern.create_for_surface}.
      The resulting pattern can be queried with {!Cairo.get_source} so
      that these attributes can be modified if desired (e.g. to create
      a repeating pattern with {!Cairo.Pattern.set_extend}). *)

external get_source : context -> Pattern.any = "caml_cairo_get_source"
  (** [get_source cr] gets the current source pattern for [cr]. *)

external set_antialias : context -> antialias -> unit
  = "caml_cairo_set_antialias"
    (** Set the antialiasing mode of the rasterizer used for drawing
        shapes.  This value is a hint, and a particular backend may or
        may not support a particular value.  At the current time, no
        backend supports [ANTIALIAS_SUBPIXEL] when drawing shapes.

        Note that this option does not affect text rendering, instead
        see {!Cairo.Font_options.set_antialias}. *)

external get_antialias : context -> antialias = "caml_cairo_get_antialias"
  (** Gets the current shape antialiasing mode, as set by
      {!Cairo.set_antialias}. *)

val set_dash : context -> ?ofs:float -> float array -> unit
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

external get_dash : context -> float array * float = "caml_cairo_get_dash"
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

external set_fill_rule : context -> fill_rule -> unit
  = "caml_cairo_set_fill_rule"
    (** [set_fill_rule cr fill_rule] sets the current fill rule within
        the cairo context [cr].  The fill rule is used to determine
        which regions are inside or outside a complex (potentially
        self-intersecting) path. The current fill rule affects both
        {!Cairo.fill} and {!Cairo.clip}.  See {!Cairo.fill_rule} for
        details on the semantics of each available fill rule.  *)

external get_fill_rule : context -> fill_rule = "caml_cairo_get_fill_rule"
    (** Gets the current fill rule, as set by [set_fill_rule]. *)


(** Specifies how to render the endpoints of the path when stroking.
    The default line cap style is [BUTT].  *)
type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

external set_line_cap : context -> line_cap -> unit = "caml_cairo_set_line_cap"
    (** [set_line_cap cr line_cap] sets the current line cap style
        within the cairo context [cr].  See {!Cairo.line_cap} for
        details about how the available line cap styles are drawn.

        As with the other stroke parameters, the current line cap
        style is examined by {!Cairo.stroke}, {!Cairo.stroke_extents},
        and {!Cairo.stroke_to_path}, but does not have any effect
        during path construction.

        The default line cap style is [BUTT].  *)

external get_line_cap : context -> line_cap = "caml_cairo_get_line_cap"
    (** Gets the current line cap style, as set by {!Cairo.set_line_cap}. *)


(** Specifies how to render the junction of two lines when stroking.
    The default line join style is [MITER]. *)
type line_join =
  | JOIN_MITER (** use a sharp (angled) corner, see {!Cairo.set_miter_limit} *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

external set_line_join : context -> line_join -> unit
  = "caml_cairo_set_line_join"
    (** Sets the current line join style within the cairo context.
        See {!Cairo.line_join} for details about how the available
        line join styles are drawn.

        As with the other stroke parameters, the current line join
        style is examined by {!Cairo.stroke}, {!Cairo.stroke_extents},
        and {!Cairo.stroke_to_path}, but does not have any effect
        during path construction.

        The default line join style is [MITER]. *)
external get_line_join : context -> line_join = "caml_cairo_get_line_join"
    (** Gets the current line join style, as set by {!Cairo.set_line_join}. *)


external set_line_width : context -> float -> unit = "caml_cairo_set_line_width"
    (** Sets the current line width within the cairo context. The line
        width value specifies the diameter of a pen that is circular in
        user space (though device-space pen may be an ellipse in general
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

external get_line_width : context -> float = "caml_cairo_get_line_width"
    (** This function returns the current line width value exactly as
        set by {!Cairo.set_line_width}.  Note that the value is
        unchanged even if the CTM has changed between the calls to
        [set_line_width] and [get_line_width]. *)

external set_miter_limit : context -> float -> unit
  = "caml_cairo_set_miter_limit"
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

external get_miter_limit : context -> float = "caml_cairo_get_miter_limit"
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

external set_operator : context -> operator -> unit = "caml_cairo_set_operator"
    (** Sets the compositing operator to be used for all drawing
        operations.  See {!Cairo.operator} for details on the
        semantics of each available compositing operator.

        The default operator is [OVER]. *)

external get_operator : context -> operator = "caml_cairo_get_operator"
    (** Gets the current compositing operator for a cairo context.  *)

external set_tolerance : context -> float -> unit = "caml_cairo_set_tolerance"
    (** Sets the tolerance used when converting paths into trapezoids.
        Curved segments of the path will be subdivided until the
        maximum deviation between the original path and the polygonal
        approximation is less than tolerance.  The default value is
        [0.1].  A larger value will give better performance, a smaller
        value, better appearance.  (Reducing the value from the
        default value of [0.1] is unlikely to improve appearance
        significantly.)  *)
external get_tolerance : context -> float = "caml_cairo_get_tolerance"
    (** Gets the current tolerance value, as set by {!Cairo.set_tolerance}. *)


external clip : context -> unit = "caml_cairo_clip"
  (** Establishes a new clip region by intersecting the current clip
      region with the current path as it would be filled by
      {!Cairo.fill} and according to the current fill rule (see
      {!Cairo.set_fill_rule}).

      After [clip], the current path will be cleared from the cairo
      context.

      Calling {!Cairo.clip} can only make the clip region smaller,
      never larger.  But the current clip is part of the graphics
      state, so a temporary restriction of the clip region can be
      achieved by calling {!Cairo.clip} within a {!Cairo.save} /
      {!Cairo.restore} pair. The only other means of increasing the
      size of the clip region is {!Cairo.clip_reset}.  *)

external clip_preserve : context -> unit = "caml_cairo_clip_preserve"
  (** Establishes a new clip region by intersecting the current clip
      region with the current path as it would be filled by
      {!Cairo.fill} and according to the current fill rule (see
      {!Cairo.set_fill_rule}).

      Unlike {!Cairo.clip}, preserves the path within the cairo context.

      Calling {!Cairo.clip_preserve} can only make the clip region
      smaller, never larger.  But the current clip is part of the
      graphics state, so a temporary restriction of the clip region
      can be achieved by calling {!Cairo.clip_preserve} within a
      {!Cairo.save} / {!Cairo.restore} pair. The only other means of
      increasing the size of the clip region is
      {!Cairo.clip_reset}.  *)

external clip_extents : context -> rectangle = "caml_cairo_clip_extents"
  (** Computes a bounding box in user coordinates covering the area
      inside the current clip.  *)

external clip_reset : context -> unit = "caml_cairo_reset_clip"
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

external clip_rectangle_list : context -> rectangle list
  = "caml_cairo_copy_clip_rectangle_list"
  (** Gets the current clip region as a list of rectangles in user
      coordinates.

      Raises [Error(CLIP_NOT_REPRESENTABLE)] to indicate that the clip
      region cannot be represented as a list of user-space rectangles.  *)


external fill : context -> unit = "caml_cairo_fill"
  (** A drawing operator that fills the current path according to the
      current fill rule (each sub-path is implicitly closed before
      being filled).  After [fill], the current path will be cleared
      from the cairo context.

      See also {!Cairo.set_fill_rule}. *)

external fill_preserve : context -> unit = "caml_cairo_fill_preserve"
  (** A drawing operator that fills the current path according to
      the current fill rule (each sub-path is implicitly closed
      before being filled).  Unlike {!Cairo.fill}, [fill_preserve]
      preserves the path within the cairo context.

      See also {!Cairo.set_fill_rule}. *)

external fill_extents : context -> rectangle = "caml_cairo_fill_extents"
  (** Computes a bounding box in user coordinates covering the area
      that would be affected (the "inked" area) by a [fill]
      operation given the current path and fill parameters.  If the
      current path is empty, returns an empty rectangle [{ x=0.;
      y=0.; w=0.; h=0. }].  Surface dimensions and clipping are not
      taken into account.

      Contrast with {!Cairo.Path.extents}, which is similar, but
      returns non-zero extents for some paths with no inked area,
      (such as a simple line segment).

      Note that [fill_extents] must necessarily do more work to
      compute the precise inked areas in light of the fill rule, so
      {!Cairo.Path.extents} may be more desirable for sake of
      performance if the non-inked path extents are desired.

      See {!Cairo.fill} and {!Cairo.set_fill_rule}. *)

external in_fill : context -> x:float -> y:float -> bool = "caml_cairo_in_fill"
  (** Tests whether the given point is inside the area that would be
      affected by a [fill] operation given the current path and
      filling parameters.  Surface dimensions and clipping are not
      taken into account.

      See also {!Cairo.fill} and {!Cairo.set_fill_rule}.  *)

external mask : context -> 'a Pattern.t -> unit = "caml_cairo_mask"
  (** [mask cr pattern]: a drawing operator that paints the current
      source using the alpha channel of [pattern] as a mask.  (Opaque
      areas of [pattern] are painted with the source, transparent
      areas are not painted.) *)
external mask_surface : context -> Surface.t -> x:float -> y:float -> unit
  = "caml_cairo_mask_surface"
  (** [mask_surface cr surface x y]: a drawing operator that paints
      the current source using the alpha channel of [surface] as a
      mask.  (Opaque areas of [surface] are painted with the source,
      transparent areas are not painted.)

      @param x  X coordinate at which to place the origin of [surface].
      @param y  Y coordinate at which to place the origin of [surface]. *)

val paint : ?alpha:float -> context -> unit
  (** A drawing operator that paints the current source everywhere
      within the current clip region.  If [alpha] is set, the drawing
      is faded out using the alpha value.

      @param alpha  alpha value, between 0 (transparent) and 1 (opaque).  *)

external stroke : context -> unit = "caml_cairo_stroke"
  (** A drawing operator that strokes the current path according to
      the current line width, line join, line cap, and dash settings.
      After [stroke], the current path will be cleared from the cairo
      context.  See {!Cairo.set_line_width}, {!Cairo.set_line_join},
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
      {!Cairo.Path.close} or one or more calls to {!Cairo.line_to} to
      the same coordinate as the {!Cairo.move_to}.  If the cap style
      is [ROUND] then these sub-paths will be drawn as circular dots.
      Note that in the case of [SQUARE] line cap, a degenerate
      sub-path will not be drawn at all (since the correct
      orientation is indeterminate).

      In no case will a cap style of [BUTT] cause anything to be drawn
      in the case of either degenerate segments or sub-paths. *)

external stroke_preserve : context -> unit = "caml_cairo_stroke_preserve"
  (** Like {!Cairo.stroke} except that it preserves the path within
      the cairo context. *)

val stroke_extents : context -> rectangle
  (** Computes a bounding box in user coordinates covering the area
      that would be affected (the "inked" area) by a {!Cairo.stroke}
      operation operation given the current path and stroke
      parameters.  If the current path is empty, returns an empty
      rectangle [{ x=0.; y=0.; w=0.; h=0. }].  Surface dimensions
      and clipping are not taken into account.

      Note that if the line width is set to exactly zero, then
      [stroke_extents] will return an empty rectangle.  Contrast with
      {!Cairo.Path.extents} which can be used to compute the non-empty
      bounds as the line width approaches zero.

      Note that [stroke_extents] must necessarily do more work to
      compute the precise inked areas in light of the stroke
      parameters, so {!Cairo.Path.extents} may be more desirable for
      sake of performance if non-inked path extents are desired.

      See {!Cairo.stroke}, {!Cairo.set_line_width}, {!Cairo.set_line_join},
      {!Cairo.set_line_cap}, and {!Cairo.set_dash}. *)

external in_stroke : context -> x:float -> y:float -> bool
  = "caml_cairo_in_stroke"
  (** Tests whether the given point is inside the area that would be
      affected by a {!Cairo.stroke} operation given the current path
      and stroking parameters. Surface dimensions and clipping are not
      taken into account.  *)

external copy_page : context -> unit = "caml_cairo_copy_page"
  (** [copy_page cr] emits the current page for backends that support
      multiple pages, but doesn't clear it, so, the contents of the
      current page will be retained for the next page too.  Use
      {!Cairo.show_page} if you want to get an empty page after the
      emission.

      This is a convenience function that simply calls
      {!Cairo.Surface.copy_page} on [cr]'s target. *)
external show_page : context -> unit = "caml_cairo_show_page"
  (** [show_page cr] emits and clears the current page for backends
      that support multiple pages.  Use {!Cairo.copy_page} if you
      don't want to clear the page.

      This is a convenience function that simply calls
      {!Cairo.Surface.show_page} on [cr]'s target. *)


(* ---------------------------------------------------------------------- *)
(** {2:paths  Creating paths and manipulating path data}

    Paths are the most basic drawing tools and are primarily used to
    implicitly generate simple masks.
*)

type path_data =
  | MOVE_TO of float * float
  | LINE_TO of float * float
  | CURVE_TO of float * float * float * float * float * float
  | CLOSE_PATH

module Path :
sig
  type t

  external copy : context -> t = "caml_cairo_copy_path"
    (** Creates a copy of the current path. See cairo_path_data_t for
        hints on how to iterate over the returned data structure.  *)

  external copy_flat : context -> t = "caml_cairo_copy_path_flat"
    (** Gets a flattened copy of the current path.

        This function is like {!Cairo.Path.copy} except that any
        curves in the path will be approximated with piecewise-linear
        approximations (accurate to within the current tolerance
        value).  That is, the result is guaranteed to not have any
        elements of type [CURVE_TO] which will instead be replaced by
        a series of [LINE_TO] elements.  *)

  external append : context -> t -> unit = "caml_cairo_append_path"
    (** Append the path onto the current path.  The path may be either
        the return value from one of {!Cairo.Path.copy} or
        {!Cairo.Path.copy_flat} or it may be constructed manually.  *)

  external get_current_point : context -> float * float
    = "caml_cairo_get_current_point"
    (** [get_current_point cr] gets the (x,y) coordinates of the
        current point of the current path, which is conceptually the
        final point reached by the path so far.  The current point is
        returned in the user-space coordinate system.

        Raise [Error NO_CURRENT_POINT] if there is no defined current
        point.

        Most path construction functions alter the current point.  See the
        following for details on how they affect the current point:
        {!Cairo.Path.clear}, {!Cairo.Path.sub}, {!Cairo.Path.append},
        {!Cairo.Path.close}, {!Cairo.move_to}, {!Cairo.line_to},
        {!Cairo.curve_to}, {!Cairo.rel_move_to}, {!Cairo.rel_line_to},
        {!Cairo.rel_curve_to}, {!Cairo.arc}, {!Cairo.arc_negative},
        {!Cairo.rectangle}, {!Cairo.Path.text}, {!Cairo.Path.glyph}.

        Some functions use and alter the current point but do not
        otherwise change current path: {!Cairo.show_text}.

        Some functions unset the current path and as a result, current
        point: {!Cairo.fill}, {!Cairo.stroke}. *)

  external clear : context -> unit = "caml_cairo_new_path"
    (** Clears the current path. After this call there will be no path
        and no current point. *)

  external sub : context -> unit = "caml_cairo_new_sub_path"
    (** Begin a new sub-path. Note that the existing path is not
        affected. After this call there will be no current point.

        In many cases, this call is not needed since new sub-paths are
        frequently started with {!Cairo.move_to}.

        A call to {!Cairo.Path.sub} is particularly useful when
        beginning a new sub-path with one of the {!Cairo.arc} calls.
        This makes things easier as it is no longer necessary to
        manually compute the arc's initial coordinates for a call to
        {!Cairo.move_to}. *)

  external close : context -> unit = "caml_cairo_close_path"
    (** Adds a line segment to the path from the current point to the
        beginning of the current sub-path (the most recent point
        passed to {!Cairo.move_to}) and closes this sub-path.  After
        this call the current point will be at the joined endpoint of
        the sub-path.

        The behavior of {!Cairo.Path.close} is distinct from simply
        calling {!Cairo.line_to} with the equivalent coordinate in the
        case of stroking.  When a closed sub-path is stroked, there
        are no caps on the ends of the sub-path.  Instead, there is a
        line join connecting the final and initial segments of the
        sub-path.

        If there is no current point before the call to [close], this
        function will have no effect.

        Note: As of cairo version 1.2.4 any call to [close] will place
        an explicit [MOVE_TO] element into the path immediately after
        the [CLOSE_PATH] element (which can be seen in
        {!Cairo.Path.copy} for example).  This can simplify path
        processing in some cases as it may not be necessary to save
        the "last move_to point" during processing as the [MOVE_TO]
        immediately after the [CLOSE_PATH] will provide that point. *)

  external glyph : context -> Glyph.t array -> unit = "caml_cairo_glyph_path"
    (** Adds closed paths for the glyphs to the current path. The
        generated path if filled, achieves an effect similar to that
        of {!Cairo.Glyph.show}. *)

  external text : context -> string -> unit = "caml_cairo_text_path"
    (** [text cr utf8] adds closed paths for text to the current path.
        The generated path if filled, achieves an effect similar to
        that of {!Cairo.show_text}.  [utf8] should be a valid UTF8
        string containing no ['\000'] characters.

        Text conversion and positioning is done similar to {!Cairo.show_text}.

        Like {!Cairo.show_text}, after this call the current point is
        moved to the origin of where the next glyph would be placed in
        this same progression.  That is, the current point will be at
        the origin of the final glyph offset by its advance values.
        This allows for chaining multiple calls to to [text] without
        having to set current point in between.

        Note: The [text] function call is part of what the cairo
        designers call the "toy" text API.  It is convenient for short
        demos and simple programs, but it is not expected to be
        adequate for serious text-using applications.  See
        {!Cairo.Path.glyph} for the "real" text path API in cairo. *)

  external extents : context -> rectangle = "caml_cairo_path_extents"
    (** Computes a bounding box in user-space coordinates covering the
        points on the current path. If the current path is empty,
        returns an empty rectangle [{ x=0.; y=0.; w=0.; h=0. }].
        Stroke parameters, fill rule, surface dimensions and clipping
        are not taken into account.

        Contrast with {!Cairo.fill_extents} and
        {!Cairo.stroke_extents} which return the extents of only the
        area that would be "inked" by the corresponding drawing
        operations.

        The result of [Cairo.Path.extents] is defined as equivalent to
        the limit of {!Cairo.stroke_extents} with [ROUND] as the line
        width approaches 0.0 (but never reaching the empty-rectangle
        returned by {!Cairo.stroke_extents} for a line width of 0.0).

        Specifically, this means that zero-area sub-paths such as
        {!Cairo.move_to} and {!Cairo.line_to} segments (even degenerate
        cases where the coordinates to both calls are identical) will
        be considered as contributing to the extents.  However, a lone
        {!Cairo.move_to} will not contribute to the results of
        [Cairo.Path.extents]. *)

  external fold : t -> ('a -> path_data -> 'a) -> 'a -> 'a
    = "caml_cairo_path_fold"
      (** [fold cr f] folds [f] over all elements of the path. *)

  val to_array : t -> path_data array

  val of_array : path_data array -> t
end

external arc : context ->
  x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
  = "caml_cairo_arc_bc" "caml_cairo_arc"
  (** [arc xc yc radius angle1 angle2] adds a circular arc of the
      given radius to the current path.  The arc is centered at [(xc,
      yc)], begins at [angle1] and proceeds in the direction of
      increasing angles to end at [angle2].  If [angle2] is less than
      [angle1] it will be progressively increased by 2*PI until it is
      greater than [angle1].

      If there is a current point, an initial line segment will be
      added to the path to connect the current point to the beginning
      of the arc. If this initial line is undesired, it can be avoided
      by calling {!Cairo.Path.sub} before calling [arc].

      Angles are measured in radians.  An angle of 0.0 is in the
      direction of the positive X axis (in user space). An angle of
      PI/2.0 radians (90 degrees) is in the direction of the positive
      Y axis (in user space).  Angles increase in the direction from
      the positive X axis toward the positive Y axis. So with the
      default transformation matrix, angles increase in a clockwise
      direction.

      (To convert from degrees to radians, use degrees * (PI / 180.).)

      This function gives the arc in the direction of increasing
      angles; see {!Cairo.arc_negative} to get the arc in the
      direction of decreasing angles.

      The arc is circular in user space.  To achieve an elliptical
      arc, you can scale the current transformation matrix by
      different amounts in the X and Y directions.  For example, to
      draw an ellipse in the box given by [x], [y], [width], [height]
      (we suppose [pi] holds the value of PI):
      {[
      open Cairo

      save cr;
      translate cr (x +. width /. 2.) (y +. height /. 2.);
      scale cr (width /. 2.) (height /. 2.);
      arc cr 0. 0. 1. 0. (2 * pi);
      restore cr;
      ]}
  *)

external arc_negative : context ->
  x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
  = "caml_cairo_arc_negative_bc" "caml_cairo_arc_negative"
  (** [arc_negative xc yc radius angle1 angle2] adds a circular arc of
      the given radius to the current path.  The arc is centered at
      [(xc, yc)], begins at [angle1] and proceeds in the direction of
      decreasing angles to end at [angle2].  If [angle2] is greater
      than [angle1] it will be progressively decreased by 2*PI until
      it is less than [angle1].

      See {!Cairo.arc} for more details.  This function differs only
      in the direction of the arc between the two angles. *)

external curve_to : context ->
  x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
  = "caml_cairo_curve_to_bc" "caml_cairo_curve_to"
  (** Adds a cubic B�zier spline to the path from the current point to
      position (x3, y3) in user-space coordinates, using (x1, y1) and
      (x2, y2) as the control points.  After this call the current
      point will be (x3, y3).

      If there is no current point before the call to [curve_to] this
      function will behave as if preceded by a call to
      {!Cairo.move_to}[ cr x1 y1]. *)

external line_to : context -> x:float -> y:float -> unit = "caml_cairo_line_to"
  (** Adds a line to the path from the current point to position (x,
      y) in user-space coordinates. After this call the current point
      will be (x, y).

      If there is no current point before the call to [Cairo.line_to],
      this function will behave as {!Cairo.move_to}[ cr x y]. *)

external move_to : context -> x:float -> y:float -> unit = "caml_cairo_move_to"
  (** Begin a new sub-path.  After this call the current point will be
      (x, y). *)

external rectangle : context ->
  x:float -> y:float -> w:float -> h:float -> unit
  = "caml_cairo_rectangle"
  (** Adds a closed sub-path rectangle of the given size to the
      current path at position (x, y) in user-space coordinates.

      This function is logically equivalent to:
      {[
      move_to cr x y;
      rel_line_to cr width 0;
      rel_line_to cr 0 height;
      rel_line_to cr (-. width) 0;
      Path.close cr;
      ]}
  *)

external rel_curve_to : context ->
  x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
  = "caml_cairo_rel_curve_to_bc" "caml_cairo_rel_curve_to"
  (** Relative-coordinate version of {!Cairo.curve_to}.  All offsets
      are relative to the current point.  Adds a cubic B�zier spline
      to the path from the current point to a point offset from the
      current point by (dx3, dy3), using points offset by (dx1, dy1)
      and (dx2, dy2) as the control points.  After this call the
      current point will be offset by (dx3, dy3).

      Given a current point of (x, y), [rel_curve_to cr dx1 dy1 dx2
      dy2 dx3 dy3] is logically equivalent to [curve_to cr (x+.dx1)
      (y+.dy1) (x+.dx2) (y+.dy2) (x+.dx3) (y+.dy3)].

      It is an error to call this function with no current point.
      Doing so will cause [Error NO_CURRENT_POINT] to be raised.  *)

external rel_line_to : context -> x:float -> y:float -> unit
  = "caml_cairo_rel_line_to"
  (** Relative-coordinate version of {!Cairo.line_to}.  Adds a line to
      the path from the current point to a point that is offset from the
      current point by (dx, dy) in user space. After this call the current
      point will be offset by (dx, dy).

      Given a current point of (x, y), [rel_line_to cr dx dy] is
      logically equivalent to [line_to cr (x +. dx) (y +. dy)].

      It is an error to call this function with no current point.
      Doing so will cause [Error NO_CURRENT_POINT] to be raised.  *)

external rel_move_to : context -> x:float -> y:float -> unit
  = "caml_cairo_rel_move_to"
  (** Begin a new sub-path. After this call the current point will
      offset by (x, y).

      Given a current point of (x, y), [rel_move_to cr dx dy] is
      logically equivalent to [move_to cr (x +. dx) (y +. dy)].

      It is an error to call this function with no current point.
      Doing so will cause [Error NO_CURRENT_POINT] to be raised. *)



(* ---------------------------------------------------------------------- *)
(** {2:transformations  Manipulating the current transformation matrix}

    The current transformation matrix, {i ctm}, is a two-dimensional
    affine transformation that maps all coordinates and other drawing
    instruments from the {i user space} into the surface's canonical
    coordinate system, also known as the {i device space}.

    See also {!Cairo.Matrix}. *)


external translate : context -> x:float -> y:float -> unit
  = "caml_cairo_translate"
  (** [translate cr tx ty] modifies the current transformation matrix
      (CTM) by translating the user-space origin by ([tx], [ty]).
      This offset is interpreted as a user-space coordinate according
      to the CTM in place before the new call to [translate].  In
      other words, the translation of the user-space origin takes
      place after any existing transformation. *)

external scale : context -> x:float -> y:float -> unit = "caml_cairo_scale"
  (** [scale sx sy] modifies the current transformation matrix (CTM)
      by scaling the X and Y user-space axes by [sx] and [sy]
      respectively.  The scaling of the axes takes place after any
      existing transformation of user space. *)

external rotate : context -> angle:float -> unit = "caml_cairo_rotate"
  (** Modifies the current transformation matrix (CTM) by rotating the
      user-space axes by [angle] radians.  The rotation of the axes
      takes places after any existing transformation of user space.
      The rotation direction for positive angles is from the positive
      X axis toward the positive Y axis. *)

external transform : context -> Matrix.t -> unit
  = "caml_cairo_transform" "noalloc"
  (** [transform cr matrix] modifies the current transformation matrix
      (CTM) by applying [matrix] as an additional transformation.  The
      new transformation of user space takes place after any existing
      transformation. *)

external set_matrix : context -> Matrix.t -> unit
  = "caml_cairo_set_matrix" "noalloc"
  (** [set_matrix cr matrix] Modifies the current transformation
      matrix (CTM) by setting it equal to [matrix]. *)

external get_matrix : context -> Matrix.t = "caml_cairo_get_matrix"
  (** Return the current transformation matrix (CTM). *)

external identity_matrix : context -> unit = "caml_cairo_identity_matrix"
  (** Resets the current transformation matrix (CTM) by setting it
      equal to the identity matrix.  That is, the user-space and
      device-space axes will be aligned and one user-space unit will
      transform to one device-space unit. *)

external user_to_device : context -> x:float -> y:float -> float * float
  = "caml_cairo_user_to_device"
  (** [user_to_device cr x y] transform a coordinate from user space
      to device space by multiplying the given point by the current
      transformation matrix (CTM). *)

external user_to_device_distance :
  context -> x:float -> y:float -> float * float
  = "caml_cairo_user_to_device_distance"
  (** [user_to_device_distance cr dx dy] transform a distance vector
      from user space to device space.  This function is similar to
      {!Cairo.user_to_device} except that the translation components
      of the CTM will be ignored when transforming ([dx],[dy]). *)

external device_to_user : context -> x:float -> y:float -> float * float
  = "caml_cairo_device_to_user"
  (** Transform a coordinate from device space to user space by
      multiplying the given point by the inverse of the current
      transformation matrix (CTM). *)

external device_to_user_distance :
  context -> x:float -> y:float -> float * float
  = "caml_cairo_device_to_user_distance"
  (** [device_to_user_distance cr dx dy] transform a distance vector
      from device space to user space.  This function is similar to
      {!Cairo.device_to_user} except that the translation components
      of the inverse CTM will be ignored when transforming ([dx],[dy]). *)

