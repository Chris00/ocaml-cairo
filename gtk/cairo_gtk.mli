(* File: cairo_gtk.mli

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

(** Integration of Cairo with labgtk2.

    It allows to create Cairo contexts which can be used to draw on
    GDK drawables. Additional functions allow to convert GDK's
    rectangles and regions into Cairo paths and to use pixbufs as
    sources for drawing operations.  *)

val create : [> `drawable] Gobject.obj -> Cairo.context
(** [create drawable] creates a Cairo context for drawing to
   [drawable].

   NOTE: due to double-buffering, Cairo contexts created in a GTK+
   expose event handler cannot be cached and reused between different
   expose events.  *)

val set_source_color : Cairo.context -> Gdk.color -> unit
(** [set_source_color cr color] sets the specified [color] as the
   source color of cr. *)

val rectangle : Cairo.context -> Gdk.Rectangle.t -> unit
(** [rectangle cr r] adds the rectangle [r] to the current path of [cr].  *)

val region : Cairo.context -> Gdk.region -> unit
(** [region cr r] adds the region [r] to the current path of [cr]. *)

val set_source_pixbuf : Cairo.context ->
  GdkPixbuf.pixbuf -> x:float -> y:float -> unit
(** Sets the given pixbuf as the source pattern for the Cairo context.
   The pattern has an extend mode of {!Cairo.Pattern.extend} set to
   [NONE] and is aligned so that the origin of pixbuf is ([x],[y]). *)
