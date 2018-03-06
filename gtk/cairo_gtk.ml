(* File: cairo_gtk.ml

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

(* http://library.gnome.org/devel/gdk/unstable/gdk-Cairo-Interaction.html *)

external create : [> `drawable] Gobject.obj -> Cairo.context
  = "caml_gdk_cairo_create"
external set_source_color : Cairo.context -> Gdk.color -> unit
  = "caml_gdk_cairo_set_source_color"
external rectangle : Cairo.context -> Gdk.Rectangle.t -> unit
  = "caml_gdk_cairo_rectangle"
external region : Cairo.context -> Gdk.region -> unit
  = "caml_gdk_cairo_region"

external set_source_pixbuf : Cairo.context -> GdkPixbuf.pixbuf ->
  x:float -> y:float -> unit = "caml_gdk_cairo_set_source_pixbuf"
