/* File: cairo_gtk_stubs.c

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include <gtk/gtkversion.h>
#include <gdk/gdk.h>
/* OCaml labgtk stubs */
#include <wrappers.h>
#include <ml_gobject.h>
#include <ml_gdk.h>
#include <ml_gdkpixbuf.h>

/* OCaml Cairo bindings */
#include "cairo_ocaml.h"
#include "cairo_macros.c"  /* for convenience only */


#if ! GTK_CHECK_VERSION(2,8,0)

#error "Gtk+ version >= 2.8 is required"

#else /* not GTK_CHECK_VERSION(2,8,0) */

CAMLexport value caml_gdk_cairo_create(value vdrawable)
{
  CAMLparam1(vdrawable);
  CAMLlocal1(vcontext);
  cairo_t *cr = gdk_cairo_create(GdkDrawable_val(vdrawable));
  caml_check_status(cr);
  /* FIXME: is there a way to indicate the dependence of [vcontext] on
     [vdrawable] ? */
  vcontext = alloc_custom(&caml_cairo_ops, sizeof(void*), 1, 50);
  CAIRO_VAL(vcontext) = cr;
  CAMLreturn(vcontext);
}

DO1_CONTEXT(gdk_cairo_set_source_color, GdkColor_val)
DO1_CONTEXT(gdk_cairo_rectangle, GdkRectangle_val)
DO1_CONTEXT(gdk_cairo_region, GdkRegion_val)
DO3_CONTEXT(gdk_cairo_set_source_pixbuf, GdkPixbuf_val, Double_val, Double_val)

#endif /* GTK_CHECK_VERSION(2,8,0) */
