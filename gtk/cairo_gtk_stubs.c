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


#if ! GTK_CHECK_VERSION(2,8,0)

#error "Gtk+ version >= 2.8 is required"

#else /* not GTK_CHECK_VERSION(2,8,0) */

#define DO1_CONTEXT(name, of_value)                  \
  CAMLexport value caml_##name(value vcr, value v)   \
  {                                                  \
    CAMLparam2(vcr, v);                              \
    cairo_t* cr = CAIRO_VAL(vcr);                    \
    name(cr, of_value(v));                           \
    caml_cairo_raise_Error(cairo_status(cr));        \
    CAMLreturn(Val_unit);                            \
  }

#define DO3_CONTEXT(name, of_val1, of_val2, of_val3)                    \
  CAMLexport value caml_##name(value vcr, value v1, value v2, value v3) \
  {                                                                     \
    CAMLparam4(vcr, v1, v2, v3);                                        \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2), of_val3(v3));                    \
    caml_cairo_raise_Error(cairo_status(cr));                           \
    CAMLreturn(Val_unit);                                               \
  }


CAMLexport value caml_gdk_cairo_create(value vdrawable)
{
  CAMLparam1(vdrawable);
  CAMLlocal1(vcontext);
  cairo_t *cr = gdk_cairo_create(GdkDrawable_val(vdrawable));
  caml_cairo_raise_Error(cairo_status(cr)); /* caml_check_status not exported */
  vcontext = caml_alloc_custom(&caml_cairo_ops, sizeof(void*), 1, 50);
  CAIRO_VAL(vcontext) = cr;
  CAMLreturn(vcontext);
}

DO1_CONTEXT(gdk_cairo_set_source_color, GdkColor_val)
DO1_CONTEXT(gdk_cairo_rectangle, GdkRectangle_val)
DO1_CONTEXT(gdk_cairo_region, GdkRegion_val)
DO3_CONTEXT(gdk_cairo_set_source_pixbuf, GdkPixbuf_val, Double_val, Double_val)

#endif /* GTK_CHECK_VERSION(2,8,0) */
