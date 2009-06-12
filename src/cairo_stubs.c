/* File: cairo_stubs.c

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

#include <cairo.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/bigarray.h>

/* Generic functions for types */

static int caml_cairo_compare_pointers(value v1, value v2)
{
  void *p1 = Data_custom_val(v1);
  void *p2 = Data_custom_val(v2);
  if (p1 == p2) return(0);
  else if (p1 < p2) return(-1);
  else return(1);
}

static long caml_cairo_hash_pointer(value v)
{
  return((long) Data_custom_val(v));
}

#define DEFINE_CUSTOM_OPERATIONS(name, destroy, val)                    \
  static void caml_##name##_finalize(value v)                           \
  {                                                                     \
    /* [cairo_*_reference] not used, the first [destroy] frees it. */   \
    destroy(val(v));                                                    \
  }                                                                     \
                                                                        \
  static struct custom_operations caml_##name##_ops = {                 \
    #name "_t", /* identifier for serialization and deserialization */ \
    &caml_##name##_finalize,                                            \
    &caml_cairo_compare_pointers,                                       \
    &caml_cairo_hash_pointer,                                           \
    custom_serialize_default,                                           \
    custom_deserialize_default };

#define ALLOC(name) alloc_custom(&caml_##name##_ops, sizeof(void*), 1, 50)

/* Type cairo_t
***********************************************************************/

#define CAIRO_VAL(v) (* (cairo_t **) Data_custom_val(v))
#define CAIRO_ASSIGN(v, x) v = ALLOC(cairo); CAIRO_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(cairo, cairo_destroy, CAIRO_VAL)

/* For non Raise the corresponding OCaml exception. */
static void caml_check_status(cairo_t *cr)
{
  static value * exn = NULL;
  cairo_status_t status = cairo_status(cr);

  if (status != CAIRO_STATUS_SUCCESS) {
    if (exn == NULL) {
      /* First time around, look up by name */
      exn = caml_named_value("Cairo.error");
    }
    if (status == CAIRO_STATUS_NO_MEMORY)
      caml_raise_out_of_memory();
    else
      /* Keep in sync with the OCaml def of [status]; variant without
         arguments == int.  The first 2 values of cairo_status_t are
         deleted. */
      caml_raise_with_arg(*exn, Val_int(status - 2));
  }
}

CAMLexport value caml_cairo_status_to_string(value vstatus)
{
  CAMLparam1(vstatus);
  cairo_status_t status = Int_val(vstatus) + 2;
  const char* msg = cairo_status_to_string(status);
  CAMLreturn(caml_copy_string(msg));
}

/* Type cairo_pattern_t
***********************************************************************/

#define PATTERN_VAL(v) (* (cairo_pattern_t **) Data_custom_val(v))
#define PATTERN_ASSIGN(v, x) v = ALLOC(pattern); PATTERN_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(pattern, cairo_pattern_destroy, PATTERN_VAL)

/* Type cairo_surface_t
***********************************************************************/

#define SURFACE_VAL(v) (* (cairo_surface_t **) Data_custom_val(v))
#define SURFACE_ASSIGN(v, x) v = ALLOC(surface); SURFACE_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(surface, cairo_surface_destroy, SURFACE_VAL)


/* Type cairo_content_t */

#define CONTENT_ASSIGN(v, vcontent)                                     \
  switch (Int_val(vcontent))                                            \
    {                                                                   \
    case 0 : v = CAIRO_CONTENT_COLOR;  break;                           \
    case 1 : v = CAIRO_CONTENT_ALPHA;  break;                           \
    case 2 : v = CAIRO_CONTENT_COLOR_ALPHA;  break;                     \
    default : caml_failwith("Decode Cairo.content");                    \
    }

/* cairo_t functions.
***********************************************************************/

CAMLexport value caml_cairo_create(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vcontext);
  cairo_t *cr;

  cr = cairo_create(SURFACE_VAL(vsurf));
  caml_check_status(cr);
  CAIRO_ASSIGN(vcontext, cr);
  CAMLreturn(vcontext);
}

CAMLexport value caml_cairo_save(value vcr)
{
  CAMLparam1(vcr);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_save(cr);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_restore(value vcr)
{
  CAMLparam1(vcr);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_restore(cr);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_surface_t* s = cairo_get_target(cr);
  SURFACE_ASSIGN(vsurf, s);
  caml_check_status(cr);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_push_group(value vcr)
{
  CAMLparam1(vcr);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_push_group(cr);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_push_group_with_content(value vcr, value vcontent)
{
  CAMLparam2(vcr, vcontent);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_content_t content;
  CONTENT_ASSIGN(content, vcontent);
  cairo_push_group_with_content(cr, content);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_pop_group(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpat);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_pattern_t* pat = cairo_pop_group(cr);
  caml_check_status(cr);
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pop_group_to_source(value vcr)
{
  CAMLparam1(vcr);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_pop_group_to_source(cr);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_group_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_surface_t* s = cairo_get_group_target(cr);
  caml_check_status(cr);
  SURFACE_ASSIGN(vsurf, s);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_set_source_rgb(
  value vcr, value vr, value vg, value vb)
{
  CAMLparam4(vcr, vr, vg, vb);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_set_source_rgb(cr, Double_val(vr), Double_val(vg), Double_val(vb));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_set_source_rgba(
  value vcr, value vr, value vg, value vb, value va)
{
  CAMLparam4(vcr, vr, vg, vb);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_set_source_rgba(cr, Double_val(vr), Double_val(vg), Double_val(vb),
                        Double_val(va));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_set_source(value vcr, value vpat)
{
  CAMLparam2(vcr, vpat);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_set_source(cr, PATTERN_VAL(vpat));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_source(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpat);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_pattern_t* pat = cairo_get_source(cr);
  caml_check_status(cr);
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

#define ANTIALIAS_VAL(v) Int_val(v)

CAMLexport value caml_cairo_set_antialias(value vcr, value vantialias)
{
  CAMLparam2(vcr, vantialias);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_set_antialias(cr, ANTIALIAS_VAL(vantialias));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_antialias(value vcr)
{
  CAMLparam1(vcr);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_antialias_t antialias = cairo_get_antialias(cr);
  caml_check_status(cr);
  CAMLreturn(Val_int(antialias));
}

CAMLexport value caml_cairo_set_dash(value vcr, value vdashes, value voffset)
{
  CAMLparam3(vcr, vdashes, voffset);
  cairo_t* cr = CAIRO_VAL(vcr);
  double *dashes;
  int num_dashes = Wosize_val(vdashes) / Double_wosize;
  int i;
  for(i = 0; i < num_dashes; i++)  dashes[i] = Double_field(vdashes, i);
  cairo_set_dash(cr, dashes, num_dashes, Double_val(voffset));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_dash(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal2(couple, vdashes);
  cairo_t* cr = CAIRO_VAL(vcr);
  int num_dashes = cairo_get_dash_count(cr);
  double *dashes;
  double offset;
  int i;

  couple = caml_alloc_tuple(2);
  if (num_dashes == 0) { /* return ([||], 0.) */
    Store_field(couple, 0, caml_alloc_tuple(0));  /* [||] */
    Store_field(couple, 1, caml_copy_double(0.0));
  }
  else {
    dashes = malloc(num_dashes * sizeof(double));
    cairo_get_dash(cr, dashes, &offset);
    vdashes = caml_alloc(num_dashes * Double_wosize, Double_array_tag);
    for(i = 0; i < num_dashes; i++)
      Store_double_field(vdashes, i, dashes[i]);
    Store_field(couple, 0, vdashes);
    Store_field(couple, 1, caml_copy_double(offset));
    free(dashes);
  }
  CAMLreturn(couple);
}





/* Local Variables: */
/* compile-command: "make -k cairo_stubs.o" */
/* End: */
