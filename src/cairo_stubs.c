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

#include "cairo_macros.c"
#include "cairo_ocaml_types.c"

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

DO_FUNCTION(cairo_save)
DO_FUNCTION(cairo_restore)

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

DO_FUNCTION(cairo_push_group)

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

DO_FUNCTION(cairo_pop_group_to_source)

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

DO1_FUNCTION(cairo_set_source, PATTERN_VAL)

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
#define VAL_ANTIALIAS(v) Val_int(v)

DO1_FUNCTION(cairo_set_antialias, ANTIALIAS_VAL)
GET_FUNCTION(cairo_get_antialias, VAL_ANTIALIAS, cairo_antialias_t)

CAMLexport value caml_cairo_set_dash(value vcr, value vdashes, value voffset)
{
  CAMLparam3(vcr, vdashes, voffset);
  cairo_t* cr = CAIRO_VAL(vcr);
  double *dashes;
  const int num_dashes = FLOAT_ARRAY_LENGTH(vdashes);
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

#define FILL_RULE_VAL(v) Int_val(v)
#define VAL_FILL_RULE(v) Val_int(v)

DO1_FUNCTION(cairo_set_fill_rule, FILL_RULE_VAL)
GET_FUNCTION(cairo_get_fill_rule, VAL_FILL_RULE, cairo_fill_rule_t)

#define LINE_CAP_VAL(v) Int_val(v)
#define VAL_LINE_CAP(v) Val_int(v)

DO1_FUNCTION(cairo_set_line_cap, FILL_RULE_VAL)
GET_FUNCTION(cairo_get_line_cap, VAL_LINE_CAP, cairo_line_cap_t)

#define LINE_JOIN_VAL(v) Int_val(v)
#define VAL_LINE_JOIN(v) Val_int(v)

DO1_FUNCTION(cairo_set_line_join, LINE_JOIN_VAL)
GET_FUNCTION(cairo_get_line_join, VAL_LINE_JOIN, cairo_line_join_t)

DO1_FUNCTION(cairo_set_line_width, Double_val)
GET_FUNCTION(cairo_get_line_width, caml_copy_double, double)

DO1_FUNCTION(cairo_set_miter_limit, Double_val)
GET_FUNCTION(cairo_get_miter_limit, caml_copy_double, double)

#define OPERATOR_VAL(v) Int_val(v)
#define VAL_OPERATOR(v) Val_int(v)

DO1_FUNCTION(cairo_set_operator, OPERATOR_VAL)
GET_FUNCTION(cairo_get_operator, VAL_OPERATOR, cairo_operator_t)

DO1_FUNCTION(cairo_set_tolerance, Double_val)
GET_FUNCTION(cairo_get_tolerance, caml_copy_double, double)

DO_FUNCTION(cairo_clip)
DO_FUNCTION(cairo_clip_preserve)
GET_EXTENTS(cairo_clip_extents)
DO_FUNCTION(cairo_reset_clip)

CAMLexport value caml_cairo_copy_clip_rectangle_list(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal3(vlist, vrec, cons);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_rectangle_list_t* list = cairo_copy_clip_rectangle_list(cr);
  int i;
  cairo_rectangle_t *r;
  /* assert(list != NULL); */
  caml_raise_Error(list->status);
  vlist = Val_int(0); /* [] */
  for(i = 0, r = list->rectangles;  i < list->num_rectangles;  i++, r++) {
    /* New rectangle (pure float record) */
    vrec = caml_alloc(4 * Double_wosize, Double_array_tag);
    Store_double_field(vrec, 0, r->x);
    Store_double_field(vrec, 1, r->y);
    Store_double_field(vrec, 2, r->width);
    Store_double_field(vrec, 3, r->height);
    /* New cons cell */
    cons = caml_alloc_tuple(2);
    Store_field(cons, 0, vrec);
    Store_field(cons, 1, vlist);
    vlist = cons;
  }
  cairo_rectangle_list_destroy(list);
  CAMLreturn(vlist);
}


DO_FUNCTION(cairo_fill)
DO_FUNCTION(cairo_fill_preserve)

GET_EXTENTS(cairo_fill_extents)

CAMLexport value caml_cairo_in_fill(value vcr, value vx, value vy)
{
  CAMLparam3(vcr, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_bool_t b = cairo_in_fill(cr, Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  /* doc of cairo_bool_t: b=0 or 1 */
  CAMLreturn(Val_int(b));
}

DO1_FUNCTION(cairo_mask, PATTERN_VAL)

CAMLexport value caml_cairo_mask_surface(value vcr, value vsurf,
                                         value vx, value vy)
{
  CAMLparam4(vcr, vsurf, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_mask_surface(cr, SURFACE_VAL(vsurf), Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO_FUNCTION(cairo_paint)
DO1_FUNCTION(cairo_paint_with_alpha, Double_val)

DO_FUNCTION(cairo_stroke)
DO_FUNCTION(cairo_stroke_preserve)

GET_EXTENTS(cairo_stroke_extents)

CAMLexport value caml_cairo_in_stroke(value vcr, value vx, value vy)
{
  CAMLparam3(vcr, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_bool_t b = cairo_in_stroke(cr, Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  /* doc of cairo_bool_t: b=0 or 1 */
  CAMLreturn(Val_int(b));
}

DO_FUNCTION(cairo_copy_page)
DO_FUNCTION(cairo_show_page)

/* TODO: cairo_set_user_data, cairo_get_user_data */

/* Paths -- Creating paths and manipulating path data
***********************************************************************/

CAMLexport value caml_cairo_copy_path(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_path_t* path = cairo_copy_path(cr);
  caml_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

CAMLexport value caml_cairo_copy_path_flat(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_path_t* path = cairo_copy_path_flat(cr);
  caml_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

DO1_FUNCTION(cairo_append_path, PATH_VAL)

CAMLexport value caml_cairo_get_current_point(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vcouple);
  cairo_t* cr = CAIRO_VAL(vcr);
  double x, y;
  cairo_get_current_point(cr, &x, &y);
  caml_check_status(cr);
  /* Couple (x,y) */
  vcouple = caml_alloc_tuple(2);
  Store_field(vcouple, 0, caml_copy_double(x));
  Store_field(vcouple, 1, caml_copy_double(y));
  CAMLreturn(vcouple);
}

DO_FUNCTION(cairo_new_path)
DO_FUNCTION(cairo_new_sub_path)
DO_FUNCTION(cairo_close_path)

CAMLexport value caml_cairo_glyph_path(value vcr, value vglyphs)
{
  CAMLparam2(vcr, vglyphs);
  cairo_t* cr = CAIRO_VAL(vcr);
  int num_glyphs = Wosize_val(vglyphs);
  cairo_glyph_t *glyphs, *p;
  int i;

  glyphs = malloc(num_glyphs * sizeof(cairo_glyph_t));
  for(i=0, p = glyphs; i < num_glyphs; i++, p++) {
    SET_GLYPH_VAL(p, vglyphs);
  }
  cairo_glyph_path(cr, glyphs, num_glyphs);
  free(glyphs);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO1_FUNCTION(cairo_text_path, String_val)
GET_EXTENTS(cairo_path_extents)

DO5_FUNCTION(cairo_arc, Double_val, Double_val, Double_val, Double_val,
             Double_val)
DO5_FUNCTION(cairo_arc_negative, Double_val, Double_val, Double_val,
             Double_val, Double_val)
DO6_FUNCTION(cairo_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_FUNCTION(cairo_line_to, Double_val, Double_val)
DO2_FUNCTION(cairo_move_to, Double_val, Double_val)
DO4_FUNCTION(cairo_rectangle, Double_val, Double_val, Double_val, Double_val)

DO6_FUNCTION(cairo_rel_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_FUNCTION(cairo_rel_line_to, Double_val, Double_val)
DO2_FUNCTION(cairo_rel_move_to, Double_val, Double_val)


/* Interacting with the paths content from OCaml. */

CAMLexport value caml_cairo_path_fold(value vpath, value fn, value va)
{
  CAMLparam3(vpath, fn, va);
  CAMLlocal2(vacc, vdata);
  cairo_path_t * path = PATH_VAL(vpath);
  cairo_path_data_t *data;
  int i;

  vacc = va;
  for(i = 0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    PATH_DATA_ASSIGN(vdata, data);
    vdata = caml_callback2(fn, vacc, vdata);
  }
  CAMLreturn(vacc);
}

CAMLexport value caml_cairo_path_to_array(value vpath)
{
  CAMLparam1(vpath);
  CAMLlocal2(varray, vdata);
  cairo_path_t * path = PATH_VAL(vpath);
  cairo_path_data_t *data;
  int i;

  varray = caml_alloc_tuple(path->num_data);
  for(i = 0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    PATH_DATA_ASSIGN(vdata, data);
    Store_field(varray, i, vdata);
  }
  CAMLreturn(varray);
}

CAMLexport value caml_cairo_path_of_array(value varray)
{
  CAMLparam1(varray);
  CAMLlocal2(vpath, vdata);
  int length = Wosize_val(varray);
  cairo_path_t* path;
  cairo_path_data_t *data;
  int i, num_data;

  path = malloc(sizeof(cairo_path_t));
  path->status = CAIRO_STATUS_SUCCESS;
  path->num_data = num_data;
  /* Compute the total length */
  num_data = 0;
#define ADD1 num_data += 1
#define ADD2(x,y) num_data += 2 /* 1 header + 1 point */
#define ADD4(x1,y1, x2,y2, x3,y3) num_data += 4 /* 1 header + 3 point */
  for(i = 0; i < length; i++) {
    vdata = Field(varray, i);
    SWITCH_PATH_DATA(vdata, ADD2, ADD2, ADD4, ADD1);
  }

#define MOVE(x1,y1)                             \
  data->header.type = CAIRO_PATH_MOVE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1)
#define LINE(x1,y1)                             \
  data->header.type = CAIRO_PATH_LINE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1)
#define CURVE(x1,y1, x2,y2, x3,y3)              \
  data->header.type = CAIRO_PATH_CURVE_TO;      \
  data->header.length = 4;                      \
  data[1].point.x = caml_copy_double(x1);       \
  data[1].point.y = caml_copy_double(y1);       \
  data[2].point.x = caml_copy_double(x2);       \
  data[2].point.y = caml_copy_double(y2);       \
  data[3].point.x = caml_copy_double(x3);       \
  data[3].point.y = caml_copy_double(y3)
#define CLOSE                                   \
  data->header.type = CAIRO_PATH_CLOSE_PATH;    \
  data->header.length = 1;

  path->data = malloc(num_data * sizeof(cairo_path_data_t));
  for(i = 0; i < num_data; i += data->header.length) {
    vdata = Field(varray, i);
    data = &path->data[i];
    SWITCH_PATH_DATA(vdata,MOVE, LINE, CURVE, CLOSE);
  }
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}


/* Patterns -- Sources for drawing
***********************************************************************/




/* Local Variables: */
/* compile-command: "make -k cairo_stubs.o" */
/* End: */
