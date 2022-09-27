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

#include <string.h>
#include <cairo.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/bigarray.h>

#include "cairo_macros.h"
#include "cairo_ocaml_types.h"

/* cairo_t functions.
***********************************************************************/

CAMLexport value caml_cairo_create(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vcontext);
  cairo_t *cr;

  cr = cairo_create(SURFACE_VAL(vsurf));
  caml_check_status(cr);
  /* Cairo documentation says that [cairo_create] "references target,
     so you can immediately call cairo_surface_destroy() on it if you
     don't need to maintain a separate reference to it".  We leave
     destroying the surface to the GC but that means there is no need
     to increase the reference of [vsurf]. */
  CAIRO_ASSIGN(vcontext, cr);
  CAMLreturn(vcontext);
}

DO_CONTEXT(cairo_save)
DO_CONTEXT(cairo_restore)

CAMLexport value caml_cairo_get_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_surface_t* surf = cairo_get_target(cr);
  caml_check_status(cr);
  /* This returns a surface value [vsurf] which will be GC.  In order
     to avoid that GC [vsurf] destroy the underlying surface too soon,
     one must increase its ref count. */
  cairo_surface_reference(surf);
  SURFACE_ASSIGN(vsurf, surf);
  CAMLreturn(vsurf);
}

DO_CONTEXT(cairo_push_group)

CAMLexport value caml_cairo_push_group_with_content(value vcr, value vcontent)
{
  CAMLparam2(vcr, vcontent);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_content_t content;
  SET_CONTENT_VAL(content, vcontent);
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

DO_CONTEXT(cairo_pop_group_to_source)

CAMLexport value caml_cairo_get_group_target(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsurf);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_surface_t* surf = cairo_get_group_target(cr);
  caml_check_status(cr);
  /* New GC value [vsurf] depending on a (shared) surface => incr ref
     count (see caml_cairo_get_target).  */
  cairo_surface_reference(surf);
  SURFACE_ASSIGN(vsurf, surf);
  CAMLreturn(vsurf);
}

DO3_CONTEXT(cairo_set_source_rgb, Double_val, Double_val, Double_val)

DO4_CONTEXT(cairo_set_source_rgba, Double_val, Double_val,
             Double_val, Double_val)

DO3_CONTEXT(cairo_set_source_surface, SURFACE_VAL, Double_val, Double_val)

DO1_CONTEXT(cairo_set_source, PATTERN_VAL)


CAMLexport value caml_cairo_get_source(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpat);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_pattern_t* pat = cairo_get_source(cr);
  caml_check_status(cr);
  /* New value [vpat] sharing the pattern => incr ref count. */
  cairo_pattern_reference(pat);
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}


#define ANTIALIAS_VAL(v) ((cairo_antialias_t) Int_val(v))
#define VAL_ANTIALIAS(v) Val_int(v)

DO1_CONTEXT(cairo_set_antialias, ANTIALIAS_VAL)
GET_CONTEXT(cairo_get_antialias, VAL_ANTIALIAS, cairo_antialias_t)

CAMLexport value caml_cairo_set_dash(value vcr, value vdashes, value voffset)
{
  CAMLparam3(vcr, vdashes, voffset);
  cairo_t* cr = CAIRO_VAL(vcr);
  double *dashes;
  const int num_dashes = FLOAT_ARRAY_LENGTH(vdashes);
  int i;

  SET_FLOAT_ARRAY(dashes, vdashes, num_dashes);
  cairo_set_dash(cr, dashes, num_dashes, Double_val(voffset));
  FREE_FLOAT_ARRAY(dashes);
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
    /* Alloc the Caml value first in case it raises an exn */
    vdashes = caml_alloc(num_dashes * Double_wosize, Double_array_tag);
    SET_MALLOC(dashes, num_dashes, double);
    cairo_get_dash(cr, dashes, &offset);
    for(i = 0; i < num_dashes; i++)
      Store_double_field(vdashes, i, dashes[i]);
    Store_field(couple, 0, vdashes);
    Store_field(couple, 1, caml_copy_double(offset));
    free(dashes);
  }
  CAMLreturn(couple);
}

#define FILL_RULE_VAL(v) ((cairo_fill_rule_t) Int_val(v))
#define VAL_FILL_RULE(v) Val_int(v)

DO1_CONTEXT(cairo_set_fill_rule, FILL_RULE_VAL)
GET_CONTEXT(cairo_get_fill_rule, VAL_FILL_RULE, cairo_fill_rule_t)

#define LINE_CAP_VAL(v) ((cairo_line_cap_t) Int_val(v))
#define VAL_LINE_CAP(v) Val_int(v)

DO1_CONTEXT(cairo_set_line_cap, LINE_CAP_VAL)
GET_CONTEXT(cairo_get_line_cap, VAL_LINE_CAP, cairo_line_cap_t)

#define LINE_JOIN_VAL(v) ((cairo_line_join_t) Int_val(v))
#define VAL_LINE_JOIN(v) Val_int(v)

DO1_CONTEXT(cairo_set_line_join, LINE_JOIN_VAL)
GET_CONTEXT(cairo_get_line_join, VAL_LINE_JOIN, cairo_line_join_t)

DO1_CONTEXT(cairo_set_line_width, Double_val)
GET_CONTEXT(cairo_get_line_width, caml_copy_double, double)

DO1_CONTEXT(cairo_set_miter_limit, Double_val)
GET_CONTEXT(cairo_get_miter_limit, caml_copy_double, double)

#define OPERATOR_VAL(v) ((cairo_operator_t) Int_val(v))
#define VAL_OPERATOR(v) Val_int(v)

DO1_CONTEXT(cairo_set_operator, OPERATOR_VAL)
GET_CONTEXT(cairo_get_operator, VAL_OPERATOR, cairo_operator_t)

DO1_CONTEXT(cairo_set_tolerance, Double_val)
GET_CONTEXT(cairo_get_tolerance, caml_copy_double, double)

DO_CONTEXT(cairo_clip)
DO_CONTEXT(cairo_clip_preserve)
GET_EXTENTS(cairo_clip_extents)
DO_CONTEXT(cairo_reset_clip)

CAMLexport value caml_cairo_copy_clip_rectangle_list(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal3(vlist, vrec, cons);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_rectangle_list_t* list = cairo_copy_clip_rectangle_list(cr);
  int i;
  cairo_rectangle_t *r;
  /* assert(list != NULL); */
  caml_cairo_raise_Error(list->status);
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


DO_CONTEXT(cairo_fill)
DO_CONTEXT(cairo_fill_preserve)

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

DO1_CONTEXT(cairo_mask, PATTERN_VAL)

CAMLexport value caml_cairo_mask_surface(value vcr, value vsurf,
                                         value vx, value vy)
{
  CAMLparam4(vcr, vsurf, vx, vy);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_mask_surface(cr, SURFACE_VAL(vsurf), Double_val(vx), Double_val(vy));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO_CONTEXT(cairo_paint)
DO1_CONTEXT(cairo_paint_with_alpha, Double_val)

DO_CONTEXT(cairo_stroke)
DO_CONTEXT(cairo_stroke_preserve)

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

DO_CONTEXT(cairo_copy_page)
DO_CONTEXT(cairo_show_page)


/* Paths -- Creating paths and manipulating path data
***********************************************************************/

CAMLexport value caml_cairo_copy_path(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_path_t* path = cairo_copy_path(CAIRO_VAL(vcr));
  caml_cairo_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

CAMLexport value caml_cairo_copy_path_flat(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vpath);
  cairo_path_t* path = cairo_copy_path_flat(CAIRO_VAL(vcr));
  caml_cairo_raise_Error(path->status);
  PATH_ASSIGN(vpath, path);
  CAMLreturn(vpath);
}

DO1_CONTEXT(cairo_append_path, PATH_VAL)

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

DO_CONTEXT(cairo_new_path)
DO_CONTEXT(cairo_new_sub_path)
DO_CONTEXT(cairo_close_path)

CAMLexport value caml_cairo_glyph_path(value vcr, value vglyphs)
{
  CAMLparam2(vcr, vglyphs);
  cairo_t* cr = CAIRO_VAL(vcr);
  cairo_glyph_t *glyphs, *p;
  int i, num_glyphs;

  ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs);
  cairo_glyph_path(cr, glyphs, num_glyphs);
  free(glyphs);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO1_CONTEXT(cairo_text_path, String_val)
GET_EXTENTS(cairo_path_extents)

DO5_CONTEXT(cairo_arc, Double_val, Double_val, Double_val, Double_val,
             Double_val)
DO5_CONTEXT(cairo_arc_negative, Double_val, Double_val, Double_val,
             Double_val, Double_val)
DO6_CONTEXT(cairo_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_CONTEXT(cairo_line_to, Double_val, Double_val)
DO2_CONTEXT(cairo_move_to, Double_val, Double_val)
DO4_CONTEXT(cairo_rectangle, Double_val, Double_val, Double_val, Double_val)

DO6_CONTEXT(cairo_rel_curve_to, Double_val, Double_val, Double_val,
             Double_val, Double_val, Double_val)
DO2_CONTEXT(cairo_rel_line_to, Double_val, Double_val)
DO2_CONTEXT(cairo_rel_move_to, Double_val, Double_val)


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
    vacc = caml_callback2(fn, vacc, vdata);
  }
  CAMLreturn(vacc);
}

CAMLexport value caml_cairo_path_to_array(value vpath)
{
  CAMLparam1(vpath);
  CAMLlocal2(varray, vdata);
  cairo_path_t * path = PATH_VAL(vpath);
  cairo_path_data_t *data;
  int i, el;

  /* Determine the number of elements in the path. */
  el = 0;
  for(i = 0; i < path->num_data; i += path->data[i].header.length)
    el++;
  varray = caml_alloc_tuple(el);
  /* Assign each element of the array. */
  el = 0;
  for(i = 0; i < path->num_data; i += path->data[i].header.length) {
    data = &path->data[i];
    PATH_DATA_ASSIGN(vdata, data);
    Store_field(varray, el, vdata);
    el++;
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
  int i, j, num_data;

  SET_MALLOC(path, 1, cairo_path_t);
  path->status = CAIRO_STATUS_SUCCESS;
  /* Compute the total length */
  num_data = 0;
#define ADD1 num_data += 1
#define ADD2(x,y) num_data += 2 /* 1 header + 1 point */
#define ADD4(x1,y1, x2,y2, x3,y3) num_data += 4 /* 1 header + 3 point */
  for(i = 0; i < length; i++) {
    vdata = Field(varray, i);
    SWITCH_PATH_DATA(vdata, ADD2, ADD2, ADD4, ADD1);
  }
  path->num_data = num_data;

#define MOVE(x1,y1)                             \
  data->header.type = CAIRO_PATH_MOVE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = Double_val(x1);             \
  data[1].point.y = Double_val(y1)
#define LINE(x1,y1)                             \
  data->header.type = CAIRO_PATH_LINE_TO;       \
  data->header.length = 2;                      \
  data[1].point.x = Double_val(x1);             \
  data[1].point.y = Double_val(y1)
#define CURVE(x1,y1, x2,y2, x3,y3)              \
  data->header.type = CAIRO_PATH_CURVE_TO;      \
  data->header.length = 4;                      \
  data[1].point.x = Double_val(x1);             \
  data[1].point.y = Double_val(y1);             \
  data[2].point.x = Double_val(x2);             \
  data[2].point.y = Double_val(y2);             \
  data[3].point.x = Double_val(x3);             \
  data[3].point.y = Double_val(y3)
#define CLOSE                                   \
  data->header.type = CAIRO_PATH_CLOSE_PATH;    \
  data->header.length = 1;

  path->data = malloc(num_data * sizeof(cairo_path_data_t));
  if (path->data == NULL) {
    free(path); /* free previously allocated memory */
    caml_raise_out_of_memory();
  }
  for(i = 0, j = 0; j < num_data; i++, j += data->header.length) {
    vdata = Field(varray, i);
    data = &path->data[j];
    SWITCH_PATH_DATA(vdata, MOVE, LINE, CURVE, CLOSE);
  }
  PATH_ASSIGN(vpath, path); /* vpath points to path */
  CAMLreturn(vpath);
}


/* Patterns -- Sources for drawing
***********************************************************************/

CAMLexport value caml_cairo_pattern_add_color_stop_rgb
(value vpat, value vofs, value vr, value vg, value vb)
{
  /* noalloc */
  cairo_pattern_add_color_stop_rgb(PATTERN_VAL(vpat), Double_val(vofs),
                                   Double_val(vr), Double_val(vg),
                                   Double_val(vb));
  return(Val_unit);
}


CAMLexport value caml_cairo_pattern_add_color_stop_rgba
(value vpat, value vofs, value vr, value vg, value vb, value va)
{
  /* noalloc */
  cairo_pattern_add_color_stop_rgba(PATTERN_VAL(vpat), Double_val(vofs),
                                    Double_val(vr), Double_val(vg),
                                    Double_val(vb), Double_val(va));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_add_color_stop_rgba_bc
(value * argv, int argn)
{
  return caml_cairo_pattern_add_color_stop_rgba
    (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}


CAMLexport value caml_cairo_pattern_get_color_stop_count(value vpat)
{
  CAMLparam1(vpat);
  int count;
  cairo_status_t st = cairo_pattern_get_color_stop_count(PATTERN_VAL(vpat),
                                                         &count);
  caml_cairo_raise_Error(st);
  CAMLreturn(Val_int(count));
}

CAMLexport value caml_cairo_pattern_get_color_stop_rgba(value vpat,
                                                        value vidx)
{
  CAMLparam2(vpat, vidx);
  CAMLlocal1(vcolors);
  double offset, red, green, blue, alpha;
  cairo_status_t st = cairo_pattern_get_color_stop_rgba
    (PATTERN_VAL(vpat), Int_val(vidx), &offset, &red, &green, &blue, &alpha);
  caml_cairo_raise_Error(st);
  /* tuple (offset, red, green, blue, alpha) */
  vcolors = caml_alloc_tuple(5);
  Store_field(vcolors, 0, caml_copy_double(offset));
  Store_field(vcolors, 1, caml_copy_double(red));
  Store_field(vcolors, 2, caml_copy_double(green));
  Store_field(vcolors, 3, caml_copy_double(blue));
  Store_field(vcolors, 4, caml_copy_double(alpha));
  CAMLreturn(vcolors);
}

CAMLexport value caml_cairo_pattern_create_rgb(value vr, value vg, value vb)
{
  CAMLparam3(vr,vg,vb);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_rgb(Double_val(vr),
                                                  Double_val(vg),
                                                  Double_val(vb));
  caml_cairo_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_create_rgba(value vr, value vg, value vb,
                                                value va)
{
  CAMLparam4(vr,vg,vb,va);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_rgba(Double_val(vr),
                                                   Double_val(vg),
                                                   Double_val(vb),
                                                   Double_val(va));
  caml_cairo_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_rgba(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vrgba);
  double red, green, blue, alpha;
  cairo_status_t st = cairo_pattern_get_rgba(PATTERN_VAL(vpat),
                                             &red, &green, &blue, &alpha);
  caml_cairo_raise_Error(st);
  vrgba = caml_alloc_tuple(4);
  Store_field(vrgba, 0, caml_copy_double(red));
  Store_field(vrgba, 1, caml_copy_double(green));
  Store_field(vrgba, 2, caml_copy_double(blue));
  Store_field(vrgba, 3, caml_copy_double(alpha));
  CAMLreturn(vrgba);
}

CAMLexport value caml_cairo_pattern_create_for_surface(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_for_surface(SURFACE_VAL(vsurf));
  caml_cairo_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_surface(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vsurf);
  cairo_surface_t *surface;
  cairo_status_t st = cairo_pattern_get_surface(PATTERN_VAL(vpat),
                                                &surface);
  caml_cairo_raise_Error(st);
  /* The surface is shared with the pattern => incr ref count. */
  cairo_surface_reference(surface);
  SURFACE_ASSIGN(vsurf, surface);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_pattern_create_linear
(value vx0, value vy0, value vx1, value vy1)
{
  CAMLparam4(vx0, vy0, vx1, vy1);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_linear
    (Double_val(vx0), Double_val(vy0), Double_val(vx1), Double_val(vy1));
  caml_cairo_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_get_linear_points(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vcoord);
  double x0, y0, x1, y1;
  cairo_status_t st = cairo_pattern_get_linear_points
    (PATTERN_VAL(vpat), &x0, &y0, &x1, &y1);
  caml_cairo_raise_Error(st);
  vcoord = caml_alloc_tuple(4);
  Store_field(vcoord, 0, caml_copy_double(x0));
  Store_field(vcoord, 1, caml_copy_double(y0));
  Store_field(vcoord, 2, caml_copy_double(x1));
  Store_field(vcoord, 3, caml_copy_double(y1));
  CAMLreturn(vcoord);
}

CAMLexport value caml_cairo_pattern_create_radial
(value vx0, value vy0, value vr0, value vx1, value vy1, value vr1)
{
  CAMLparam5(vx0, vy0, vr0, vx1, vy1);
  CAMLxparam1(vr1);
  CAMLlocal1(vpat);
  cairo_pattern_t* pat = cairo_pattern_create_radial
    (Double_val(vx0), Double_val(vy0), Double_val(vr0),
     Double_val(vx1), Double_val(vy1), Double_val(vr1));
  caml_cairo_raise_Error(cairo_pattern_status(pat));
  PATTERN_ASSIGN(vpat, pat);
  CAMLreturn(vpat);
}

CAMLexport value caml_cairo_pattern_create_radial_bc(value * argv, int argn)
{
  return caml_cairo_pattern_create_radial(argv[0], argv[1], argv[2], argv[3],
                                          argv[4], argv[5]);
}

CAMLexport value caml_cairo_pattern_get_radial_circles(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vcircles);
  double x0, y0, r0, x1, y1, r1;
  cairo_status_t st = cairo_pattern_get_radial_circles
    (PATTERN_VAL(vpat), &x0, &y0, &r0, &x1, &y1, &r1);
  caml_cairo_raise_Error(st);
  vcircles = caml_alloc_tuple(6);
  Store_field(vcircles, 0, caml_copy_double(x0));
  Store_field(vcircles, 1, caml_copy_double(y0));
  Store_field(vcircles, 2, caml_copy_double(r0));
  Store_field(vcircles, 3, caml_copy_double(x1));
  Store_field(vcircles, 4, caml_copy_double(y1));
  Store_field(vcircles, 5, caml_copy_double(r1));
  CAMLreturn(vcircles);
}

CAMLexport value caml_cairo_pattern_set_extend(value vpat, value vextend)
{
  /* noalloc */
  cairo_pattern_set_extend(PATTERN_VAL(vpat), EXTEND_VAL(vextend));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_extend(value vpat)
{
  CAMLparam1(vpat);
  cairo_extend_t extend = cairo_pattern_get_extend(PATTERN_VAL(vpat));
  CAMLreturn(VAL_EXTEND(extend));
}

CAMLexport value caml_cairo_pattern_set_filter(value vpat, value vfilter)
{
  /* noalloc */
  cairo_pattern_set_filter(PATTERN_VAL(vpat), FILTER_VAL(vfilter));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_filter(value vpat)
{
  CAMLparam1(vpat);
  cairo_filter_t filter = cairo_pattern_get_filter(PATTERN_VAL(vpat));
  CAMLreturn(VAL_FILTER(filter));
}

CAMLexport value caml_cairo_pattern_set_matrix(value vpat, value vmat)
{
  /* noalloc */
  ALLOC_CAIRO_MATRIX(vmat);
  cairo_pattern_set_matrix(PATTERN_VAL(vpat), GET_MATRIX(vmat));
  return(Val_unit);
}

CAMLexport value caml_cairo_pattern_get_matrix(value vpat)
{
  CAMLparam1(vpat);
  CAMLlocal1(vmat);
  WITH_MATRIX_DO(vmat,
                 cairo_pattern_get_matrix(PATTERN_VAL(vpat), GET_MATRIX(vmat)));
  CAMLreturn(vmat);
}


/* Transformations - Manipulating the current transformation matrix
***********************************************************************/

DO2_CONTEXT(cairo_translate, Double_val, Double_val)
DO2_CONTEXT(cairo_scale, Double_val, Double_val)
DO1_CONTEXT(cairo_rotate, Double_val)

CAMLexport value caml_cairo_transform(value vcr, value vmat)
{
  /* noalloc */
  ALLOC_CAIRO_MATRIX(vmat);
  cairo_transform(CAIRO_VAL(vcr), GET_MATRIX(vmat));
  return(Val_unit);
}

CAMLexport value caml_cairo_set_matrix(value vcr, value vmat)
{
  /* noalloc */
  ALLOC_CAIRO_MATRIX(vmat);
  cairo_set_matrix(CAIRO_VAL(vcr), GET_MATRIX(vmat));
  return(Val_unit);
}

CAMLexport value caml_cairo_get_matrix(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vmat);
  WITH_MATRIX_DO(vmat,
                 cairo_get_matrix(CAIRO_VAL(vcr), GET_MATRIX(vmat)));
  CAMLreturn(vmat);
}

DO_CONTEXT(cairo_identity_matrix)

#define COORD_TRANSFORM(name)                                 \
  CAMLexport value caml_##name(value vcr, value vx, value vy) \
  {                                                           \
    CAMLparam3(vcr, vx, vy);                                  \
    CAMLlocal1(vcouple);                                      \
    cairo_t* cr = CAIRO_VAL(vcr);                             \
    double x = Double_val(vx);                                \
    double y = Double_val(vy);                                \
    name(cr, &x, &y);                                         \
    vcouple = caml_alloc_tuple(2);                            \
    Store_field(vcouple, 0, caml_copy_double(x));             \
    Store_field(vcouple, 1, caml_copy_double(y));             \
    CAMLreturn(vcouple);                                      \
  }

COORD_TRANSFORM(cairo_user_to_device)
COORD_TRANSFORM(cairo_user_to_device_distance)
COORD_TRANSFORM(cairo_device_to_user)
COORD_TRANSFORM(cairo_device_to_user_distance)


/* Font options
***********************************************************************/

DO1_CONTEXT(cairo_set_font_options, FONT_OPTIONS_VAL)

CAMLexport value caml_cairo_get_font_options(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vfont_option);
  cairo_font_options_t *options = cairo_font_options_create();
  caml_cairo_raise_Error(cairo_font_options_status(options));
  cairo_get_font_options(CAIRO_VAL(vcr), options);
  FONT_OPTIONS_ASSIGN(vfont_option, options);
  CAMLreturn(vfont_option);
}

CAMLexport value caml_cairo_font_options_create(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal1(vfo);
  cairo_font_options_t* fo = cairo_font_options_create();
  caml_cairo_raise_Error(cairo_font_options_status(fo));
  FONT_OPTIONS_ASSIGN(vfo, fo);
  CAMLreturn(vfo);
}

CAMLexport value caml_cairo_font_options_copy(value vorig)
{
  CAMLparam1(vorig);
  CAMLlocal1(vcopy);
  cairo_font_options_t* copy = cairo_font_options_copy(FONT_OPTIONS_VAL(vorig));
  caml_cairo_raise_Error(cairo_font_options_status(copy));
  FONT_OPTIONS_ASSIGN(vcopy, copy);
  CAMLreturn(vcopy);
}

#define SET_FONT_OPTIONS(name, of_val)                  \
  CAMLexport value caml_##name(value vfo, value v)      \
  {                                                     \
    CAMLparam2(vfo, v);                                 \
    name(FONT_OPTIONS_VAL(vfo), of_val(v));             \
    CAMLreturn(Val_unit);                               \
  }

#define GET_FONT_OPTIONS(name, val_of, type)            \
  CAMLexport value caml_##name(value vfo)               \
  {                                                     \
    CAMLparam1(vfo);                                    \
    type ret = name(FONT_OPTIONS_VAL(vfo));             \
    CAMLreturn(val_of(ret));                            \
  }

SET_FONT_OPTIONS(cairo_font_options_merge, FONT_OPTIONS_VAL)
SET_FONT_OPTIONS(cairo_font_options_set_antialias, ANTIALIAS_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_antialias,
                 VAL_ANTIALIAS, cairo_antialias_t)

#define SUBPIXEL_ORDER_VAL(v) ((cairo_subpixel_order_t) Int_val(v))
#define VAL_SUBPIXEL_ORDER(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_subpixel_order, SUBPIXEL_ORDER_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_subpixel_order,
                 VAL_SUBPIXEL_ORDER, cairo_subpixel_order_t)

#define HINT_STYLE_VAL(v) ((cairo_hint_style_t) Int_val(v))
#define VAL_HINT_STYLE(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_hint_style, HINT_STYLE_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_hint_style,
                 VAL_HINT_STYLE, cairo_hint_style_t)

#define HINT_METRICS_VAL(v) ((cairo_hint_metrics_t) Int_val(v))
#define VAL_HINT_METRICS(v) Val_int(v)

SET_FONT_OPTIONS(cairo_font_options_set_hint_metrics, HINT_METRICS_VAL)
GET_FONT_OPTIONS(cairo_font_options_get_hint_metrics,
                 VAL_HINT_METRICS, cairo_hint_metrics_t)

/* Font face
***********************************************************************/

CAMLexport value caml_cairo_font_face_get_type(value vff)
{
  CAMLparam1(vff);
  cairo_font_type_t ft = cairo_font_face_get_type(FONT_FACE_VAL(vff));
  CAMLreturn(VAL_FONT_TYPE(ft));
}


DO1_CONTEXT(cairo_set_font_face, FONT_FACE_VAL)

CAMLexport value caml_cairo_get_font_face(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vff);
  cairo_font_face_t* ff = cairo_get_font_face(CAIRO_VAL(vcr));
  caml_cairo_raise_Error(cairo_font_face_status(ff));
  /* Since we are going to create a value with the [ff] and this value
     is shared with the one hold inside the cairo context, one must
     increase the reference count (to avoid that destroying one of
     these object leaves a dangling pointer for the other).  */
  cairo_font_face_reference(ff);
  FONT_FACE_ASSIGN(vff, ff);
  CAMLreturn(vff);
}

CAMLexport value caml_cairo_toy_font_face_create
(value vfamily, value vslant, value vweight)
{
  CAMLparam3(vfamily, vslant, vweight);
  CAMLlocal1(vff);
  cairo_font_face_t* ff;
  ff = cairo_toy_font_face_create(String_val(vfamily), SLANT_VAL(vslant),
                                  WEIGHT_VAL(vweight));
  caml_cairo_raise_Error(cairo_font_face_status(ff));
  FONT_FACE_ASSIGN(vff, ff);
  CAMLreturn(vff);
}

CAMLexport value caml_cairo_toy_font_face_get_family(value vff)
{
  CAMLparam1(vff);
  const char* family = cairo_toy_font_face_get_family(FONT_FACE_VAL(vff));
  /* Since the string is going to be copied, it does not matter that
     it belongs to the font face. */
  CAMLreturn(caml_copy_string(family));
}

CAMLexport value caml_cairo_toy_font_face_get_slant(value vff)
{
  CAMLparam1(vff);
  cairo_font_slant_t slant = cairo_toy_font_face_get_slant(FONT_FACE_VAL(vff));
  CAMLreturn(VAL_SLANT(slant));
}

CAMLexport value caml_cairo_toy_font_face_get_weight(value vff)
{
  CAMLparam1(vff);
  cairo_font_weight_t w = cairo_toy_font_face_get_weight(FONT_FACE_VAL(vff));
  CAMLreturn(VAL_WEIGHT(w));
}


/* Scaled font
***********************************************************************/

DO1_CONTEXT(cairo_set_scaled_font, SCALED_FONT_VAL)

CAMLexport value caml_cairo_get_scaled_font(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vsf);
  cairo_scaled_font_t* sf = cairo_get_scaled_font(CAIRO_VAL(vcr));
  /* create a value with shared [sf] => must increase ref. count */
  cairo_scaled_font_reference(sf);
  vsf = ALLOC(scaled_font);
  SCALED_FONT_VAL(vsf) = sf;
  CAMLreturn(vsf);
}

CAMLexport value caml_cairo_scaled_font_create
(value vff, value vfont_matrix, value vctm, value voptions)
{
  CAMLparam4(vff, vfont_matrix, vctm, voptions);
  CAMLlocal1(vsf);
  ALLOC_CAIRO_MATRIX2(vfont_matrix, vctm);
  cairo_scaled_font_t* sf = cairo_scaled_font_create
    (FONT_FACE_VAL(vff), GET_MATRIX(vfont_matrix), GET_MATRIX(vctm),
     FONT_OPTIONS_VAL(voptions));
  vsf = ALLOC(scaled_font);
  SCALED_FONT_VAL(vsf) = sf;
  CAMLreturn(vsf);
}

CAMLexport value caml_cairo_scaled_font_extents(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vfe);
  cairo_font_extents_t fe;
  cairo_scaled_font_extents(SCALED_FONT_VAL(vsf), &fe);
  FONT_EXTENTS_ASSIGN(vfe, fe);
  CAMLreturn(vfe);
}

CAMLexport value caml_cairo_scaled_font_text_extents(value vsf, value vutf8)
{
  CAMLparam2(vsf, vutf8);
  CAMLlocal1(vte);
  cairo_text_extents_t te;
  cairo_scaled_font_text_extents(SCALED_FONT_VAL(vsf), String_val(vutf8), &te);
  TEXT_EXTENTS_ASSIGN(vte, te);
  CAMLreturn(vte);
}

CAMLexport value caml_cairo_scaled_font_glyph_extents(value vsf, value vglyphs)
{
  CAMLparam2(vsf, vglyphs);
  CAMLlocal1(vte);
  cairo_text_extents_t te;
  cairo_glyph_t *glyphs, *p;
  int i, num_glyphs;

  ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs);
  cairo_scaled_font_glyph_extents(SCALED_FONT_VAL(vsf),
                                  glyphs, num_glyphs, &te);
  free(glyphs);
  vte = caml_alloc(6 * Double_wosize, Double_array_tag);
  Store_double_field(vte, 0, te.x_bearing);
  Store_double_field(vte, 1, te.y_bearing);
  Store_double_field(vte, 2, te.width);
  Store_double_field(vte, 3, te.height);
  Store_double_field(vte, 4, te.x_advance);
  Store_double_field(vte, 5, te.y_advance);
  CAMLreturn(vte);
}

CAMLexport value caml_cairo_scaled_font_text_to_glyphs
(value vsf, value vx, value vy, value vutf8)
{
  CAMLparam4(vsf, vx, vy, vutf8);
  CAMLlocal4(vglyphs, vclusters, vtriplet, v);
  cairo_glyph_t *glyphs = NULL;
  int i, num_glyphs;
  cairo_text_cluster_t *clusters = NULL;
  int num_clusters;
  cairo_text_cluster_flags_t cluster_flags;
  cairo_status_t status;

  status = cairo_scaled_font_text_to_glyphs
    (SCALED_FONT_VAL(vsf), Double_val(vx), Double_val(vy),
     String_val(vutf8), caml_string_length(vutf8),
     &glyphs, &num_glyphs,  &clusters, &num_clusters,  &cluster_flags);
  caml_cairo_raise_Error(status);

  vglyphs = caml_alloc_tuple(num_glyphs);
  for(i = 0; i < num_glyphs; i++) {
    GLYPH_ASSIGN(v, glyphs[i]);
    Store_field(vglyphs, i, v);
  }
  cairo_glyph_free(glyphs);
  vclusters = caml_alloc_tuple(num_clusters);
  for(i = 0; i < num_clusters; i++) {
    CLUSTER_ASSIGN(v, clusters[i]);
    Store_field(vclusters, i, v);
  }
  cairo_text_cluster_free(clusters);
  /* FIXME: cluster_flags */
  /* (glyphs, clusters, cluster_flags) */
  vtriplet = caml_alloc_tuple(3);
  Store_field(vtriplet, 0, vglyphs);
  Store_field(vtriplet, 1, vclusters);
  Store_field(vtriplet, 2, VAL_CLUSTER_FLAGS(cluster_flags));
  CAMLreturn(vtriplet);
}

CAMLexport value caml_cairo_scaled_font_get_font_face(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vff);
  cairo_font_face_t* ff;
  ff = cairo_scaled_font_get_font_face(SCALED_FONT_VAL(vsf));
  /* FIXME: The documentation does not say whether it is shared or
     not; assuming it is as for other functions. */
  cairo_font_face_reference(ff);
  vff = ALLOC(font_face);
  FONT_FACE_VAL(vff) = ff;
  CAMLreturn(vff);
}


CAMLexport value caml_cairo_scaled_font_get_font_options(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vfo);
  cairo_font_options_t *fo = cairo_font_options_create();
  caml_cairo_raise_Error(cairo_font_options_status(fo));
  cairo_scaled_font_get_font_options(SCALED_FONT_VAL(vsf), fo);
  FONT_OPTIONS_ASSIGN(vfo, fo);
  CAMLreturn(vfo);
}

#define SCALED_FONT_GET_MATRIX(name)                                    \
  CAMLexport value caml_##name(value vsf)                               \
  {                                                                     \
    CAMLparam1(vsf);                                                    \
    CAMLlocal1(vmatrix);                                                \
    WITH_MATRIX_DO(vmatrix,                                             \
                   name(SCALED_FONT_VAL(vsf), GET_MATRIX(vmatrix)));    \
    CAMLreturn(vmatrix);                                                \
  }

SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_font_matrix)
SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_ctm)
SCALED_FONT_GET_MATRIX(cairo_scaled_font_get_scale_matrix)

CAMLexport value caml_cairo_scaled_font_get_type(value vff)
{
  CAMLparam1(vff);
  cairo_font_type_t ft = cairo_scaled_font_get_type(SCALED_FONT_VAL(vff));
  CAMLreturn(VAL_FONT_TYPE(ft));
}


/* Ft : TrueType fonts
***********************************************************************/

#if CAIRO_HAS_FT_FONT && CAIRO_HAS_FC_FONT
#include <cairo-ft.h>

CAMLexport value caml_cairo_Ft_init_FreeType(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(vft);
  FT_Library ft;

  if (FT_Init_FreeType (&ft) != 0) {
    caml_failwith("Cairo.Ft: cannot initialize the FreeType library");
  }
  FT_LIBRARY_ASSIGN(vft, ft);
  CAMLreturn(vft);
}

CAMLexport
value caml_cairo_Ft_new_face(value vftlib, value vpath, value vindex)
{
  CAMLparam3(vftlib, vpath, vindex);
  CAMLlocal1(vface);
  FT_Face face;

  if (FT_New_Face(FT_LIBRARY_VAL(vftlib),
                  (const char*) String_val(vpath),
                  Int_val(vindex),
                  &face) != 0) {
    caml_failwith("Cairo.Ft.face");
  }
  FT_FACE_ASSIGN(vface, face);
  CAMLreturn(vface);
}

CAMLexport value caml_cairo_ft_create_for_ft_face(
  value vface, value vertical, value autohint)
{
  CAMLparam3(vface, vertical, autohint);
  CAMLlocal1(vff);
  FT_Int32 flags = FT_LOAD_DEFAULT;
  cairo_font_face_t *ff;

  if (Bool_val(vertical)) flags |= FT_LOAD_VERTICAL_LAYOUT;
  if (Bool_val(autohint)) flags |= FT_LOAD_FORCE_AUTOHINT;

  ff = cairo_ft_font_face_create_for_ft_face(FT_FACE_VAL(vface), flags);
  caml_cairo_raise_Error(cairo_font_face_status(ff));
  FONT_FACE_ASSIGN(vff, ff);
  CAMLreturn(vff);
}

CAMLexport value caml_cairo_ft_create_for_pattern(
  value voptions, value vpattern)
{
  CAMLparam2(voptions, vpattern);
  CAMLlocal1(vff);
  FcPattern *p1, *p2;
  FcResult res;
  cairo_font_face_t *ff;

  p1 = FcNameParse((const FcChar8 *) String_val(vpattern));
  if (FcConfigSubstitute(NULL, p1, FcMatchPattern) == FcFalse)
    caml_failwith("Cairo.Ft.create_for_pattern:");
  if (Is_block (voptions)) {
      cairo_ft_font_options_substitute(FONT_OPTIONS_VAL(Field(voptions, 0)),
                                       p1);
  }
  FcDefaultSubstitute(p1);
  p2 = FcFontMatch(NULL, p1, &res);
  FcPatternDestroy(p1);
  switch (res) {
  case FcResultMatch:  break;
  case FcResultNoMatch:
    caml_failwith("Cairo.Ft.create_for_pattern: no match");
  case FcResultTypeMismatch:
    caml_failwith("Cairo.Ft.create_for_pattern: type mismatch");
  case FcResultNoId:
    caml_failwith("Cairo.Ft.create_for_pattern: font exists but does not "
                  "have enough values");
  case FcResultOutOfMemory:
    caml_failwith("Cairo.Ft.create_for_pattern: out of memory ");
  }

  ff = cairo_ft_font_face_create_for_pattern(p2);
  FONT_FACE_ASSIGN(vff, ff);  
  FcPatternDestroy(p2);
  CAMLreturn(vff);
}

CAMLexport value caml_cairo_ft_scaled_font_lock_face(value vsf)
{
  CAMLparam1(vsf);
  CAMLlocal1(vface);
  FT_Face face;

  face = cairo_ft_scaled_font_lock_face(SCALED_FONT_VAL(vsf));
  FT_FACE_ASSIGN(vface, face);
  CAMLreturn(vface);
}

CAMLexport value caml_cairo_ft_scaled_font_unlock_face(value vsf)
{
  CAMLparam1(vsf);
  cairo_ft_scaled_font_unlock_face(SCALED_FONT_VAL(vsf));
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_ft_synthesize_get(value vff)
{
  CAMLparam1(vff);
  CAMLlocal1(vsyn);
  unsigned int syn;
  
  syn = cairo_ft_font_face_get_synthesize(FONT_FACE_VAL(vff));
  vsyn = caml_alloc(2, 0);
  Store_field(vsyn, 0, Val_bool(syn & CAIRO_FT_SYNTHESIZE_BOLD));
  Store_field(vsyn, 1, Val_bool(syn & CAIRO_FT_SYNTHESIZE_OBLIQUE));
  CAMLreturn(vsyn);
}

CAMLexport value caml_cairo_ft_synthesize_set(
  value vff, value vbold, value voblique)
{
  CAMLparam3(vff, vbold, voblique);
  unsigned int synth_flags = 0;

  if (Bool_val(vbold)) synth_flags |= CAIRO_FT_SYNTHESIZE_BOLD;
  if (Bool_val(voblique)) synth_flags |= CAIRO_FT_SYNTHESIZE_OBLIQUE;
  cairo_ft_font_face_set_synthesize(FONT_FACE_VAL(vff), synth_flags);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_ft_synthesize_unset(
  value vff, value vbold, value voblique)
{
  CAMLparam3(vff, vbold, voblique);
  unsigned int synth_flags = 0;

  if (Bool_val(vbold)) synth_flags |= CAIRO_FT_SYNTHESIZE_BOLD;
  if (Bool_val(voblique)) synth_flags |= CAIRO_FT_SYNTHESIZE_OBLIQUE;
  cairo_ft_font_face_unset_synthesize(FONT_FACE_VAL(vff), synth_flags);
  CAMLreturn(Val_unit);
}


#else

UNAVAILABLE1(Ft_init_FreeType)
UNAVAILABLE2(caml_Ft_new_face)
UNAVAILABLE3(caml_cairo_ft_create_for_ft_face)
UNAVAILABLE2(caml_cairo_ft_create_for_pattern)
UNAVAILABLE1(caml_cairo_ft_scaled_font_lock_face)
UNAVAILABLE1(caml_cairo_ft_scaled_font_unlock_face)
UNAVAILABLE1(caml_cairo_ft_synthesize_get)
UNAVAILABLE3(caml_cairo_ft_synthesize_set)
UNAVAILABLE3(caml_cairo_ft_synthesize_unset)

#endif

/* Glyphs
***********************************************************************/

CAMLexport value caml_cairo_show_glyphs(value vcr, value vglyphs)
{
  CAMLparam1(vcr);
  cairo_t *cr = CAIRO_VAL(vcr);
  int i, num_glyphs = Wosize_val(vglyphs);
  cairo_glyph_t *glyphs, *p;

  ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs);
  cairo_show_glyphs(cr, glyphs, num_glyphs);
  free(glyphs);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_show_text_glyphs
(value vcr, value vutf8, value vglyphs, value vclusters, value vcluster_flags)
{
  CAMLparam5(vcr, vutf8, vglyphs, vclusters, vcluster_flags);
  CAMLlocal1(v);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_glyph_t *glyphs, *p;
  cairo_text_cluster_t *clusters, *q;
  int i, num_glyphs, num_clusters;

  ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs);
  ARRAY_CLUSTER_VAL(clusters, q, vglyphs, num_glyphs);
  cairo_show_text_glyphs(cr, String_val(vutf8), caml_string_length(vutf8),
                         glyphs, num_glyphs, clusters, num_clusters,
                         /* FIXME: is it a binary | ? */
                         CLUSTER_FLAGS_VAL(vcluster_flags));
  free(glyphs);
  free(clusters);
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_glyph_extents(value vcr, value vglyphs)
{
  CAMLparam2(vcr, vglyphs);
  CAMLlocal1(vte);
  cairo_glyph_t *glyphs, *p;
  int i, num_glyphs;
  cairo_text_extents_t te;

  ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs);
  cairo_glyph_extents(CAIRO_VAL(vcr), glyphs, num_glyphs, &te);
  free(glyphs);
  TEXT_EXTENTS_ASSIGN(vte, te);
  CAMLreturn(vte);
}



/* Toy text API
 ***********************************************************************/

CAMLexport value caml_cairo_select_font_face
(value vcr, value vslant, value vweight, value vfamily)
{
  CAMLparam4(vcr, vslant, vweight, vfamily);
  cairo_t *cr = CAIRO_VAL(vcr);
  cairo_select_font_face(cr, String_val(vfamily),
                         SLANT_VAL(vslant), WEIGHT_VAL(vweight));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

DO1_CONTEXT(cairo_set_font_size, Double_val)

CAMLexport value caml_cairo_set_font_matrix(value vcr, value vmatrix)
{
  CAMLparam2(vcr, vmatrix);
  cairo_t *cr = CAIRO_VAL(vcr);
  ALLOC_CAIRO_MATRIX(vmatrix);
  cairo_set_font_matrix(cr, GET_MATRIX(vmatrix));
  caml_check_status(cr);
  CAMLreturn(Val_unit);
}

CAMLexport value caml_cairo_get_font_matrix(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vmatrix);
  cairo_t *cr = CAIRO_VAL(vcr);
  WITH_MATRIX_DO(vmatrix,
                 cairo_get_font_matrix(cr, GET_MATRIX(vmatrix)));
  CAMLreturn(vmatrix);
}

DO1_CONTEXT(cairo_show_text, String_val)

CAMLexport value caml_cairo_font_extents(value vcr)
{
  CAMLparam1(vcr);
  CAMLlocal1(vfe);
  cairo_font_extents_t fe;
  cairo_font_extents(CAIRO_VAL(vcr), &fe);
  FONT_EXTENTS_ASSIGN(vfe, fe);
  CAMLreturn(vfe);
}

CAMLexport value caml_cairo_text_extents(value vcr, value vutf8)
{
  CAMLparam2(vcr, vutf8);
  CAMLlocal1(vte);
  cairo_text_extents_t te;
  cairo_text_extents(CAIRO_VAL(vcr), String_val(vutf8), &te);
  TEXT_EXTENTS_ASSIGN(vte, te);
  CAMLreturn(vte);
}


/* Surface
***********************************************************************/

static cairo_user_data_key_t image_bigarray_key;
/* See the Image surfaces below */

CAMLexport value caml_cairo_surface_create_similar
(value vother, value vcontent, value vwidth, value vheight)
{
  CAMLparam4(vother, vcontent, vwidth, vheight);
  CAMLlocal1(vsurf);
  cairo_content_t content;
  cairo_surface_t* surf;

  SET_CONTENT_VAL(content, vcontent);
  surf = cairo_surface_create_similar(SURFACE_VAL(vother), content,
                                      Int_val(vwidth), Int_val(vheight));
  caml_cairo_raise_Error(cairo_surface_status(surf));
  SURFACE_ASSIGN(vsurf, surf);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_surface_finish(value vsurf)
{
  /* noalloc */
  cairo_surface_t *surface = SURFACE_VAL(vsurf);

  cairo_surface_finish(surface);
  /* Remove the user data with the bigarray key.  That will cause the
     finalizer to be executed (and release the proxy) and the
     finalizing function not to be called again when the value is
     garbage collected. */
  cairo_surface_set_user_data(surface, &image_bigarray_key, NULL, NULL);
  return(Val_unit);
}

DO_SURFACE(cairo_surface_flush)

CAMLexport value caml_cairo_surface_get_font_options(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vfo);
  cairo_surface_t *surface = SURFACE_VAL(vsurf);
  cairo_font_options_t *fo = cairo_font_options_create();
  caml_cairo_raise_Error(cairo_font_options_status(fo));
  cairo_surface_get_font_options(surface, fo);
  FONT_OPTIONS_ASSIGN(vfo, fo);
  CAMLreturn(vfo);
}

CAMLexport value caml_cairo_surface_get_content(value vsurf)
{
  CAMLparam1(vsurf);
  CAMLlocal1(vcontent);
  cairo_surface_t *surface = SURFACE_VAL(vsurf);
  cairo_content_t content = cairo_surface_get_content(surface);
  CONTENT_ASSIGN(vcontent, content);
  CAMLreturn(vcontent);
}

DO_SURFACE(cairo_surface_mark_dirty)

CAMLexport value caml_cairo_surface_mark_dirty_rectangle
(value vsurf, value vx, value vy, value vwidth, value vheight)
{
  /* noalloc */
  cairo_surface_mark_dirty_rectangle
    (SURFACE_VAL(vsurf), Int_val(vx), Int_val(vy),
     Int_val(vwidth), Int_val(vheight));
  return(Val_unit);
}

#define SET_SURFACE_XY(name)                                    \
  CAMLexport value caml_##name(value vsurf, value vx, value vy) \
  {                                                             \
    /* noalloc */                                               \
    name(SURFACE_VAL(vsurf), Double_val(vx), Double_val(vy));   \
    return(Val_unit);                                           \
  }

#define GET_SURFACE_XY(name)                                    \
  CAMLexport value caml_##name(value vsurf)                     \
  {                                                             \
    CAMLparam1(vsurf);                                          \
    CAMLlocal1(vcouple);                                        \
    double x, y;                                                \
    name(SURFACE_VAL(vsurf), &x, &y);                           \
    vcouple = caml_alloc_tuple(2);                              \
    Store_field(vcouple, 0, caml_copy_double(x));               \
    Store_field(vcouple, 1, caml_copy_double(y));               \
    CAMLreturn(vcouple);                                        \
  }

SET_SURFACE_XY(cairo_surface_set_device_offset)
GET_SURFACE_XY(cairo_surface_get_device_offset)
SET_SURFACE_XY(cairo_surface_set_fallback_resolution)
GET_SURFACE_XY(cairo_surface_get_fallback_resolution)


CAMLexport value caml_cairo_surface_get_type(value vsurf)
{
  /* noalloc */
  cairo_surface_type_t k = cairo_surface_get_type(SURFACE_VAL(vsurf));
  return(VAL_SURFACE_KIND(k));
}

DO_SURFACE(cairo_surface_copy_page)
DO_SURFACE(cairo_surface_show_page)

CAMLexport value caml_cairo_surface_has_show_text_glyphs(value vsurf)
{
  /* noalloc */
  cairo_bool_t b = cairo_surface_has_show_text_glyphs(SURFACE_VAL(vsurf));
  return(Val_bool(b));
}


/* Image surfaces
***********************************************************************/

#define FORMAT_VAL(x) ((cairo_format_t) Int_val(x))
#define VAL_FORMAT(x) Val_int(x)

#ifdef CAIRO_HAS_IMAGE_SURFACE

/* Any image surface may be queried for its data (which is returned
   shared with the surface).  We cannot track the data unless we
   allocate it ourselves.  Since it can be shared with other surfaces
   and bigarrays, we will see an image surface as a kind of bigarray:
   it will hold a bigarray proxy that will be referenced by all
   bigarrays and surfaces created from them (and ref count the data).  */

/* Finalize the proxy attached to the image surface. */
static void caml_cairo_image_bigarray_finalize(void *data)
{
#define proxy ((struct caml_ba_proxy *) data)
  /* Adapted from caml_ba_finalize in the OCaml library sources. */
  if (-- proxy->refcount == 0) {
    free(proxy->data);
    free(proxy);
  }
#undef proxy
}

CAMLexport value caml_cairo_image_surface_create(value vformat,
                                                 value vwidth, value vheight)
{
  CAMLparam3(vformat, vwidth, vheight);
  CAMLlocal1(vsurf);
  cairo_format_t format = FORMAT_VAL(vformat);
  int stride = cairo_format_stride_for_width(format, Int_val(vwidth));
  unsigned char *data;
  cairo_surface_t *surf;
  struct caml_ba_proxy *proxy;
  cairo_status_t status;

  vsurf = ALLOC(surface); /* alloc this first in case it raises an exn */
  /* Use calloc to initialize the surface to all black. */
  data = calloc(1, stride * Int_val(vheight));
  if (data == NULL) caml_raise_out_of_memory();
  surf = cairo_image_surface_create_for_data(data, format, Int_val(vwidth),
                                             Int_val(vheight), stride);
  status = cairo_surface_status(surf);
  if (status != CAIRO_STATUS_SUCCESS) {
    free(data);
    caml_cairo_raise_Error(status);
  }
  /* Create a proxy and attach it to the surface */
  proxy = malloc(sizeof(struct caml_ba_proxy));
  if (proxy == NULL) {
    cairo_surface_destroy(surf);
    free(data);
    caml_cairo_raise_Error(CAIRO_STATUS_NO_MEMORY);
  }
  proxy->refcount = 1;      /* surface */
  proxy->data = data;
  proxy->size = 0;
  status = cairo_surface_set_user_data(surf, &image_bigarray_key, proxy,
                                       caml_cairo_image_bigarray_finalize);
  if (status != CAIRO_STATUS_SUCCESS) {
    cairo_surface_destroy(surf);
    free(data);
    free(proxy);
    caml_cairo_raise_Error(status);
  }
  SURFACE_VAL(vsurf) = surf;
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_format_stride_for_width(value vformat, value vw)
{
  /* noalloc */
  return Val_int(cairo_format_stride_for_width(FORMAT_VAL(vformat),
                                               Int_val(vw)));
}


/* Attach a proxy to the bigarray (no need to create another bigarray
   refering to the same proxy as for sub-arrays).  This proxy is
   finalized when the surface is destroyed. */
static cairo_status_t caml_cairo_image_bigarray_attach_proxy
(cairo_surface_t* surf, struct caml_ba_array * b)
{
  struct caml_ba_proxy * proxy;

  if ((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL)
    return(CAIRO_STATUS_SUCCESS);
  if (b->proxy != NULL) {
    /* If b is already a proxy, increment refcount. */
    /* FIXME, use caml_atomic_refcount_incr if available. */
    ++ b->proxy->refcount;
  }
  else {
    /* Otherwise, create proxy and attach it to b and the surface.
       (Adapted from caml_ba_update_proxy in the OCaml std lib.) */
    proxy = malloc(sizeof(struct caml_ba_proxy));
    if (proxy == NULL) return(CAIRO_STATUS_NO_MEMORY);
    proxy->refcount = 2;      /* original array + surface */
    proxy->data = b->data;
    proxy->size = 0; /* CAML_BA_MAPPED_FILE excluded by the calling fun */
    b->proxy = proxy;
  }
  return cairo_surface_set_user_data(surf, &image_bigarray_key, b->proxy,
                                     caml_cairo_image_bigarray_finalize);
}

#define b (Caml_ba_array_val(vb))
#define SURFACE_CREATE_DATA(name)                                       \
  CAMLexport value caml_cairo_image_surface_create_for_##name           \
  (value vb, value vformat, value vwidth, value vheight, value vstride) \
  {                                                                     \
    CAMLparam5(vb, vformat, vwidth, vheight, vstride);                  \
    CAMLlocal1(vsurf);                                                  \
    cairo_surface_t* surf;                                              \
    const int width =  Int_val(vwidth);                                 \
    cairo_status_t status;                                              \
                                                                        \
    if ((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_MAPPED_FILE)       \
      caml_invalid_argument("Cairo.Image.create_for_" #name             \
                            ": cannot use a memory mapped file.");      \
    vsurf = ALLOC(surface); /* alloc this first in case it raises an exn */ \
    surf = cairo_image_surface_create_for_data                          \
      ((unsigned char *) b->data, FORMAT_VAL(vformat),                  \
       width, Int_val(vheight), Int_val(vstride));                      \
    caml_cairo_raise_Error(cairo_surface_status(surf));                 \
    status = caml_cairo_image_bigarray_attach_proxy(surf, b);           \
    if (status != CAIRO_STATUS_SUCCESS) {                               \
      cairo_surface_destroy(surf);                                      \
      caml_cairo_raise_Error(status);                                   \
    }                                                                   \
    SURFACE_VAL(vsurf) = surf;                                          \
    CAMLreturn(vsurf);                                                  \
  }

SURFACE_CREATE_DATA(data8)
SURFACE_CREATE_DATA(data32)
#undef b

#define SURFACE_GET_DATA(type, num_dims, dims ...)                      \
  CAMLexport value caml_cairo_image_surface_get_##type(value vsurf)     \
  {                                                                     \
    CAMLparam1(vsurf);                                                  \
    CAMLlocal1(vb);                                                     \
    unsigned char* data = cairo_image_surface_get_data(SURFACE_VAL(vsurf)); \
    intnat dim[num_dims] = {dims};                                      \
    struct caml_ba_proxy * proxy = (struct caml_ba_proxy *)             \
      cairo_surface_get_user_data(SURFACE_VAL(vsurf), &image_bigarray_key); \
                                                                        \
    if (data == NULL)                                                   \
      caml_invalid_argument("Cairo.Image.get_data: not an image surface.");  \
    if (proxy == NULL) {                                                \
      /* We assume the payload is externally managed */                \
      vb = caml_ba_alloc(CAML_BA_##type | CAML_BA_C_LAYOUT              \
                         | CAML_BA_EXTERNAL,                            \
                         num_dims, data, dim);                          \
    } else {                                                            \
      vb = caml_ba_alloc(CAML_BA_##type | CAML_BA_C_LAYOUT              \
                         | CAML_BA_MANAGED,                             \
                         num_dims, data, dim);                          \
      /* Attach the proxy of the surface to the bigarray */             \
      ++ proxy->refcount;                                               \
      (Caml_ba_array_val(vb))->proxy = proxy;                           \
    }                                                                   \
    CAMLreturn(vb);                                                     \
  }

SURFACE_GET_DATA(UINT8, 1,
                 cairo_image_surface_get_stride(SURFACE_VAL(vsurf))
                 * cairo_image_surface_get_height(SURFACE_VAL(vsurf)) )
SURFACE_GET_DATA(INT32, 2,
                 cairo_image_surface_get_height(SURFACE_VAL(vsurf)),
                 cairo_image_surface_get_stride(SURFACE_VAL(vsurf)) / 4 )


#define GET_SURFACE(name, val_of, type)                         \
  CAMLexport value caml_##name(value vsurf)                     \
  {                                                             \
    CAMLparam1(vsurf);                                          \
    CAMLlocal1(vret);                                           \
    type ret = name(SURFACE_VAL(vsurf));                        \
    vret = val_of(ret);                                         \
    CAMLreturn(vret);                                           \
  }

GET_SURFACE(cairo_image_surface_get_format, VAL_FORMAT, cairo_format_t)
GET_SURFACE(cairo_image_surface_get_width, Val_int, int)
GET_SURFACE(cairo_image_surface_get_height, Val_int, int)
GET_SURFACE(cairo_image_surface_get_stride, Val_int, int)

#else

UNAVAILABLE3(cairo_image_surface_create)
UNAVAILABLE2(cairo_format_stride_for_width)
UNAVAILABLE5(cairo_image_surface_create_for_data8)
UNAVAILABLE5(cairo_image_surface_create_for_data32)
UNAVAILABLE1(cairo_image_surface_get_UINT8)
UNAVAILABLE1(cairo_image_surface_get_INT32)
UNAVAILABLE1(cairo_image_surface_get_format)
UNAVAILABLE1(cairo_image_surface_get_width)
UNAVAILABLE1(cairo_image_surface_get_height)
UNAVAILABLE1(cairo_image_surface_get_stride)

#endif /* CAIRO_HAS_IMAGE_SURFACE */

/* PDF surface
***********************************************************************/

static cairo_status_t caml_cairo_output_string
(void *fn, const unsigned char *data, unsigned int length)
{
  CAMLparam0();
  CAMLlocal2(s, r);

  s = caml_alloc_string(length);
  memmove((char *) String_val(s), data, length);
  r = caml_callback_exn(* ((value *) fn), s);
  if (Is_exception_result(r))
    CAMLreturn(CAIRO_STATUS_WRITE_ERROR);
  else
    CAMLreturn(CAIRO_STATUS_SUCCESS);
}

#define SURFACE_CREATE_FROM_STREAM(name)                                \
  CAMLexport value caml_##name(value voutput, value vwidth, value vheight) \
  {                                                                     \
    CAMLparam3(voutput, vwidth, vheight);                               \
    CAMLlocal1(vsurf);                                                  \
    cairo_surface_t* surf;                                              \
    value *output;                                                      \
                                                                        \
    output = malloc(sizeof(value));                                     \
    output[0] = voutput;                                                \
    surf = name(&caml_cairo_output_string, output,                      \
                Double_val(vwidth), Double_val(vheight));               \
    caml_cairo_raise_Error(cairo_surface_status(surf));                 \
    SET_SURFACE_CALLBACK(surf, output);                                 \
    SURFACE_ASSIGN(vsurf, surf);                                        \
    CAMLreturn(vsurf);                                                  \
  }

#define SURFACE_CREATE(name)                                            \
  CAMLexport value caml_##name(value vfname, value vwidth, value vheight) \
  {                                                                     \
    CAMLparam3(vfname, vwidth, vheight);                                \
    CAMLlocal1(vsurf);                                                  \
    cairo_surface_t* surf;                                              \
                                                                        \
    surf = name(String_val(vfname), Double_val(vwidth), Double_val(vheight)); \
    caml_cairo_raise_Error(cairo_surface_status(surf));                       \
    SURFACE_ASSIGN(vsurf, surf);                                        \
    CAMLreturn(vsurf);                                                  \
  }


#ifdef CAIRO_HAS_PDF_SURFACE

SURFACE_CREATE_FROM_STREAM(cairo_pdf_surface_create_for_stream)
SURFACE_CREATE(cairo_pdf_surface_create)
DO2_SURFACE(cairo_pdf_surface_set_size, Double_val, Double_val)

#else

UNAVAILABLE3(cairo_pdf_surface_create_for_stream)
UNAVAILABLE3(cairo_pdf_surface_create)
UNAVAILABLE3(cairo_pdf_surface_set_size)

#endif /* CAIRO_HAS_PDF_SURFACE */

/* PNG functions
***********************************************************************/

static cairo_status_t caml_cairo_input_string
(void *fn, unsigned char *data, unsigned int length)
{
  value s, r;
  /* Contrarily to what is customary, it is the caller which specifies
     the length of the data to read and we know no upper bound, so
     there is no way to preallocate a single OCaml string for all read
     operations. */
  s = caml_alloc_string(length);

  r = caml_callback2_exn(* ((value *) fn), s, Val_int(length));
  if (Is_exception_result(r)) return(CAIRO_STATUS_READ_ERROR);
  else {
    memmove(data, String_val(s), length);
    return(CAIRO_STATUS_SUCCESS);
  }
}

#ifdef CAIRO_HAS_PNG_FUNCTIONS

CAMLexport value caml_cairo_image_surface_create_from_png(value fname)
{
  CAMLparam1(fname);
  CAMLlocal1(vsurf);
  cairo_surface_t* surf;

  surf = cairo_image_surface_create_from_png(String_val(fname));
  caml_cairo_raise_Error(cairo_surface_status(surf));
  SURFACE_ASSIGN(vsurf, surf);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_image_surface_create_from_png_stream(value vinput)
{
  CAMLparam1(vinput);
  CAMLlocal1(vsurf);
  cairo_surface_t* surf;

  surf = cairo_image_surface_create_from_png_stream(&caml_cairo_input_string,
                                                    &vinput);
  if (surf == NULL) caml_cairo_raise_Error(CAIRO_STATUS_READ_ERROR);
  caml_cairo_raise_Error(cairo_surface_status(surf));
  SURFACE_ASSIGN(vsurf, surf);
  CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_surface_write_to_png(value vsurf, value vfname)
{
  /* noalloc */
  cairo_status_t status;

  status = cairo_surface_write_to_png(SURFACE_VAL(vsurf), String_val(vfname));
  caml_cairo_raise_Error(status);
  return(Val_unit);
}

CAMLexport value caml_cairo_surface_write_to_png_stream(value vsurf,
                                                        value voutput)
{
  CAMLparam2(vsurf, voutput);
  cairo_status_t status = cairo_surface_write_to_png_stream
    (SURFACE_VAL(vsurf), &caml_cairo_output_string, &voutput);
  caml_cairo_raise_Error(status);
  CAMLreturn(Val_unit);
}

#else

UNAVAILABLE1(cairo_image_surface_create_from_png)
UNAVAILABLE1(cairo_image_surface_create_from_png_stream)
UNAVAILABLE1(cairo_surface_write_to_png)
UNAVAILABLE2(cairo_surface_write_to_png_stream)

#endif /* CAIRO_HAS_PNG_FUNCTIONS */

/* Postscript surface
***********************************************************************/

#ifdef CAIRO_HAS_PS_SURFACE

SURFACE_CREATE(cairo_ps_surface_create)
SURFACE_CREATE_FROM_STREAM(cairo_ps_surface_create_for_stream)

#define PS_LEVEL_VAL(v) ((cairo_ps_level_t) Int_val(v))
#define VAL_PS_LEVEL(v) Val_int(v)

DO1_SURFACE(cairo_ps_surface_restrict_to_level, PS_LEVEL_VAL)

#define GET_LIST(name, val_of, type)                    \
  CAMLexport value caml_##name(value unit)              \
  {                                                     \
    CAMLparam1(unit);                                   \
    CAMLlocal2(vlist, vcons);                           \
    type *array;                                        \
    int num, i;                                         \
    /* Fill array */                                    \
    name(&array, &num);                                 \
    /* Create OCaml list */                             \
    vlist = Val_int(0); /* [] */                        \
    for(i = 0; i < num; i++) {                          \
      vcons = caml_alloc_tuple(2);                      \
      Store_field(vcons, 0, val_of(array[i]));          \
      Store_field(vcons, 1, vlist);                     \
      vlist = vcons; /* new head */                     \
    }                                                   \
    CAMLreturn(vlist);                                  \
  }

GET_LIST(cairo_ps_get_levels, VAL_PS_LEVEL, cairo_ps_level_t const)

CAMLexport value caml_cairo_ps_level_to_string(value vlevel)
{
  CAMLparam1(vlevel);
  const char* s = cairo_ps_level_to_string(PS_LEVEL_VAL(vlevel));
  CAMLreturn(caml_copy_string(s));
}

DO1_SURFACE(cairo_ps_surface_set_eps, Bool_val)

CAMLexport value caml_cairo_ps_surface_get_eps(value vsurf)
{
  /* noalloc */
  cairo_bool_t b = cairo_ps_surface_get_eps(SURFACE_VAL(vsurf));
  return(Val_bool(b));
}

DO2_SURFACE(cairo_ps_surface_set_size, Double_val, Double_val)

DO_SURFACE(cairo_ps_surface_dsc_begin_setup)
DO_SURFACE(cairo_ps_surface_dsc_begin_page_setup)
DO1_SURFACE(cairo_ps_surface_dsc_comment, String_val)

#else

UNAVAILABLE3(cairo_ps_surface_create)
UNAVAILABLE3(cairo_ps_surface_create_for_stream)
UNAVAILABLE2(cairo_ps_surface_restrict_to_level)
UNAVAILABLE1(cairo_ps_get_levels)
UNAVAILABLE1(cairo_ps_level_to_string)
UNAVAILABLE2(cairo_ps_surface_set_eps)
UNAVAILABLE1(cairo_ps_surface_get_eps)
UNAVAILABLE3(cairo_ps_surface_set_size)
UNAVAILABLE1(cairo_ps_surface_dsc_begin_setup)
UNAVAILABLE1(cairo_ps_surface_dsc_begin_page_setup)
UNAVAILABLE2(cairo_ps_surface_dsc_comment)

#endif /* CAIRO_HAS_PS_SURFACE */

/* SVG surface
***********************************************************************/

#ifdef CAIRO_HAS_SVG_SURFACE

SURFACE_CREATE(cairo_svg_surface_create)
SURFACE_CREATE_FROM_STREAM(cairo_svg_surface_create_for_stream)

#define SVG_VERSION_VAL(v) ((cairo_svg_version_t) Int_val(v))
#define VAL_SVG_VERSION(v) Val_int(v)

DO1_SURFACE(cairo_svg_surface_restrict_to_version, SVG_VERSION_VAL)
GET_LIST(cairo_svg_get_versions, VAL_SVG_VERSION, cairo_svg_version_t const)

CAMLexport value caml_cairo_svg_version_to_string(value vversion)
{
  CAMLparam1(vversion);
  const char* s = cairo_svg_version_to_string(SVG_VERSION_VAL(vversion));
  CAMLreturn(caml_copy_string(s));
}

#else

UNAVAILABLE3(cairo_svg_surface_create)
UNAVAILABLE3(cairo_svg_surface_create_for_stream)
UNAVAILABLE2(cairo_svg_surface_restrict_to_version)
UNAVAILABLE1(cairo_svg_get_versions)
UNAVAILABLE1(cairo_svg_version_to_string)

#endif /* CAIRO_HAS_SVG_SURFACE */

/* Recording surface
***********************************************************************/

#ifdef CAIRO_HAS_RECORDING_SURFACE

CAMLexport value caml_cairo_recording_surface_create(
  value vextents, value vcontent)
{
    CAMLparam2(vcontent, vextents);
    CAMLlocal2(vsurf, vrectangle);
    cairo_surface_t *surf;
    cairo_content_t content;
    cairo_rectangle_t *extents = NULL;

    SET_CONTENT_VAL(content, vcontent);

    /* Get extents rectangle, if given. */
    if (Is_block(vextents)) /* = Some _ */ {
      vrectangle = Field(vextents, 0);
      SET_MALLOC(extents, 1, cairo_rectangle_t);
      extents->x = Double_field(vrectangle, 0);
      extents->y = Double_field(vrectangle, 1);
      extents->width = Double_field(vrectangle, 2);
      extents->height = Double_field(vrectangle, 3);
    }

    surf = cairo_recording_surface_create(content, extents);

    if (extents != NULL) {
      free(extents);
    }

    caml_cairo_raise_Error(cairo_surface_status(surf));

    SURFACE_ASSIGN(vsurf, surf);
    CAMLreturn(vsurf);
}

CAMLexport value caml_cairo_recording_surface_ink_extents(value vsurf)
{
    CAMLparam1(vsurf);
    CAMLlocal1(vextents);
    double x, y, w, h;

    cairo_recording_surface_ink_extents(SURFACE_VAL(vsurf), &x, &y, &w, &h);

    vextents = caml_alloc(4 * Double_wosize, Double_array_tag);
    Store_double_field(vextents, 0, x);
    Store_double_field(vextents, 1, y);
    Store_double_field(vextents, 2, w);
    Store_double_field(vextents, 3, h);

    CAMLreturn(vextents);
}

#else

UNAVAILABLE2(cairo_recording_surface_create)
UNAVAILABLE1(cairo_recording_surface_ink_extents)

#endif /* CAIRO_HAS_RECORDING_SURFACE */



/* Local Variables: */
/* compile-command: "make -k -C.." */
/* End: */
