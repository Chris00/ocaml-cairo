/* File: cairo_ocaml.h                                           -*-c-*-

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#ifndef __CAIRO_OCAML_H__
#define __CAIRO_OCAML_H__

#include <cairo.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>

/* cairo_t
***********************************************************************/
#define CAIRO_VAL(v) (* (cairo_t **) Data_custom_val(v))
extern struct custom_operations caml_cairo_ops;

void caml_cairo_raise_Error(cairo_status_t status);
/* raise [Cairo.Error] if the status indicates a failure. */

/* cairo_pattern_t
***********************************************************************/
#define PATTERN_VAL(v) (* (cairo_pattern_t **) Data_custom_val(v))
extern struct custom_operations caml_pattern_ops;

#define EXTEND_VAL(v) ((cairo_extend_t) Int_val(v))
#define VAL_EXTEND(v) Val_int(v)

#define FILTER_VAL(v) ((cairo_filter_t) Int_val(v))
#define VAL_FILTER(v) Val_int(v)

/* cairo_font_options_t
***********************************************************************/

#define FONT_OPTIONS_VAL(v) (* (cairo_font_options_t**) Data_custom_val(v))
extern struct custom_operations caml_font_options_ops;

/* cairo_font_type_t
***********************************************************************/

extern value caml_cairo_font_type[5];

cairo_font_type_t caml_cairo_font_type_val(value vft);

#define FONT_TYPE_VAL(vft) caml_cairo_font_type_val(vft)
#define VAL_FONT_TYPE(v) caml_cairo_font_type[v]

/* cairo_scaled_font_t
***********************************************************************/

#define SCALED_FONT_VAL(v) (* (cairo_scaled_font_t**) Data_custom_val(v))
extern struct custom_operations caml_scaled_font_ops;

/* cairo_surface_t
***********************************************************************/

#define SURFACE_VAL(v) (* (cairo_surface_t **) Data_custom_val(v))
extern struct custom_operations caml_surface_ops;

/* Type cairo_content_t */

#define SET_CONTENT_VAL(c, vcontent)                                    \
  switch (Int_val(vcontent))                                            \
    {                                                                   \
    case 0 : c = CAIRO_CONTENT_COLOR;  break;                           \
    case 1 : c = CAIRO_CONTENT_ALPHA;  break;                           \
    case 2 : c = CAIRO_CONTENT_COLOR_ALPHA;  break;                     \
    default : caml_failwith(__FILE__ ": Decode Cairo.content");         \
    }

#define CONTENT_ASSIGN(vcontent, content)                               \
  switch (content)                                                      \
    {                                                                   \
    case CAIRO_CONTENT_COLOR: vcontent = Val_int(0); break;             \
    case CAIRO_CONTENT_ALPHA: vcontent = Val_int(1); break;             \
    case CAIRO_CONTENT_COLOR_ALPHA: vcontent = Val_int(2); break;       \
    default : caml_failwith(__FILE__ ": Assign Cairo.content");         \
    }

/* cairo_path_t
***********************************************************************/

#define PATH_VAL(v) (* (cairo_path_t **) Data_custom_val(v))
extern struct custom_operations caml_path_ops;

#define PATH_DATA_ASSIGN(vdata, data)                                   \
  switch (data->header.type) {                                          \
    /* keep in sync the tags with the OCaml def of path_data */         \
  case CAIRO_PATH_MOVE_TO:                                              \
    vdata = caml_alloc(2, 0);                                           \
    Store_field(vdata, 0, caml_copy_double(data[1].point.x));           \
    Store_field(vdata, 1, caml_copy_double(data[1].point.y));           \
    break;                                                              \
  case CAIRO_PATH_LINE_TO:                                              \
    vdata = caml_alloc(2, 1);                                           \
    Store_field(vdata, 0, caml_copy_double(data[1].point.x));           \
    Store_field(vdata, 1, caml_copy_double(data[1].point.y));           \
    break;                                                              \
  case CAIRO_PATH_CURVE_TO:                                             \
    vdata = caml_alloc(6, 2);                                           \
    Store_field(vdata, 0, caml_copy_double(data[1].point.x));           \
    Store_field(vdata, 1, caml_copy_double(data[1].point.y));           \
    Store_field(vdata, 2, caml_copy_double(data[2].point.x));           \
    Store_field(vdata, 3, caml_copy_double(data[2].point.y));           \
    Store_field(vdata, 4, caml_copy_double(data[3].point.x));           \
    Store_field(vdata, 5, caml_copy_double(data[3].point.y));           \
    break;                                                              \
  case CAIRO_PATH_CLOSE_PATH:                                           \
    vdata = Val_int(0); /* first constant constructor */                \
    break;                                                              \
  }

#define SWITCH_PATH_DATA(v, move, line, curve, close)   \
  if(Is_long(v)) {                                      \
    close;                                              \
  } else switch(Tag_val(v)) {                           \
    case 0:                                             \
      move(Field(v,0), Field(v,1));                     \
      break;                                            \
    case 1:                                             \
      line(Field(v,0), Field(v,1));                     \
      break;                                            \
    case 2:                                             \
      curve(Field(v,0), Field(v,1),                     \
            Field(v,2), Field(v,3),                     \
            Field(v,4), Field(v,5));                    \
      break;                                            \
    default:                                            \
      caml_failwith(__FILE__ ": SWITCH_PATH_DATA");     \
    }


/* FreeType
***********************************************************************/

/* #define OCAML_CAIRO_HAS_FT 1 */

#ifdef OCAML_CAIRO_HAS_FT
#undef alloc
#include <cairo-ft.h>

#define FT_LIBRARY_VAL(v) (* (FT_Library*) Data_custom_val(v))
extern struct custom_operations caml_cairo_ft_library_ops;

#define FT_FACE_VAL(v) (* (FT_Face*) Data_custom_val(v))
extern struct custom_operations caml_cairo_ft_face_ops;

#endif /* OCAML_CAIRO_HAS_FT */

#endif /* __CAIRO_OCAML_H__ */
