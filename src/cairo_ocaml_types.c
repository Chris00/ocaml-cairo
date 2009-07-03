/* Generic functions for types */

static int caml_cairo_compare_pointers(value v1, value v2)
{
  void *p1 = * (void **) Data_custom_val(v1);
  void *p2 = * (void **) Data_custom_val(v2);
  if (p1 == p2) return(0);
  else if (p1 < p2) return(-1);
  else return(1);
}

static long caml_cairo_hash_pointer(value v)
{
  return((long) (* (void **) Data_custom_val(v)));
}

#define DEFINE_CUSTOM_OPERATIONS(name, destroy, val)                    \
  static void caml_cairo_##name##_finalize(value v)                     \
  {                                                                     \
    /* [cairo_*_reference] not used, the first [destroy] frees it. */   \
    destroy(val(v));                                                    \
  }                                                                     \
                                                                        \
  static struct custom_operations caml_##name##_ops = {                 \
    #name "_t", /* identifier for serialization and deserialization */  \
    &caml_cairo_##name##_finalize,                                      \
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

/* raise [Error] if the status indicates a failure. */
static void caml_raise_Error(cairo_status_t status)
{
  static value * exn = NULL;
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

/* For non Raise the corresponding OCaml exception. */
static void caml_check_status(cairo_t *cr)
{
  caml_raise_Error(cairo_status(cr));
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

#define EXTEND_VAL(v) Int_val(v)
#define VAL_EXTEND(v) Val_int(v)

#define FILTER_VAL(v) Int_val(v)
#define VAL_FILTER(v) Val_int(v)

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

/* Type cairo_path_t
***********************************************************************/

#define PATH_VAL(v) (* (cairo_path_t **) Data_custom_val(v))
#define PATH_ASSIGN(v, x) v = ALLOC(path); PATH_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(path, cairo_path_destroy, PATH_VAL)

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
      caml_failwith("C bindings: SWITCH_PATH_DATA");    \
    }




/* Type cairo_glyph_t
***********************************************************************/

#define SET_GLYPH_VAL(p, v)                      \
  p->index = Int_val(Field(v,0));                \
  p->x = Double_val(Field(v,1));                 \
  p->y = Double_val(Field(v,2))

#define ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs) \
  num_glyphs = Wosize_val(vglyphs);                     \
  glyphs = malloc(num_glyphs * sizeof(cairo_glyph_t));  \
  for(i=0, p = glyphs; i < num_glyphs; i++, p++) {      \
    SET_GLYPH_VAL(p, Field(vglyphs, i));                \
  }

#define GLYPH_ASSIGN(v, glyph)                          \
  v = caml_alloc_tuple(3);                              \
  Store_field(v, 0, Val_int(glyph.index));              \
  Store_field(v, 1, caml_copy_double(glyph.x));         \
  Store_field(v, 2, caml_copy_double(glyph.y))

#define SET_CLUSTER_VAL(p, v) \
  p->num_bytes = Int_val(Field(v, 0));          \
  p->num_glyphs = Int_val(Field(v, 1))

#define ARRAY_CLUSTER_VAL(clusters, q, vglyphs, num_glyphs)             \
  num_clusters = Wosize_val(vclusters);                                 \
  clusters = malloc(num_clusters * sizeof(cairo_text_cluster_t));       \
  for(i=0, q = clusters; i < num_clusters; i++, q++) {                  \
    SET_CLUSTER_VAL(q, Field(vclusters, i));                            \
  }


#define CLUSTER_ASSIGN(v, cluster)                      \
  v = caml_alloc_tuple(2);                              \
  Store_field(v, 0, Val_int(cluster.num_bytes));        \
  Store_field(v, 1, Val_int(cluster.num_glyphs))

#define CLUSTER_FLAGS_VAL(v) Int_val(v)
#define VAL_CLUSTER_FLAGS(v) Val_int(v)

/* Type cairo_matrix_t
***********************************************************************/

/* FIXME: optimize when possible */
#define SET_MATRIX_VAL(m, v)                    \
  m.xx = Double_field(v, 0);                    \
  m.yx = Double_field(v, 1);                    \
  m.xy = Double_field(v, 2);                    \
  m.yy = Double_field(v, 3);                    \
  m.x0 = Double_field(v, 4);                    \
  m.y0 = Double_field(v, 5)

#define MATRIX_ASSIGN(v, m)                     \
  v = caml_alloc(6, Double_array_tag);          \
  Store_double_field(v, 0, m.xx);               \
  Store_double_field(v, 1, m.yx);               \
  Store_double_field(v, 2, m.xy);               \
  Store_double_field(v, 3, m.yy);               \
  Store_double_field(v, 4, m.x0);               \
  Store_double_field(v, 5, m.y0)


/* Text
***********************************************************************/

#define FONT_OPTIONS_VAL(v) (* (cairo_font_options_t**) Data_custom_val(v))
#define FONT_OPTIONS_ASSIGN(vfo, fo) \
  vfo = ALLOC(font_options);         \
  FONT_OPTIONS_VAL(vfo) = fo

static void caml_cairo_font_options_finalize(value v)
{
  cairo_font_options_destroy(FONT_OPTIONS_VAL(v));
}

static int caml_cairo_font_options_compare(value v1, value v2)
{
  cairo_font_options_t *fo1 = FONT_OPTIONS_VAL(v1);
  cairo_font_options_t *fo2 = FONT_OPTIONS_VAL(v2);
  /* fo1 == fo2 => cairo_font_options_equal(fo1, fo2) ; thus this
     remains a total order. */
  if (cairo_font_options_equal(fo1, fo2)) return(0);
  else if (fo1 < fo2) return(-1);
  else return(1);
}

static long caml_cairo_font_options_hash(value v)
{
  return(cairo_font_options_hash(FONT_OPTIONS_VAL(v)));
}

static struct custom_operations caml_font_options_ops = {
  "font_options_t", /* identifier for serialization and deserialization */
  &caml_cairo_font_options_finalize,
  &caml_cairo_font_options_compare,
  &caml_cairo_font_options_hash,
  custom_serialize_default,
  custom_deserialize_default };


static value caml_cairo_font_type[5];

CAMLexport value caml_cairo_font_type_init(value unit)
{
  /* noalloc */
  caml_cairo_font_type[0] = caml_hash_variant("Toy");
  caml_cairo_font_type[1] = caml_hash_variant("Ft");
  caml_cairo_font_type[2] = caml_hash_variant("Win32");
  caml_cairo_font_type[3] = caml_hash_variant("Quartz");
  caml_cairo_font_type[4] = caml_hash_variant("User");
  return(Val_unit);
}

#define VAL_FONT_TYPE(v) caml_cairo_font_type[v]


#define FONT_FACE_VAL(v) (* (cairo_font_face_t**) Data_custom_val(v))

DEFINE_CUSTOM_OPERATIONS(font_face, cairo_font_face_destroy, FONT_FACE_VAL)


#define SCALED_FONT_VAL(v) (* (cairo_scaled_font_t**) Data_custom_val(v))

DEFINE_CUSTOM_OPERATIONS(scaled_font,
                         cairo_scaled_font_destroy, SCALED_FONT_VAL)

#define FONT_EXTENTS_ASSIGN(vfe, fe)                           \
  vfe = caml_alloc(5 * Double_wosize, Double_array_tag);        \
  Store_double_field(vfe, 0, fe.ascent);                        \
  Store_double_field(vfe, 1, fe.descent);                       \
  Store_double_field(vfe, 2, fe.height);                        \
  Store_double_field(vfe, 3, fe.max_x_advance);                 \
  Store_double_field(vfe, 4, fe.max_y_advance)

#define TEXT_EXTENTS_ASSIGN(vte, te)                            \
  vte = caml_alloc(6 * Double_wosize, Double_array_tag);        \
  Store_double_field(vte, 0, te.x_bearing);                     \
  Store_double_field(vte, 1, te.y_bearing);                     \
  Store_double_field(vte, 2, te.width);                         \
  Store_double_field(vte, 3, te.height);                        \
  Store_double_field(vte, 4, te.x_advance);                     \
  Store_double_field(vte, 5, te.y_advance)

#define SLANT_VAL(v) Int_val(v)
#define VAL_SLANT(v) Val_int(v)
#define WEIGHT_VAL(v) Int_val(v)
#define VAL_WEIGHT(v) Val_int(v)


/* Local Variables: */
/* compile-command: "make -k cairo_stubs.o" */
/* End: */
