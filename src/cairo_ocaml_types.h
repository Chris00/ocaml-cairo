/* Generic functions for types */

#include "cairo_ocaml.h"

static int caml_cairo_compare_pointers(value v1, value v2)
{
  void *p1 = * (void **) Data_custom_val(v1);
  void *p2 = * (void **) Data_custom_val(v2);
  if (p1 == p2) return(0);
  else if (p1 < p2) return(-1);
  else return(1);
}

static intnat caml_cairo_hash_pointer(value v)
{
  return((intnat) (* (void **) Data_custom_val(v)));
}

#define CUSTOM_OPERATIONS(name)                                         \
  struct custom_operations caml_##name##_ops = {                        \
    #name "_t", /* identifier for serialization and deserialization */  \
    &caml_cairo_##name##_finalize,                                      \
    &caml_cairo_compare_pointers,                                       \
    &caml_cairo_hash_pointer,                                           \
    custom_serialize_default,                                           \
    custom_deserialize_default };

#define DEFINE_CUSTOM_OPERATIONS(name, destroy, val)                    \
  static void caml_cairo_##name##_finalize(value v)                     \
  {                                                                     \
    /* fprintf(stderr, "DESTROY %s\n", #name);  fflush(stderr); */      \
    destroy(val(v));                                                    \
  }                                                                     \
  CUSTOM_OPERATIONS(name)

#define ALLOC(name) caml_alloc_custom(&caml_##name##_ops, sizeof(void*), 1, 50)

/* Type cairo_t
***********************************************************************/

#define CAIRO_ASSIGN(v, x) v = ALLOC(cairo); CAIRO_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(cairo, cairo_destroy, CAIRO_VAL)

/* raise [Error] if the status indicates a failure. */
void caml_cairo_raise_Error(cairo_status_t status)
{
  static const value * exn = NULL;

  if (status != CAIRO_STATUS_SUCCESS) {
    if (exn == NULL) {
      /* First time around, look up by name */
      exn = caml_named_value("Cairo.Error");
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

/* Raise the corresponding OCaml exception for errors. */
static void caml_check_status(cairo_t *cr)
{
  caml_cairo_raise_Error(cairo_status(cr));
}


CAMLexport value caml_cairo_status_to_string(value vstatus)
{
  CAMLparam1(vstatus);
  cairo_status_t status = (cairo_status_t) (Int_val(vstatus) + 2);
  const char* msg = cairo_status_to_string(status);
  CAMLreturn(caml_copy_string(msg));
}

/* Type cairo_pattern_t
***********************************************************************/

#define PATTERN_ASSIGN(v, x) v = ALLOC(pattern); PATTERN_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(pattern, cairo_pattern_destroy, PATTERN_VAL)

/* Type cairo_surface_t
***********************************************************************/

#define SURFACE_ASSIGN(v, x) v = ALLOC(surface); SURFACE_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(surface, cairo_surface_destroy, SURFACE_VAL)

/* Some surfaces have a callback attached.  We must store its value at
   a location that exists for the lifetime of the surface so one can
   pass a pointer to it to the *_for_stream functions and the
   finalizer may unregister it.  Wrapping cairo_surface_t is not
   possible because such values are returned by C functions.  Thus
   store it in the surface (thanks Cairo!). */
static const cairo_user_data_key_t surface_callback;

static void caml_destroy_surface_callback(void *data)
{
  /* fprintf(stderr, "DESTROY surface callback\n");  fflush(stderr); */
  caml_remove_generational_global_root((value *)data);
  free(data);
}

/* [output] is a pointer on the callback [value], already allocated
   (and assigned). */
#define SET_SURFACE_CALLBACK(surf, output)                              \
  caml_register_generational_global_root(output);                       \
  caml_cairo_raise_Error(                                               \
    cairo_surface_set_user_data (surf, &surface_callback, output,       \
                                 &caml_destroy_surface_callback))


static value caml_cairo_surface_kind[15];

CAMLexport value caml_cairo_surface_kind_init(value unit)
{
  /* noalloc */
  caml_cairo_surface_kind[0] = caml_hash_variant("Image");
  caml_cairo_surface_kind[1] = caml_hash_variant("PDF");
  caml_cairo_surface_kind[2] = caml_hash_variant("PS");
  caml_cairo_surface_kind[3] = caml_hash_variant("XLib");
  caml_cairo_surface_kind[4] = caml_hash_variant("XCB");
  caml_cairo_surface_kind[5] = caml_hash_variant("GLITZ");
  caml_cairo_surface_kind[6] = caml_hash_variant("Quartz");
  caml_cairo_surface_kind[7] = caml_hash_variant("Win32");
  caml_cairo_surface_kind[8] = caml_hash_variant("BEOS");
  caml_cairo_surface_kind[9] = caml_hash_variant("DirectFB");
  caml_cairo_surface_kind[10] = caml_hash_variant("SVG");
  caml_cairo_surface_kind[11] = caml_hash_variant("OS2");
  caml_cairo_surface_kind[12] = caml_hash_variant("Win32_printing");
  caml_cairo_surface_kind[13] = caml_hash_variant("Quartz_image");
  caml_cairo_surface_kind[14] = caml_hash_variant("Recording");
  return(Val_unit);
}

#define VAL_SURFACE_KIND(k) caml_cairo_surface_kind[k]


/* Type cairo_path_t
***********************************************************************/

#define PATH_ASSIGN(v, x) v = ALLOC(path); PATH_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(path, cairo_path_destroy, PATH_VAL)


/* Type cairo_glyph_t
***********************************************************************/

#define SET_GLYPH_VAL(p, v)                      \
  p->index = Int_val(Field(v,0));                \
  p->x = Double_val(Field(v,1));                 \
  p->y = Double_val(Field(v,2))

#define ARRAY_GLYPH_VAL(glyphs, p, vglyphs, num_glyphs)         \
  num_glyphs = Wosize_val(vglyphs);                             \
  SET_MALLOC(glyphs, num_glyphs, cairo_glyph_t);                \
  for(i=0, p = glyphs; i < num_glyphs; i++, p++) {              \
    SET_GLYPH_VAL(p, Field(vglyphs, i));                        \
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
  SET_MALLOC(clusters, num_clusters, cairo_text_cluster_t);             \
  for(i=0, q = clusters; i < num_clusters; i++, q++) {                  \
    SET_CLUSTER_VAL(q, Field(vclusters, i));                            \
  }


#define CLUSTER_ASSIGN(v, cluster)                      \
  v = caml_alloc_tuple(2);                              \
  Store_field(v, 0, Val_int(cluster.num_bytes));        \
  Store_field(v, 1, Val_int(cluster.num_glyphs))

#define CLUSTER_FLAGS_VAL(v) ((cairo_text_cluster_flags_t) Int_val(v))
#define VAL_CLUSTER_FLAGS(v) Val_int(v)

/* Type cairo_matrix_t
***********************************************************************/

#ifdef ARCH_ALIGN_DOUBLE
#define SET_CAIRO_MATRIX_(v)                             \
  matrix_##v.xx = Double_field(v, 0);                    \
  matrix_##v.yx = Double_field(v, 1);                    \
  matrix_##v.xy = Double_field(v, 2);                    \
  matrix_##v.yy = Double_field(v, 3);                    \
  matrix_##v.x0 = Double_field(v, 4);                    \
  matrix_##v.y0 = Double_field(v, 5);

#define ALLOC_CAIRO_MATRIX(v)                            \
  cairo_matrix_t matrix_##v;                             \
  SET_CAIRO_MATRIX_(v)

#define ALLOC_CAIRO_MATRIX2(v1, v2)                      \
  cairo_matrix_t matrix_##v1, matrix_##v2;               \
  SET_CAIRO_MATRIX_(v1);                                 \
  SET_CAIRO_MATRIX_(v2)

/* `f' may use `GET_MATRIX(v)' */
#define WITH_MATRIX_DO(v, f)                                            \
  cairo_matrix_t matrix_##v;                                            \
  f;                                                                    \
  v = caml_alloc(6 * sizeof(double) / sizeof(void *), Double_array_tag); \
  Store_double_field(v, 0, matrix_##v.xx);                              \
  Store_double_field(v, 1, matrix_##v.yx);                              \
  Store_double_field(v, 2, matrix_##v.xy);                              \
  Store_double_field(v, 3, matrix_##v.yy);                              \
  Store_double_field(v, 4, matrix_##v.x0);                              \
  Store_double_field(v, 5, matrix_##v.y0)

#define GET_MATRIX(v) &matrix_##v

#else /* not def ARCH_ALIGN_DOUBLE */

#define ALLOC_CAIRO_MATRIX(v)       /* nothing to do */
#define ALLOC_CAIRO_MATRIX2(v1, v2) /* nothing to do */

#define WITH_MATRIX_DO(v, f)                                            \
  v = caml_alloc(6 * sizeof(double) / sizeof(void *), Double_array_tag); \
  f /* `f' may use `GET_MATRIX(v)' */

/* Optimize by using a pointer to OCaml data */
#define GET_MATRIX(v) (cairo_matrix_t *)(v)

#endif

/* Text
***********************************************************************/

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

static intnat caml_cairo_font_options_hash(value v)
{
  return(cairo_font_options_hash(FONT_OPTIONS_VAL(v)));
}

struct custom_operations caml_font_options_ops = {
  "font_options_t", /* identifier for serialization and deserialization */
  &caml_cairo_font_options_finalize,
  &caml_cairo_font_options_compare,
  &caml_cairo_font_options_hash,
  custom_serialize_default,
  custom_deserialize_default };

value caml_cairo_font_type[5];

CAMLexport value caml_cairo_font_type_init(value unit)
{
  /* noalloc */
  caml_cairo_font_type[CAIRO_FONT_TYPE_TOY] = caml_hash_variant("Toy");
  caml_cairo_font_type[CAIRO_FONT_TYPE_FT] = caml_hash_variant("Ft");
  caml_cairo_font_type[CAIRO_FONT_TYPE_WIN32] = caml_hash_variant("Win32");
  caml_cairo_font_type[CAIRO_FONT_TYPE_QUARTZ] = caml_hash_variant("Quartz");
  caml_cairo_font_type[CAIRO_FONT_TYPE_USER] = caml_hash_variant("User");
  return(Val_unit);
}

cairo_font_type_t caml_cairo_font_type_val(value vft)
{
  if (vft == caml_cairo_font_type[0]) return(CAIRO_FONT_TYPE_TOY);
  else if (vft == caml_cairo_font_type[1]) return(CAIRO_FONT_TYPE_FT);
  else if (vft == caml_cairo_font_type[2]) return(CAIRO_FONT_TYPE_WIN32);
  else if (vft == caml_cairo_font_type[3]) return(CAIRO_FONT_TYPE_QUARTZ);
  else if (vft == caml_cairo_font_type[4]) return(CAIRO_FONT_TYPE_USER);
  caml_failwith("Cairo.font_type conversion failed. Contact the developers.");
}


#define FONT_FACE_VAL(v) (* (cairo_font_face_t**) Data_custom_val(v))

#define FONT_FACE_ASSIGN(v, x) v = ALLOC(font_face);  FONT_FACE_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(font_face, cairo_font_face_destroy, FONT_FACE_VAL)

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

#define SLANT_VAL(v) ((cairo_font_slant_t) Int_val(v))
#define VAL_SLANT(v) Val_int(v)
#define WEIGHT_VAL(v) ((cairo_font_weight_t) Int_val(v))
#define VAL_WEIGHT(v) Val_int(v)

/* FreeType
***********************************************************************/

#ifdef OCAML_CAIRO_HAS_FT
#include <cairo-ft.h>
#include <ftmodapi.h>

#define FT_LIBRARY_ASSIGN(v, x) \
  v = ALLOC(cairo_ft_library);  FT_LIBRARY_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(cairo_ft_library, FT_Done_Library, FT_LIBRARY_VAL)


#define FT_FACE_ASSIGN(v, x) v = ALLOC(cairo_ft_face);  FT_FACE_VAL(v) = x

DEFINE_CUSTOM_OPERATIONS(cairo_ft_face, FT_Done_Face, FT_FACE_VAL)

#endif


/* Local Variables: */
/* compile-command: "make -k -C.." */
/* End: */
