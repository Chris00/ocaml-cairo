#define SET_MALLOC(x, size, type)                \
  x = (type *) malloc(size * sizeof(type));      \
  if (x == NULL) caml_raise_out_of_memory()

#define FLOAT_ARRAY_LENGTH(a) Wosize_val(a) / Double_wosize

#define SET_FLOAT_ARRAY(p, varray, length)                      \
  SET_MALLOC(p, length, double);                                \
  for(i = 0; i < length; i++)  p[i] = Double_field(varray, i)

#define FREE_FLOAT_ARRAY(p) free(p)


#define DO_CONTEXT(name)                       \
  CAMLexport value caml_##name(value vcr)       \
  {                                             \
    CAMLparam1(vcr);                            \
    cairo_t *cr = CAIRO_VAL(vcr);               \
    name(cr);                                   \
    caml_check_status(cr);                      \
    CAMLreturn(Val_unit);                       \
  }

#define DO1_CONTEXT(name, of_value)                 \
  CAMLexport value caml_##name(value vcr, value v)   \
  {                                                  \
    CAMLparam2(vcr, v);                              \
    cairo_t* cr = CAIRO_VAL(vcr);                    \
    name(cr, of_value(v));                           \
    caml_check_status(cr);                           \
    CAMLreturn(Val_unit);                            \
  }

#define DO2_CONTEXT(name, of_val1, of_val2)                            \
  CAMLexport value caml_##name(value vcr, value v1, value v2)           \
  {                                                                     \
    CAMLparam3(vcr, v1, v2);                                            \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2));                                 \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }

#define DO3_CONTEXT(name, of_val1, of_val2, of_val3)                   \
  CAMLexport value caml_##name(value vcr, value v1, value v2, value v3) \
  {                                                                     \
    CAMLparam4(vcr, v1, v2, v3);                                        \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2), of_val3(v3));                    \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }

#define DO4_CONTEXT(name, of_val1, of_val2, of_val3, of_val4)          \
  CAMLexport value caml_##name(value vcr, value v1, value v2, value v3, \
                               value v4)                                \
  {                                                                     \
    CAMLparam5(vcr, v1, v2, v3, v4);                                    \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2), of_val3(v3), of_val4(v4));       \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }

#define DO5_CONTEXT(name, of_val1, of_val2, of_val3, of_val4, of_val5) \
  CAMLexport value caml_##name(value vcr, value v1, value v2, value v3, \
                               value v4, value v5)                      \
  {                                                                     \
    CAMLparam5(vcr, v1, v2, v3, v4);                                    \
    CAMLxparam1(v5);                                                    \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2), of_val3(v3), of_val4(v4),        \
         of_val5(v5));                                                  \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }                                                                     \
                                                                        \
  CAMLexport value caml_##name##_bc(value * argv, int argn)             \
  {                                                                     \
    return caml_##name(argv[0], argv[1], argv[2], argv[3], argv[4],     \
                       argv[5]);                                        \
  }

#define DO6_CONTEXT(name, of_val1, of_val2, of_val3, of_val4, of_val5, \
                     of_val6)                                           \
  CAMLexport value caml_##name(value vcr, value v1, value v2, value v3, \
                               value v4, value v5, value v6)            \
  {                                                                     \
    CAMLparam5(vcr, v1, v2, v3, v4);                                    \
    CAMLxparam2(v5, v6);                                                \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_val1(v1), of_val2(v2), of_val3(v3), of_val4(v4),        \
         of_val5(v5), of_val6(v6));                                     \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }                                                                     \
                                                                        \
  CAMLexport value caml_##name##_bc(value * argv, int argn)             \
  {                                                                     \
    return caml_##name(argv[0], argv[1], argv[2], argv[3], argv[4],     \
                       argv[5], argv[6]);                               \
  }


/* The return value should not require special alloc. */
#define GET_CONTEXT(name, value_of, ty)                        \
  CAMLexport value caml_##name(value vcr)                       \
  {                                                             \
    CAMLparam1(vcr);                                            \
    cairo_t* cr = CAIRO_VAL(vcr);                               \
    ty r = name(cr);                                            \
    caml_check_status(cr);                                      \
    CAMLreturn(value_of(r));                                    \
  }

/* As recommended in the section "Multiple return values" of the cairo
 * documentation, map the "extents" to the "rectangle" representation. */
#define GET_EXTENTS(name)                                       \
  CAMLexport value caml_##name(value vcr)                       \
  {                                                             \
    CAMLparam1(vcr);                                            \
    CAMLlocal1(bb);                                             \
    cairo_t* cr = CAIRO_VAL(vcr);                               \
    double x1, y1, x2, y2;                                      \
    name(cr, &x1, &y1, &x2, &y2);                               \
    caml_check_status(cr);                                      \
    /* Create record (of only floats) */                        \
    bb = caml_alloc(4 * Double_wosize, Double_array_tag);       \
    Store_double_field(bb, 0, x1);                              \
    Store_double_field(bb, 1, y1);                              \
    Store_double_field(bb, 2, x2 - x1);                         \
    Store_double_field(bb, 3, y2 - y1);                         \
    CAMLreturn(bb);                                             \
  }


/* Surface
***********************************************************************/

#define DO_SURFACE(name)                                       \
  CAMLexport value caml_##name(value vsurf)                    \
  {                                                            \
    /* noalloc */                                              \
    cairo_surface_t *surface = SURFACE_VAL(vsurf);             \
    name(surface);                                             \
    caml_cairo_raise_Error(cairo_surface_status(surface));     \
    return(Val_unit);                                          \
  }

#define DO1_SURFACE(name, of_val1)                              \
  CAMLexport value caml_##name(value vsurf, value v1)           \
  {                                                             \
    /* noalloc */                                               \
    cairo_surface_t *surface = SURFACE_VAL(vsurf);              \
    name(surface, of_val1(v1));                                 \
    caml_cairo_raise_Error(cairo_surface_status(surface));      \
    return(Val_unit);                                           \
  }

#define DO2_SURFACE(name, of_val1, of_val2)                     \
  CAMLexport value caml_##name(value vsurf, value v1, value v2) \
  {                                                             \
    /* noalloc */                                               \
    cairo_surface_t *surface = SURFACE_VAL(vsurf);              \
    name(surface, of_val1(v1), of_val2(v2));                    \
    caml_cairo_raise_Error(cairo_surface_status(surface));            \
    return(Val_unit);                                           \
  }


/* Unavailable Cairo backend functions
***********************************************************************/

/* holds the pointer to the Unavailable exception; shared several
   functions. */
const value * caml_cairo_Unavailable;

#ifdef _MSC_VER
#define RAISE_UNAVAILABLE(name, ...)                                    \
  CAMLexport value caml_##name(__VA_ARGS__)                             \
  {                                                                     \
    if (caml_cairo_Unavailable == NULL)                                 \
      caml_cairo_Unavailable = caml_named_value("Cairo.Unavailable");   \
    caml_raise_constant(* caml_cairo_Unavailable);                      \
  }                                                                     \

#else

#define RAISE_UNAVAILABLE(name, args ...)                               \
  CAMLexport value caml_##name(args)                                    \
  {                                                                     \
    if (caml_cairo_Unavailable == NULL)                                 \
      caml_cairo_Unavailable = caml_named_value("Cairo.Unavailable");   \
    caml_raise_constant(* caml_cairo_Unavailable);                      \
  }                                                                     \

#endif

#define UNAVAILABLE1(name) RAISE_UNAVAILABLE(name, value v1)
#define UNAVAILABLE2(name) RAISE_UNAVAILABLE(name, value v1, value v2)
#define UNAVAILABLE3(name) RAISE_UNAVAILABLE(name, value v1, value v2, value v3)
#define UNAVAILABLE4(name) RAISE_UNAVAILABLE(name, value v1, value v2,  \
                                             value v3, value v4)
#define UNAVAILABLE5(name) RAISE_UNAVAILABLE(name, value v1, value v2,  \
                                             value v3, value v4, value v5)
