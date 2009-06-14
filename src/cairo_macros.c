#define DO_FUNCTION(name)                       \
  CAMLexport value caml_##name(value vcr)       \
  {                                             \
    CAMLparam1(vcr);                            \
    cairo_t *cr = CAIRO_VAL(vcr);               \
    name(cr);                                   \
    caml_check_status(cr);                      \
    CAMLreturn(Val_unit);                       \
  }


#define SET_FUNCTION(name, of_value)                                    \
  CAMLexport value caml_##name(value vcr, value v)                      \
  {                                                                     \
    CAMLparam2(vcr, v);                                                 \
    cairo_t* cr = CAIRO_VAL(vcr);                                       \
    name(cr, of_value(v));                                              \
    caml_check_status(cr);                                              \
    CAMLreturn(Val_unit);                                               \
  }

/* The return value should not require special alloc. */
#define GET_FUNCTION(name, value_of, ty)                        \
  CAMLexport value caml_##name(value vcr)                       \
  {                                                             \
    CAMLparam1(vcr);                                            \
    cairo_t* cr = CAIRO_VAL(vcr);                               \
    ty r = name(cr);                                            \
    caml_check_status(cr);                                      \
    CAMLreturn(value_of(r));                                    \
  }

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
    Store_double_field(bb, 2, x2);                              \
    Store_double_field(bb, 3, y2);                              \
    CAMLreturn(bb);                                             \
  }
