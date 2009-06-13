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
