0.6.5 2024-11-08
----------------

- Do not link `freetype` twice.

0.6.4 2022-09-28
----------------

- Be compatible with OCaml 5.0 (@kit-ty-kate).

0.6.3 2022-05-20
----------------

- Fix bug in `Path.fold`.
- Extend `Image.get_data*` to bigarrays with externally managed payload.

0.6.2 2020-11-23
----------------

- Fix a memory leak (#19).
- Fix GCC warnings, in particular the “multiple definition of …” (#23).
- Clarify the license.
- Use `dune-configurator`.

0.6.1 2019-03-20
----------------

- Fix `create_for_data32` handling of dimensions.
- Documentation improvements.
- Fix dependencies for `cairo2-gtk` and `cairo2-pango`.

0.6 2018-09-05
--------------

- New `Ft` module to support FreeType fonts.  This is enabled if the
  package `conf-freetype` is installed.  On the C side, the exported
  header file `cairo_ocaml.h` defines the macro `OCAML_CAIRO_HAS_FT`
  when the Cairo bindings were compiled with TrueType support.
- New package `Cairo2-pango` providing the module `Cairo_pango`.
- Remove labels that were not bringing a clear benefit.  With Dune
  default behavior, users will feel compelled to write labels which
  was cluttering the code with the previous interface.  With Merlin,
  it is now possible to have the documentation of a function under the
  cursor displayed with a simple keystroke which should alleviate
  having slightly less documentation in the types.
- Improve the documentation.
- Use Dune (not the former Jbuilder) to compile.
