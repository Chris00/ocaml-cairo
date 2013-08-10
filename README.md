OCaml interface to Cairo
========================

This is an OCaml binding for the
[Cairo](http://www.cairographics.org/) library, a 2D graphics library
with support for multiple output devices.

Perquisites
-----------

You need the development file of Cairo (e.g. on Debian, you must
install the package ``libcairo2-dev``) and the OCaml package
``lablgtk2`` (in the [OPAM](http://opam.ocamlpro.com/) package
``lablgtk``).

Compilation & Installation
--------------------------

This library uses [OASIS](http://oasis.forge.ocamlcore.org/), so
compiling is as simple as:

    ocaml setup.ml -configure
	ocaml setup.ml -build

You can then install with:

	ocaml setup.ml -install


Examples
--------

You can read a version of the
[Cairo tutorial](http://cairo.forge.ocamlcore.org/tutorial/) using
this module.  The code of this tutorial is available in the
``examples/`` directory.  They will be compiled with the above
commands (but will not installed).
