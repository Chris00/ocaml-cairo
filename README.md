OCaml interface to Cairo
========================

This is an OCaml binding for the
[Cairo](http://www.cairographics.org/) library, a 2D graphics library
with support for multiple output devices.

Prerequisites
-------------

You need the development file of Cairo (e.g. on Debian, you must
install the package ``libcairo2-dev``) and the OCaml package
``lablgtk2`` (in the [OPAM](http://opam.ocamlpro.com/) package
``lablgtk``).

Compilation & Installation
--------------------------

The easier way to install this library — once the prerequisites are set
up — is to use [opam](http://opam.ocaml.org/):

    opam install cairo2

If you would like to compile from the sources, install
[OASIS](http://oasis.forge.ocamlcore.org/):

    opam install oasis

and do:

    ocaml setup.ml -configure
	ocaml setup.ml -build

You can then install it with:

	ocaml setup.ml -install


Examples
--------

You can read a version of the
[Cairo tutorial](http://cairo.forge.ocamlcore.org/tutorial/) using
this module.  The code of this tutorial is available in the
``examples/`` directory.  To compile it, you must configure the
project with

    ocaml setup.ml -configure --enable-examples
