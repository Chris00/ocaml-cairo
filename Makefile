# Makefile for developers (users use dune exclusively).
PKGVERSION = $(shell git describe --always)
PACKAGES = $(basename $(wildcard *.opam))

build:
	dune build @install @examples @tutorial

test:
	dune build @runtest @tests-gtk --force

install uninstall:
	dune $@

doc: build
	dune build @doc
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/cairo2/Cairo/index.html

tutorial-submit: build doc
	$(MAKE) -C docs $@

lint:
	for p in $(PACKAGES); do opam lint $$p.opam; done

clean:
	dune clean
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg)

.PHONY: build install uninstall doc submit tutorial-submit lint clean
