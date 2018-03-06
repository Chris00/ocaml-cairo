# Makefile for developers (users use jbuilder/dune exclusively).
PKGVERSION = $(shell git describe --always --dirty)

build:
	jbuilder build @install #--dev
	jbuilder build @examples

test:
	jbuilder runtest
	@cd _build/default/tests/ && for p in *.exe; do \
	  echo "▸ $$p"; ./$$p; \
	done

install uninstall:
	jbuilder $@

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/curve_sampling.mli \
	  > _build/default/src/curve_sampling.mli
	jbuilder build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

include Makefile.conf

.PHONY: web web-html tutorial
web-html: doc
	$(MAKE) -C doc $@
web tutorial: all doc
	$(MAKE) -C doc $@

lint:
	@opam lint cairo2.opam
	@opam lint cairo2-gtk.opam

clean:
	jbuilder clean
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg)

.PHONY: build install uninstall doc lint clean
