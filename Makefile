# Makefile for developers (users use dune exclusively).
PKGVERSION = $(shell git describe --always)
PACKAGES = $(basename $(wildcard *.opam))

build:
	dune build @install @examples

test:
	dune runtest --force

install uninstall:
	dune $@

doc: build
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/src/cairo.mli
	dune build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

submit:
	topkg distrib --skip-tests
# 	Create packages and perform the subtitution that topkg does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	for p in $(PACKAGES); do \
	  topkg opam pkg -n $$p; \
	  sed -e 's/\(^ *"cairo2"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/$$p.$(PKGVERSION)/opam; \
	done
# until we have https://github.com/ocaml/opam-publish/issues/38
	[ -d packages ] || (echo "ERROR: Make a symbolic link packages → \
		opam-repo/packages"; exit 1)
	for p in $(PACKAGES); do \
	  mkdir -p packages/$$p; \
	  cp -r _build/$$p.$(PKGVERSION) packages/$$p/; \
	done
	cd packages && git add $(PACKAGES)
#	CONDUIT_TLS=native topkg opam submit $(addprefix -n, $(PACKAGES))


.PHONY: web web-html tutorial
web-html: doc
	$(MAKE) -C doc $@
web tutorial: all doc
	$(MAKE) -C doc $@

lint:
	for p in $(PACKAGES); do opam lint $$p.opam; done

clean:
	dune clean
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg)

.PHONY: build install uninstall doc submit lint clean
