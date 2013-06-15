# Makefile for developers (users use oasis exclusively).
ROOT=.
include Makefile.conf

PKGNAME = $(shell oasis query name)
PKGVERSION = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES = AUTHORS.txt INSTALL.txt README.txt _oasis _tags myocamlbuild.ml \
  config.ml setup.ml Makefile src/META src/API.odocl \
  $(wildcard $(addprefix src/, *.ml *.mli *.mllib *.c *.h *.clib)) \
  tests/ examples/ $(addprefix doc/,Makefile tutorial.css tutorial.html)

.PHONY: all byte native configure doc install uninstall reinstall upload-doc

# setup.ml modifies setup_t => not compatible with dyn mode for oasis ≤ 0.3
SAVE = cp _tags _tags.bak && cp myocamlbuild.ml myocamlbuild.ml.bak \
	&& cp setup.ml setup.ml.bak
RESTORE = cp _tags.bak _tags && cp myocamlbuild.ml.bak myocamlbuild.ml \
	&& cp setup.ml.bak setup.ml

all byte native: configure
	$(SAVE) && oasis setup
	ocaml setup.ml -build
	$(RESTORE)

configure: setup.data
setup.data: setup.ml config.ml
	$(SAVE) && oasis setup
	ocaml $< -configure
	$(RESTORE)

setup.ml: _oasis
	oasis setup -setup-update dynamic

doc install uninstall reinstall: setup.data
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -r _build/src/API.docdir/ $(WEB)/API

# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
# Generate full oasis file (independent of oasis lib).
	cd $(PKGNAME)-$(PKGVERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	rm -rf $(PKGNAME)-$(PKGVERSION)

.PHONY: tests
tests: native
	$(MAKE) -C tests

.PHONY: web web-html tutorial
web-html: doc
	$(MAKE) -C doc $@
web tutorial: all doc
	$(MAKE) -C doc $@


.PHONY: clean dist-clean distclean
clean:
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg) cairo.godiva setup.data
	$(MAKE) -C doc $@

distclean dist-clean::
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)

