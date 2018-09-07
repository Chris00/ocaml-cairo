# This publishes the tutorial.  The interface is published by dune-release

TEMP_DIR=/tmp/cairo-doc

PKGVERSION = $(shell git describe --always)

IMAGES_BASE=$(shell grep .png tutorial.html | sed "s/.*\"\(.*\.png\).*/\1/")
IMAGES=$(addprefix $(TEMP_DIR)/, $(IMAGES_BASE))

tutorial-html:
	$(MAKE) images
	mkdir -p $(TEMP_DIR)
	sed -e 's|html/|doc/cairo2/|' tutorial.html \
	| sed -e 's|\.\./examples/|https://github.com/Chris00/ocaml-cairo/tree/master/examples/|' \
	> $(TEMP_DIR)/index.html
	cp $(wildcard *.css) $(TEMP_DIR)

IMAGES_PGM_DIR=$(shell realpath `pwd`/../_build/default/examples)

images $(IMAGES):
	$(MAKE) -C.. build
	mkdir -p $(TEMP_DIR)
	cd $(TEMP_DIR) && for p in $(IMAGES_PGM_DIR)/*.exe; do $$p; done

tutorial-submit: tutorial-html
	$(RM) -rf $(TEMP_DIR)
	git clone https://github.com/Chris00/ocaml-cairo.git \
	  --branch gh-pages --depth 1 $(TEMP_DIR)
	$(MAKE) tutorial-html
# dune-release does not handle multiple packages.  So we must upload
# the full documentation "by hand".
	$(MAKE) -C.. doc
	mkdir -p $(TEMP_DIR)/doc
	cp -a ../_build/default/_doc/_html/*  $(TEMP_DIR)/doc/
	sed "s|</h2>|</h2><p>Documentation for version $(PKGVERSION).</p>|" \
	  --in-place $(TEMP_DIR)/doc/index.html
	cd $(TEMP_DIR) && git add . \
	&& git commit -m "Tutorial for version $(PKGVERSION)" \
	&& git push origin

.PHONY: tutorial-html images tutorial-submit
