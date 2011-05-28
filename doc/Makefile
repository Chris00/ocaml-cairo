ROOT=..
include $(ROOT)/Makefile.conf

TUTORIAL = $(WEB)/tutorial

IMAGES=$(shell grep .png tutorial.html | sed "s/.*\"\(.*\.png\).*/\1/")

.PHONY: web  web-html tutorial
web-html:
	cd html/ && scp *.html *.css $(TUTORIAL)
# Tutorial
	sed -e 's/html\///' tutorial.html | sed -e 's/\.\.\/examples\///' \
	> index.html && \
	scp index.html *.css $(TUTORIAL) && \
	$(RM) index.html

web: $(IMAGES) web-html
# Code
	FILES=`grep "\.\./examples/" tutorial.html | \
	sed  -e "s/.*\(\.\.\/examples\/[a-z_]*\.ml\).*/\1/"` && \
	scp $$FILES $(TUTORIAL)
# Images
	scp *.png $(TUTORIAL)

tutorial: $(IMAGES)
# Just generate the images (the .html file is ok)

$(IMAGES):
	for f in $(TUTORIAL_SOURCES:.ml=.byte); do \
	  $(ROOT)/_build/examples/$$f; \
	done


clean:
	$(RM) $(wildcard *~ *.png *.svg)