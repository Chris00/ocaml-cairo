ROOT=..
include $(ROOT)/Makefile.conf

IMAGES=$(shell grep .png tutorial.html | sed "s/.*\"\(.*\.png\).*/\1/")

.PHONY: web  web-html
web-html:
	cd html/ && scp *.html *.css $(FORGE_DOC)/cairo
# Tutorial
	sed -e 's/html\///' tutorial.html | sed -e 's/\.\.\/examples\///' \
	> index.html && \
	scp index.html *.css $(FORGE_DOC)/cairo && \
	$(RM) index.html

web: $(IMAGES) web-html
# Code
	FILES=`grep "\.\./examples/" tutorial.html | \
	sed  -e "s/.*\(\.\.\/examples\/[a-z_]*\.ml\).*/\1/"` && \
	scp $$FILES $(FORGE_DOC)/cairo
# Images
	scp *.png $(FORGE_DOC)/cairo

$(IMAGES):
	for f in $(TUTORIAL_SOURCES:.ml=.com); do \
	  $(ROOT)/examples/$$f; \
	done


clean:
	$(RM) $(wildcard *~ *.png *.svg)