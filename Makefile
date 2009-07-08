ROOT=.
include Makefile.conf

.PHONY: default all opt byte native install uninstall htdoc doc examples
default: byte opt
all: byte
opt: native
htdoc: doc
byte native install uninstall doc:
	$(MAKE) -C src $@
examples: native
	$(MAKE) -C examples

# Depends on the version number set in delimited_overloading.mli :
cairo.godiva: cairo.godiva.in
	@ sed -e "s/@PACKAGE@/$(PACKAGE)/" $< \
	| sed -e "s/@VERSION@/$(VERSION)/" \
	| sed -e "s/@TARBALL@/$(TARBALL)/" \
	| sed -e "s/@DOWNLOAD@/$(OCAMLFORGE_FILE_NO)/" > $@
	@ echo "Updated \"$@\"."

# Assume the environment variable $GODI_LOCALBASE is set
.PHONY: godi
godi: cairo.godiva
	godiva $<

# "Force" a tag to be defined for each released tarball
tar:
	bzr export /tmp/$(TARBALL) -r "tag:$(VERSION)"
	@echo "Created tarball '/tmp/$(TARBALL)'."

.PHONY: web
web: doc
	cd doc/html && scp *.html *.css $(FORGE_DOC)/cairo
	cd doc && \
	sed -e 's/html\///' tutorial.html | sed -e 's/\.\.\/examples\///' \
	> index.html && \
	scp index.html *.css $(FORGE_DOC)/cairo && \
	$(RM) index.html
	cd doc && \
	FILES=`grep "../examples/" tutorial.html | \
	sed  -e "s/.*\(\.\.\/examples\/.*\.ml\).*/\1/"` && \
	scp $$FILES $(FORGE_DOC)/cairo
	cd doc && scp *.png $(FORGE_DOC)/cairo

.PHONY: sync-scm sync_scm
sync-scm sync_scm:
	bzr push  svn+ssh://svn.forge.ocamlcore.org/svnroot/archimedes/cairo

.PHONY: clean dist-clean
clean:
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg) cairo.godiva
	$(MAKE) -C src $@

dist-clean::
	$(RM) -r aclocal.m4 autom4te.cache config.log config.status
