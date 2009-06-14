ROOT=.
include Makefile.conf

.PHONY: all opt byte native install uninstall htdoc doc
all: byte
opt: native
htdoc: doc
byte native install uninstall doc:
	$(MAKE) -C src $@

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

.PHONY: sync-scm
sync-scm:
	bzr push  svn+ssh://svn.forge.ocamlcore.org/svnroot/archimedes/cairo

.PHONY: clean
clean:
	$(RM) -f $(wildcard *~)
	$(MAKE) -C src $@
	$(RM) -f cairo.godiva
