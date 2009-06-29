ROOT=.
include Makefile.conf

.PHONY: default all opt byte native install uninstall htdoc doc
default: byte opt
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
	$(RM) $(wildcard *~) cairo.godiva
	$(MAKE) -C src $@
