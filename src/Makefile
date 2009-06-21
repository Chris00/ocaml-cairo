ROOT=..
include $(ROOT)/Makefile.conf

DOC_DIR = $(ROOT)/doc/html/
# Native code versions must be installed if and only if make opt was
# called previously (i.e. the files exist):
INSTALL_FILES = cairo.mli \
  $(wildcard )

.PHONY: all opt byte native
all: byte
opt: native
byte: cairo.cma
native: cairo.cmxa

cairo.cma: cairo.cmo dllcairo_stubs

dllcairo_stubs: cairo.cmo cairo_stubs.o
	ocamlmklib -o $@ $^ -lcairo

cairo_stubs.o: cairo_stubs.c
	$(OCAMLC) -c $^ -I /usr/include/cairo/


.PHONY: doc
OCAMLDOC_FLAGS += $(if $(OCAMLINCLUDES), $(addprefix -I , $(OCAMLINCLUDES)))
doc:
	if [ ! -d "$(DOC_DIR)" ]; then mkdir -p $(DOC_DIR); fi
	$(OCAMLDOC) -html -d $(DOC_DIR) $(OCAMLDOC_FLAGS) $(wildcard *.mli)

META: META.in cairo.mli
	sed -e "s/@PACKAGE_VERSION@/$(VERSION)/" $< > $@

.PHONY: install uninstall
# TODO: copy HTML doc
install: META $(INSTALL_FILES)
	ocamlfind install $(PACKAGE) $^

uninstall:
	ocamlfind remove $(PACKAGE)

include $(ROOT)/Makefile.ocaml
