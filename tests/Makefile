ROOT=..
include $(ROOT)/Makefile.conf

# Do not require that the library is installed to compile the tests:
OCAMLINCLUDES = $(ROOT)/src
LIBS_CMXA = bigarray.cmxa cairo.cmxa
OCAMLOPT_FLAGS = -dtypes -inline 3 -g

SOURCES = image.ml

default: $(SOURCES:.ml=.com)


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *~ *.com *.exe *.png)
