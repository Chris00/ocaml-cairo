ROOT=..
include $(ROOT)/Makefile.conf

# Does not require that the library is installed to compile the examples:
OCAMLINCLUDES = ../src
LIBS_CMXA = bigarray.cmxa cairo.cmxa
OCAMLOPT_FLAGS = -dtypes -inline 3 -g

default: arcs.com


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) *.com *.exe
