ROOT=../..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
LIBS_CMXA = bigarray.cmxa cairo.cmxa
OCAMLOPT_FLAGS = -dtypes -inline 3 -g

default: run.com

run.com: cloud.cmx


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) *.com *.exe
