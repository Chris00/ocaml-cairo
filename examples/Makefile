ROOT=..
include $(ROOT)/Makefile.conf

# Does not require that the library is installed to compile the examples:
OCAMLINCLUDES = $(ROOT)/src
LIBS_CMXA = bigarray.cmxa cairo.cmxa
OCAMLOPT_FLAGS = -dtypes -inline 3 -g

SOURCES = arcs.ml clip.ml curve_to.ml dash.ml fill_stroke.ml \
  fill_style.ml gradient.ml set_line_cap.ml set_line_join.ml \
  text.ml text_align_center.ml text_extents.ml text_rotate.ml

default: $(SOURCES:.ml=.com)
	$(MAKE) -C word_cloud


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) *.com *.exe
