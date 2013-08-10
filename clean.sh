#!/bin/sh

sed '/(\* OASIS_START \*)/,/(\* OASIS_STOP \*)/ {
  /(\* OASIS_STOP \*)/ c\
(* OASIS_START *)\n(* OASIS_STOP *)
  d
  }' myocamlbuild.ml > myocamlbuild.ml.bak \
      && mv myocamlbuild.ml.bak myocamlbuild.ml

sed '/# OASIS_START/,/# OASIS_STOP/ {
  /# OASIS_STOP/ c\
# OASIS_START\n# OASIS_STOP
  d
  }' _tags > _tags.bak \
      && mv _tags.bak _tags
