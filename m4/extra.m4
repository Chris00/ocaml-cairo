AC_DEFUN([AC_CHECK_OCAML_OS_TYPE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([OCaml Sys.os_type])

  cat > conftest.ml <<EOF
  print_string(Sys.os_type);;
EOF

  OCAML_OS_TYPE=`ocaml conftest.ml`
  AC_MSG_RESULT([$OCAML_OS_TYPE])
  AC_SUBST([OCAML_OS_TYPE])

])