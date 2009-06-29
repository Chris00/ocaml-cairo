AC_INIT(cairo, 0.1, christophe.troestler@umons.ac.be)

AC_MSG_RESULT([configuring $PACKAGE_STRING])

# C & Cairo library
########################################################################

AC_PROG_CC()

AC_SEARCH_LIBS([cairo_create], [cairo], [CAIROLIB="$withval"])

CAIRO_INCS="/usr/include/cairo"
AC_ARG_WITH(cairo,
    [  --with-cairo=INCLUDES (-I options to find cairo headers, default=/usr/include/cairo)],
    [CAIRO_INCS="$withval"], [])
CPPFLAGS="$CPPFLAGS -I $CAIRO_INCS"

AC_CHECK_HEADERS([cairo.h], , [AC_MSG_ERROR(cairo.h missing)])

# Require cairo >= 1.6 for extend to be fully implemented
AC_TRY_RUN([#include <cairo.h>
        int main(int argc, char **argv) {
            if(CAIRO_VERSION_MAJOR >= 1 && CAIRO_VERSION_MINOR >= 6) {
	      return(0);
            } else {
	      return(1);
	    }
        }],
	[],
	[AC_MSG_WARN([Please install cairo version >= 1.6])])


dnl AC_TRY_RUN([#include <cairo.h>
dnl         int main(int argc, char **argv) {
dnl             if(CAIRO_STATUS_LAST_STATUS == 34) { return(0); }
dnl             else { return(1); }
dnl         }],
dnl 	[],
dnl 	[AC_MSG_FAILURE([The type [status] must be updated to take into
dnl 	 account the new cairo errors.  Please use a newer version of
dnl 	 these cairo bindings or contact AC_PACKAGE_BUGREPORT
dnl 	 to report the issue.])
dnl 	 ])


# OCaml
########################################################################

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB


# substitutions to perform
AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLFIND)

# Finally create the Makefile and samples (read only to avoid changing
# them by mistake)
AC_CONFIG_FILES([Makefile.ocaml],[chmod a-w Makefile.ocaml])
#AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_OUTPUT
