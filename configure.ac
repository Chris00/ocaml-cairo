AC_INIT(cairo, 0.1, christophe.troestler@umons.ac.be)

AC_MSG_RESULT([configuring $PACKAGE_STRING])

# OCaml
########################################################################

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   OCAMLFIND=
   LINKPKG=
else
   #LINKPKG=-linkpkg
   OCAMLFIND_OCAMLC="$OCAMLFIND ocamlc"
   OCAMLFIND_OCAMLOPT="$OCAMLFIND ocamlopt"
   OCAMLFIND_OCAMLDEP="$OCAMLFIND ocamldep"
   OCAMLFIND_OCAMLDOC="$OCAMLFIND ocamldoc"
fi

# C & Cairo library
########################################################################

AC_PROG_CC()

AC_ARG_WITH(cairo-lib,
    [  --with-cairo-lib=DIR (-L options to find cairo lib)],
    [LD_PATH="$withval"], [])
AC_SUBST(LD_PATH)

AC_SEARCH_LIBS([cairo_create], [cairo], [],
  [if test "$LD_PATH" = ""; then
     AC_MSG_ERROR([lib cairo missing, use --with-cairo-lib])
   else
     LIBS="-lcairo $LIBS"
   fi])


CAIRO_INCS="/usr/include/cairo"
AC_ARG_WITH(cairo-inc,
    [  --with-cairo-inc=DIR (-I options to find cairo headers, default=/usr/include/cairo)],
    [CAIRO_INCS="$withval"], [])
if test "`echo \"$CAIRO_INCS\" |grep \" \" `" = ""; then
  CPPFLAGS="$CPPFLAGS -I $CAIRO_INCS"
else
  CPPFLAGS="$CPPFLAGS -I '$CAIRO_INCS'"
fi

AC_CHECK_HEADERS([cairo.h], ,
  [AC_MSG_ERROR([cairo.h missing, use --with-cairo-inc])])

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

# Check for freetype
#PKG_CHECK_MODULES(freetype2, freetype2)

# Optional GTK support (for the X11 backend)
AC_ARG_WITH(gtk, 
	    AS_HELP_STRING([--with-gtk],[Cairo/GTK+ integration via LablGTK]),
	    use_gtk=$withval, use_gtk=yes)
AC_ARG_VAR(LABLGTKDIR,[Location of the LablGTK library])

if test $use_gtk = yes ; then
  # Check for LablGTK
  AC_CHECK_OCAML_MODULE(lablgtk, LABLGTKDIR, Gobject, +lablgtk2 +lablgtk)

  if test "$LABLGTKDIR" ; then
    # Check for gdk-pixbuf
#    PKG_CHECK_MODULES(GDK, cairo gdk-2.0 gdk-pixbuf-2.0, :, use_gtk=no)
     AC_CHECK_HEADERS([gdk.h], ,
       [AC_MSG_ERROR([gdk.h missing, try to use --with-gdk])])
  else
    use_gtk=no
  fi
fi


# Under Cygwin, ocamlc may fail due to gcc being a symbolic link...
AC_CHECK_PROGS([READLINK], [readlink], [no])
if test "$READLINK" != "no"  -a  -d /cygdrive ; then
  PATH_CC=`which "$CC"`
  TARGET_CC=`$READLINK -f "$PATH_CC"`
  if test "$TARGET_CC" != "$PATH_CC"; then
    echo CYGWIN SYMLINK ISSUE: rename $TARGET_CC into $PATH_CC
  fi
fi


# substitutions to perform
AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)

AC_SUBST(LINKPKG)

# Finally create the Makefile and samples (read only to avoid changing
# them by mistake)
AC_CONFIG_FILES([Makefile.ocaml],[chmod a-w Makefile.ocaml])
#AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_CONFIG_FILES([src/Makefile], [chmod a-w src/Makefile])
AC_OUTPUT
