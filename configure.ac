AC_INIT(cairo, 0.2, christophe.troestler@umons.ac.be)

AC_MSG_RESULT([configuring $PACKAGE_STRING])

# OCaml
########################################################################

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB

AC_CHECK_OCAML_OS_TYPE

# C & Cairo library
########################################################################

# AC_DEF_ARG_WITH(arg-with, variable, default-unix, default-win, help, msg)
AC_DEFUN([AC_DEF_ARG_WITH],
[dnl
  if test "$OCAML_OS_TYPE" = "Win32"; then
    $2="$4"
  else
    $2="$3"
  fi
  AC_ARG_WITH($1, [  $5 @<:@unix=$3,
                          win32=$4@:>@],
    [$2="$withval"], [])
  if test "`echo \"$$2\" | grep \" \"`" != ""; then
    AC_MSG_ERROR([The path passed to --with-$1 must NOT contain spaces])
  fi
  AC_MSG_RESULT([Path for $6... $$2])
])

AC_DEF_ARG_WITH(cairo-lib, [CAIRO_LIB], [/usr/lib], [C:/gtk/lib],
  [--with-cairo-lib=DIR    path to find cairo lib],
  [the cairo library])
LDFLAGS="$LDFLAGS -L$CAIRO_LIB"

AC_DEF_ARG_WITH(cairo-inc, [CAIRO_INCS], [/usr/include/cairo],
  [C:/gtk/include/cairo],
  [--with-cairo-inc=DIR    path to find cairo headers],
  [cairo.h])
CPPFLAGS="$CPPFLAGS -I $CAIRO_INCS"

AC_PROG_CC()

AC_SEARCH_LIBS([cairo_create], [cairo-2 cairo], [],
  [AC_MSG_ERROR([lib cairo missing, use --with-cairo-lib])])

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
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_CONFIG_FILES([src/Makefile], [chmod a-w src/Makefile])
AC_OUTPUT
