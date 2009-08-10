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

AC_DEF_ARG_WITH(cairo-inc, [CAIRO_INC], [/usr/include/cairo],
  [C:/gtk/include/cairo],
  [--with-cairo-inc=DIR    path to find cairo headers],
  [cairo.h])
CPPFLAGS="$CPPFLAGS -I $CAIRO_INC"

AC_PROG_CC()

AC_SEARCH_LIBS([cairo_create], [cairo-2 cairo], [],
  [AC_MSG_ERROR([lib cairo missing, use --with-cairo-lib])])

AC_CHECK_HEADERS([cairo.h], ,
  [AC_MSG_ERROR([cairo.h missing, use --with-cairo-inc])])

AC_LANG_PUSH([C])
# Require cairo >= 1.6 for extend to be fully implemented
AC_RUN_IFELSE([AC_LANG_SOURCE([
        #include <cairo.h>
        int main(int argc, char **argv) {
            if(CAIRO_VERSION_MAJOR >= 1 && CAIRO_VERSION_MINOR >= 6) {
	      return(0);
            } else {
	      return(1);
	    }
        }
      ])],
      [],
      [AC_MSG_WARN([Please install cairo version >= 1.6])],
      [AC_MSG_FAILURE([Cross compiling not supported])])


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

AC_LANG_POP([C])

# Optional GTK support (for the X11 backend only?)
AC_ARG_WITH(gtk, 
	    AS_HELP_STRING([--with-gtk],[Cairo/GTK+ integration via LablGTK]),
	    USE_GTK=$withval, USE_GTK=yes)
AC_ARG_VAR(LABLGTKDIR,[Location of the LablGTK library])

AC_DEF_ARG_WITH(gtk-inc, [GTK_INC], [/usr/include/gtk-2.0],
  [C:/gtk/include],
  [--with-gtk-inc=DIR      path to Gtk+ headers],
  [cairo.h])
CPPFLAGS="$CPPFLAGS -I $GTK_INC"

if test $USE_GTK = yes ; then
  # Check for LablGTK (we need the full paths for C header files)
  AC_CHECK_OCAML_MODULE(LABLGTKDIR, lablgtk2, Gobject,
    $OCAMLLIB/lablgtk2 $OCAMLLIB/lablgtk)

  dnl ml_gobject.h,... are supposed to be in LABLGTKDIR:
  CPPFLAGS="$CPPFLAGS -I $LABLGTKDIR"
  OCAMLINCLUDES="$OCAMLINCLUDES $LABLGTKDIR"

  if test "$LABLGTKDIR" ; then
    # Check for gdk and gdk-pixbuf (=> GDK_CFLAGS, GDK_LIBS)
    if test "$OCAML_OS_TYPE" = "Win32"; then
      # FIXME: test to be added
      USE_GTK=no
    else
      PKG_CHECK_MODULES(GDK, gdk-2.0 gdk-pixbuf-2.0, :, USE_GTK=no)
    fi

  else
    USE_GTK=no
  fi
fi

AC_SUBST([USE_GTK])

# Under Cygwin, ocamlc may fail due to gcc being a symbolic link...
AC_CHECK_PROGS([READLINK], [readlink], [no])
if test "$READLINK" != "no"  -a  -d /cygdrive ; then
  PATH_CC=`which "$CC"`
  TARGET_CC=`$READLINK -f "$PATH_CC"`
  if test "$TARGET_CC" != "$PATH_CC"; then
    echo CYGWIN SYMLINK ISSUE: rename $TARGET_CC into $PATH_CC
  fi
fi


# Additional substitutions to perform
AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(OCAMLINCLUDES)
AC_SUBST(LINKPKG)

# Finally create the Makefile and samples (read only to avoid changing
# them by mistake)
AC_CONFIG_FILES([Makefile.ocaml],[chmod a-w Makefile.ocaml])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile], [chmod a-w src/Makefile])
AC_OUTPUT
