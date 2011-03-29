



INSTALLATION on Linux
---------------------

In order to compile these bindings, you need to install the cairo
library, including its development files (e.g. packages libcairo2 and
libcairo2-dev in Debian).  To use Cairo with GTK+, please also install
lablgtk2.  The installation of the library depends on findlib. Then
issue

./configure
make
make examples   # optional
make install    # requires findlib

If you use the bzr version, you also need autoconf and automake in
order to create the configure script with ./autogen.sh


INSTALLATION on windows
-----------------------

To install Cairo, we recommend that you download GTK+ for windows [1]
and install it in C:/gtk (you can install it in any other directory
whose path contains NO space but you will have to specify them with
--with-cairo-lib and --with-cairo-inc) and put C:\gtk\bin in your PATH
environment variable.  To use Cairo with GTK+, please also install
lablgtk2 [2].  Then (in a cygwin shell), issue

./configure
make
make examples   # optional
make install    # requires findlib

If you use the bzr version, you also need the autoconf and automake
Cygwin packages in order to create the configure script with
./autogen.sh

[1] http://www.gtk.org/download-windows.html 
[2] http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html

Tutorial images
---------------

To generate the tutorial images, issue

make tutorial

from the toplevel directory.  Then open doc/tutorial.html in your
favorite browser.
