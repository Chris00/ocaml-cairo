



INSTALLATION on Linux
---------------------

In order to compile these bindings, you need to install the cairo
library, including its development files (e.g. packages libcairo2 and
libcairo2-dev in Debian).  The installation of the library depends on
findlib. Then issue

./configure
make
make examples   # optional
make install

If you use the bzr version, you also need autoconf and automake in
order to create the configure script with ./bootstrap



INSTALLATION on windows
-----------------------

