
On Linux/Unix, you can run:

./configure
make
make check
make install

to build, test and install al modules. If you're only interested in a
specific language, you can tell make to only work in its subdirectory,
as in:

make -C Python

Alternatively, you can cd to a specific subdirectory and follow the
instructions in its README file. This is also the procedure for
Windows users.

