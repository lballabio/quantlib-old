#! /bin/sh
# Regenerate the files autoconf / automake

cp README.txt README
cp LICENSE.txt license.apl
libtoolize --force --automake

rm -f config.cache
rm -f config.log
aclocal
autoheader
autoconf
automake -a
