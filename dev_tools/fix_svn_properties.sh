#!/bin/sh
# text files
find . -name '*.[hc]pp' -o -name '*.[hc]' \
    -o -name '*.html' -o -name '*.css' -o -name '*.docs' -o -name '*.doxy' \
    -o -name '*.sh' -o -name '*.bat' -o -name '*.el' \
    -o -iname '*.txt' -o -name '*.tex' \
    -o -name '*.xml' -o -name 'stub.*' \
    -o -name '*.i' -o -name '*.py' -o -name '*.rb' -o -name '*.R' \
    -o -iname '*.pl' -o -name '*.scm' -o -name '*.ss' \
    -o -name '*.cs' -o -name '*.java' \
 | xargs -n 1 svn propset svn:eol-style native
# these should have UNIX eol, even when extracted from a .zip
find . -name '*.a[cm]' -o -name '*.m4' -o -name '*.in'  -o -name '*.1' \
 | xargs -n 1 svn propset svn:eol-style LF
# these should have Windows eol, even when extracted from a .tar.gz
find . -name '*.dev' -o -name '*.nsi' -o -name '*.sln' \
    -o -name '*.vcproj' -o -name '*.csproj' -o -name '*.cmd' \
 | xargs -n 1 svn propset svn:eol-style CRLF
