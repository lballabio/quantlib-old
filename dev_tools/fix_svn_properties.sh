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
    -o -name '*.dsw' -o -name '*.dsp' \
 | xargs -n 1 svn propset svn:eol-style CRLF
# these should have svn:needs-lock
find . -name '*.png' -o -name '*.jpg' -o -name '*.xls' \
    -o -name '*.xla' -o -name '*.xll' \
 | xargs -n 1 svn propset svn:needs-lock
# these should have svn:mime-type application/vnd.ms-excel
find . -name '*.xla' -o -name '*.xll' -o -name '*.xls' \
 | xargs -n 1 svn propset svn:mime-type application/vnd.ms-excel
# these should have svn:executable
find . -name '*.sh' \
 | xargs -n 1 svn propset svn:executable
