# Process this file with autoconf to produce a configure script.
AC_INIT([QuantLib-SWIG], [0.9.0],
        [quantlib-dev@lists.sourceforge.net],
        [QuantLib-SWIG])
AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([Copyright (C) 2005, 2006, 2007 StatPro Italia srl

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it
under the terms of the QuantLib license.  You should have received a
copy of the license along with this program; if not, please email
<quantlib-dev@lists.sf.net>. The license is also available online at
<http://quantlib.org/reference/license.html>.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.])

# Check for tools
AC_PATH_PROG([SWIG], [swig])

AC_PATH_PROG([PYTHON], [python])
AM_CONDITIONAL(HAVE_PYTHON, test "x${PYTHON}" != "x")

AC_PATH_PROG([PERL], [perl])
AM_CONDITIONAL(HAVE_PERL, test "x${PERL}" != "x")

AC_PATH_PROG([RUBY], [ruby])
AM_CONDITIONAL(HAVE_RUBY, test "x${RUBY}" != "x")

AC_PATH_PROG([MZSCHEME], [mzscheme])
AM_CONDITIONAL(HAVE_MZSCHEME, test "x${MZSCHEME}" != "x")

AC_PATH_PROG([GUILE], [guile])
AM_CONDITIONAL(HAVE_GUILE, test "x${GUILE}" != "x")

AC_PATH_PROGS([MCS], [gmcs mcs])
AM_CONDITIONAL(HAVE_MCS, test "x${MCS}" != "x")

AC_PATH_PROG([OCAMLC], [ocamlc])
AM_CONDITIONAL(HAVE_OCAML, test "x${OCAMLC}" != "x")

AC_PATH_PROG([R], [R])
AM_CONDITIONAL(HAVE_R, test "x${R}" != "x")

# Default compilation variables

# check target
target=`uname`
case "$target" in
Linux ) AC_SUBST([SHARED_LIB],[${SHARED_LIB='-shared'}]) 
	AC_SUBST([_JAVA_HOME_],[${_JAVA_HOME_=$JAVA_HOME}])
	AC_SUBST([JAVA_SYSTEM_INCLUDE],[${_JAVA_SYSTEM_INCLUDE=$JAVA_HOME/include/linux}])
	AC_SUBST([LIB_EXTENSION],[${LIB_EXTENSION='so'}]) ;;
Darwin ) AC_SUBST([SHARED_LIB],[${SHARED_LIB='-dynamiclib'}])
	AC_SUBST([_JAVA_HOME_],[${_JAVA_HOME_='/Library/Java/Home'}])
	AC_SUBST([JAVA_SYSTEM_INCLUDE],[${JAVA_SYSTEM_INCLUDE='/Library/Java/Home/include'}])
	AC_SUBST([LIB_EXTENSION],[${LIB_EXTENSION='dylib'}]) ;;
esac

AC_MSG_CHECKING([jdk include])

if test -z "${_JAVA_HOME_}"
then
AC_MSG_ERROR([set \$JAVA_HOME enveronment to home path to jdk])
else
AC_SUBST([JAVA_INCLUDE],[${JAVA_INCLUDE=$_JAVA_HOME_/include}])
JAVA_INCLUDE=$_JAVA_HOME_/include
AC_MSG_RESULT([$JAVA_INCLUDE])
fi


AC_MSG_CHECKING([system])
AC_MSG_RESULT([$target])
AC_MSG_CHECKING([shared linker flag])
AC_MSG_RESULT([$SHARED_LIB])


AC_SUBST([CXXFLAGS],[${CXXFLAGS='-g -O2 -Wall'}])

# done, output the configured files
AC_CONFIG_FILES([
    Makefile
    CSharp/Makefile
    Guile/Makefile
    Java/Makefile
    MzScheme/Makefile
    OCaml/Makefile
    Perl/Makefile
    Python/Makefile
    R/Makefile
    Ruby/Makefile])
AC_CONFIG_FILES([Python/QuantLib-Python.spec])
AC_CONFIG_FILES([Ruby/QuantLib-Ruby.spec])
AC_OUTPUT

