# Process this file with autoconf to produce a configure script.
AC_INIT([Object Handler], [0.0.1],
        [quantlib-dev@lists.sourceforge.net],
        [ObjectHandler])
AC_PREREQ(2.50)
AC_CONFIG_SRCDIR([ObjectHandler/objecthandler.hpp])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([ObjectHandler/config.hpp])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([Copyright (C) 2004, 2005 Eric Ehlers

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it under the
terms of the QuantLib license.  You should have received a copy of the
license along with this program; if not, please email quantlib-dev@lists.sf.net
The license is also available online at http://quantlib.org/html/license.html

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.])

#AC_PROG_AWK
AC_PROG_INSTALL

# Default compilation variables

AC_SUBST([CXXFLAGS],[${CXXFLAGS='-g -O2 -Wall'}])

# Help the compiler find external software

#AC_ARG_WITH([boost-include],
#            AC_HELP_STRING([--with-boost-include=INCLUDE_PATH],
#                           [Supply the location of Boost header files]),
#            [ql_boost_include_path=$withval],
#            [ql_boost_include_path=""])
#if test [ -n "$ql_boost_include_path" ] ; then
#   AC_SUBST([CPPFLAGS],["${CPPFLAGS} -I${ql_boost_include_path}"])
#fi
#AC_ARG_WITH([boost-lib],
#            AC_HELP_STRING([--with-boost-lib=LIB_PATH],
#                           [Supply the location of Boost libraries]),
#            [ql_boost_lib_path=$withval],
#            [ql_boost_lib_path=""])
#if test [ -n "$ql_boost_lib_path" ] ; then
#   AC_SUBST([LDFLAGS],["${LDFLAGS} -L${ql_boost_lib_path}"])
#fi

# Continue setup

AC_PROG_CC
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_LIBTOOL

AC_LANG([C++])

# Check for tools needed for building documentation
AC_PATH_PROG([DOXYGEN], [doxygen])
AC_PATH_PROG([SED], [sed])
AC_PATH_PROG([LATEX], [latex])
AC_PATH_PROG([PDFLATEX], [pdflatex])
AC_PATH_PROG([MAKEINDEX], [makeindex])
AC_PATH_PROG([DVIPS], [dvips])

# done, output the configured files
AC_CONFIG_FILES([
    Makefile
    ObjectHandler/Makefile
    Example/Makefile
    Docs/Makefile])
#AC_CONFIG_FILES([quantlib-config],[chmod +x quantlib-config])
#AC_CONFIG_FILES([QuantLib.spec])
AC_OUTPUT
