# Process this file with autoconf to produce a configure script.
AC_INIT([QuantLibAddin], [0.3.9],
        [quantlib-dev@lists.sourceforge.net],
        [QuantLibAddin])
AC_PREREQ(2.50)
AC_CONFIG_SRCDIR([qla/qladdin.hpp])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([qla/config.hpp])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([    Copyright (C) 2004, 2005 Eric Ehlers

    Copyright (C) 2004, 2005 Ferdinando Ametrano
    Copyright (C) 2004, 2005 StatPro Italia srl

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it under the
terms of the QuantLib license.  You should have received a copy of the
license along with this program; if not, please email quantlib-dev@lists.sf.net
The license is also available online at http://quantlib.org/html/license.html

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.])

AC_PROG_INSTALL

# Default compilation variables

AC_SUBST([CXXFLAGS],[${CXXFLAGS='-g -O2 -Wall'}])

# Continue setup

AC_PROG_CC
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_LIBTOOL
AC_LANG([C++])

# Check for tools needed for building documentation
AC_PATH_PROG([DOXYGEN], [doxygen])
AC_PATH_PROG([SED], [sed])

# possibly disable features
AC_ARG_ENABLE([calc],
              AC_HELP_STRING([--enable-calc],
                             [If enabled (the default) the Calc addin is
                              built by make.]),
              [qla_build_calc=$enableval],
              [qla_build_calc=yes])
AM_CONDITIONAL(BUILD_CALC, test "$qla_build_calc" != "no")

# done, output the configured files
AC_CONFIG_FILES([
    Makefile
    Autogen/Makefile
    qla/Makefile
    qla/objects/Makefile
    qla/functions/Makefile
    Addins/C/Makefile
    Addins/Calc/Makefile
    Addins/Excel/Makefile
    Clients/C/Makefile
    Clients/C++/Makefile
    Clients/Calc/Makefile
    Clients/Excel/Makefile
    Docs/Makefile])
AC_OUTPUT

