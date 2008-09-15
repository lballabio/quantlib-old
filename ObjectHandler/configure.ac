
# Process this file with autoconf to produce a configure script.

AC_INIT([ObjectHandler], [0.9.7],
        [quantlib-dev@lists.sourceforge.net],
        [ObjectHandler])
AC_PREREQ(2.50)
AC_CONFIG_SRCDIR([oh/objecthandler.hpp])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([oh/config.hpp])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([
Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

Copyright (C) 2004, 2005, 2006, 2007 Ferdinando Ametrano
Copyright (C) 2005, 2006 Plamen Neykov
Copyright (C) 2004 StatPro Italia srl

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it under the
terms of the QuantLib license.  You should have received a copy of the
license along with this program; if not, please email
<quantlib-dev@lists.sf.net>. The license is also available online at
<http://quantlib.org/license.shtml>.

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

# Confirm existence of dependencies

AC_CHECK_HEADER([log4cxx/logger.h], [],
    [AC_MSG_ERROR([ObjectHandler depends on log4cxx which cannot be found])])

# Check for tools needed for building documentation

AC_PATH_PROG([DOXYGEN], [doxygen])
AC_PATH_PROG([SED], [sed])

# Configure and validate the path to gensrc

AC_ARG_WITH([gensrc],
            AC_HELP_STRING([--with-gensrc=/path/to/gensrc],
                           [Specify the path to gensrc]),
            [gensrc_path=${withval}],
            [AC_MSG_ERROR([You must specify the location of gensrc: "./configure --with-gensrc=path/to/gensrc"])])
AC_CHECK_FILE([${gensrc_path}/gensrc.py], [],
            [AC_MSG_ERROR([Unable to locate script gensrc.py in specified gensrc directory])])
AC_ARG_VAR(gensrc_dir, path to gensrc)
AC_SUBST(gensrc_dir, [${gensrc_path}])

# Done, output the configured files

AC_CONFIG_FILES([
    Docs/Makefile
    Examples/ExampleObjects/Enumerations/Makefile
    Examples/ExampleObjects/Library/Makefile
    Examples/ExampleObjects/Objects/Makefile
    Examples/ExampleObjects/Serialization/Makefile
    Examples/ExampleObjects/ValueObjects/Makefile
    Examples/ExampleObjects/Makefile
    Examples/C++/Makefile
    Examples/xl/Makefile
    gensrc/Makefile
    Makefile
    oh/conversions/Makefile
    oh/enumerations/Makefile
    oh/loop/Makefile
    oh/valueobjects/Makefile
    oh/Makefile
    ohxl/Makefile
    xlsdk/Makefile ])
AC_OUTPUT

