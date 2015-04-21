# Process this file with autoconf to produce a configure script.
AC_INIT([QuantLibXL], [1.6.0],
        [quantlib-dev@lists.sourceforge.net],
        [QuantLibXL])
AC_PREREQ(2.50)
#AC_CONFIG_SRCDIR([oh/objecthandler.hpp])
#AC_CONFIG_AUX_DIR([config])
#AC_CONFIG_HEADERS([oh/config.hpp])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([
Copyright (C) 2004, 2005, 2006, 2007, 2008 Eric Ehlers
Copyright (C) 2004, 2005, 2006, 2007 Ferdinando Ametrano

Copyright (C) 2004, 2005, 2006 StatPro Italia srl
Copyright (C) 2005, 2006 Aurelien Chanudet
Copyright (C) 2005, 2006 Plamen Neykov
Copyright (C) 2005 Walter Penschke
Copyright (C) 2006, 2007 Chiara Fornarola
Copyright (C) 2006, 2007 Cristina Duminuco
Copyright (C) 2006, 2007 Francois du Vignaud
Copyright (C) 2006, 2007 Giorgio Facchinetti
Copyright (C) 2006, 2007 Katiuscia Manzoni
Copyright (C) 2006, 2007 Marco Bianchetti
Copyright (C) 2006 Mario Pucci
Copyright (C) 2006 Silvia Frasson

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it under the
terms of the QuantLib license.  You should have received a copy of the
license along with this program; if not, please email
<quantlib-dev@lists.sf.net>. The license is also available online at
<http://quantlib.org/license.shtml>

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.])

AC_PROG_INSTALL

# Default compilation variables

#AC_SUBST([CXXFLAGS],[${CXXFLAGS='-g -O2 -Wall'}])

# Continue setup

#AC_PROG_CC
#AC_PROG_CXX
#AC_PROG_CPP
#AC_PROG_LIBTOOL
#AC_LANG([C++])

# confirm existence of dependencies

#AC_CHECK_HEADER([log4cxx/logger.h],[],
#    [AC_MSG_ERROR([ObjectHandler depends on log4cxx which cannot be found])])

# Check for tools needed for building documentation
AC_PATH_PROG([DOXYGEN], [doxygen])
AC_PATH_PROG([SED], [sed])

# done, output the configured files
AC_CONFIG_FILES([
    Docs/Makefile
    Makefile ])
AC_OUTPUT

