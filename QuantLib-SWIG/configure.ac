# Process this file with autoconf to produce a configure script.
AC_INIT([QuantLib-SWIG], [0.3.12],
        [quantlib-dev@lists.sourceforge.net],
        [QuantLib-SWIG])
AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([Copyright (C) 2005, 2006 StatPro Italia srl

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

AC_PATH_PROG([MCS], [mcs])
AM_CONDITIONAL(HAVE_MCS, test "x${MCS}" != "x")

AC_PATH_PROG([OCAMLC], [ocamlc])
AM_CONDITIONAL(HAVE_OCAML, test "x${OCAMLC}" != "x")

AC_PATH_PROG([R], [R])
AM_CONDITIONAL(HAVE_R, test "x${R}" != "x")
AC_PATH_PROG([RSWIG], [rswig])
AM_CONDITIONAL(HAVE_RSWIG, test "x${RSWIG}" != "x")

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

