# Process this file with autoconf to produce a configure script.
AC_INIT([QuantLib-SWIG], [1.0b3],
        [quantlib-dev@lists.sourceforge.net],
        [QuantLib-SWIG])
AC_PREREQ(2.50)
AC_CONFIG_AUX_DIR([config])
AM_INIT_AUTOMAKE([foreign])

AC_COPYRIGHT([Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl

This file is part of QuantLib, a free-software/open-source library
for financial quantitative analysts and developers - http://quantlib.org/

QuantLib is free software: you can redistribute it and/or modify it
under the terms of the QuantLib license.  You should have received a
copy of the license along with this program; if not, please email
<quantlib-dev@lists.sf.net>. The license is also available online at
<http://quantlib.org/license.shtml>.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the license for more details.])

# check target
AC_MSG_CHECKING([system])
target=`uname`
AC_MSG_RESULT([$target])

# check compiler
AC_LANG([C++])

QL_CHECK_CXXFLAGS

# check for QuantLib
AC_MSG_CHECKING([for QuantLib])
ql_version=`quantlib-config --version`
AC_MSG_RESULT([$ql_version])

# check for tools
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

AC_PATH_PROGS([MCS], [gmcs mcs gmcs2])
AM_CONDITIONAL(HAVE_MCS, test "x${MCS}" != "x")

AC_PATH_PROG([OCAMLC], [ocamlc])
AM_CONDITIONAL(HAVE_OCAML, test "x${OCAMLC}" != "x")

AC_PATH_PROG([R], [R])
AM_CONDITIONAL(HAVE_R, test "x${R}" != "x")

AC_PATH_PROG([JAVAC], [javac])
AC_PATH_PROG([JAR], [jar])
AC_PATH_PROG([JAVA], [java])
AM_CONDITIONAL(HAVE_JAVA, test "x${JAVAC}" != "x" && test "x${JAR}" != "x")

AC_ARG_WITH([jdk-include],
            AC_HELP_STRING([--with-jdk-include=INCLUDE_PATH],
                           [Supply the location of JDK header files]),
            [ql_jdk_include_path="`cd ${withval} 2>/dev/null && pwd`"],
            [ql_jdk_include_path=""])
if test [ -n "$ql_jdk_include_path" ] ; then
   AC_SUBST([JDK_INCLUDE],["-I${ql_jdk_include_path}"])
fi
AC_ARG_WITH([jdk-system-include],
            AC_HELP_STRING([--with-jdk-system-include=INCLUDE_PATH],
                           [Supply the location of system-dependent JDK header files]),
            [ql_jdk_sys_include_path="`cd ${withval} 2>/dev/null && pwd`"],
            [ql_jdk_sys_include_path=""])
if test [ -n "$ql_jdk_sys_include_path" ] ; then
   AC_SUBST([JDK_SYSTEM_INCLUDE],["-I${ql_jdk_sys_include_path}"])
fi


# flags

case "$target" in
Darwin)
    AC_SUBST([SHARED_LIB],[${SHARED_LIB='-dynamiclib'}])
	AC_SUBST([JNILIB_EXTENSION],[${JNILIB_EXTENSION='jnilib'}]) ;;
*)
    AC_SUBST([SHARED_LIB],[${SHARED_LIB='-shared'}])
	AC_SUBST([JNILIB_EXTENSION],[${JNILIB_EXTENSION='so'}]) ;;
esac


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
AC_OUTPUT

