
/*
 Copyright (C) 2004 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qladdin_autolink_h
#define qladdin_autolink_h

#include <boost/config.hpp>
#include <QuantLibAddin/qladdindefines.hpp>

// select toolset:
#if (_MSC_VER < 1200)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1200)
#  define QLADDIN_LIB_TOOLSET "vc6"
#elif (_MSC_VER == 1300)
#  define QLADDIN_LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define QLADDIN_LIB_TOOLSET "vc71"
#else
#  define QLADDIN_LIB_TOOLSET "vc" BOOST_STRINGIZE(_MSC_VER)
#endif


/*** libraries to be linked ***/

// select thread opt:
#ifdef _MT
#  define QLADDIN_LIB_THREAD_OPT "-mt"
#else
#  define QLADDIN_LIB_THREAD_OPT
#endif

// select linkage opt:
#ifdef _DLL
#  if defined(_DEBUG)
#    define QLADDIN_LIB_RT_OPT "-gd"
#  else
#    define QLADDIN_LIB_RT_OPT
#  endif
#else
#  if defined(_DEBUG)
#    define QLADDIN_LIB_RT_OPT "-sgd"
#  else
#    define QLADDIN_LIB_RT_OPT "-s"
#  endif
#endif

#define QLADDIN_LIB_NAME "QuantLibAddin-" QLADDIN_LIB_TOOLSET QLADDIN_LIB_THREAD_OPT QLADDIN_LIB_RT_OPT "-" QLADDIN_LIB_VERSION ".lib"

#pragma comment(lib, QLADDIN_LIB_NAME)
#ifdef BOOST_LIB_DIAGNOSTIC
#  pragma message("Linking to lib file: " QLADDIN_LIB_NAME)
#endif

#endif
