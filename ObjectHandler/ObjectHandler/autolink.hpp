
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

#ifndef objecthandler_autolink_h
#define objecthandler_autolink_h

#include <ObjectHandler/objecthandlerversion.hpp>

// select toolset:
#if (_MSC_VER < 1200)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1200)
#  define OBJHANDLER_LIB_TOOLSET "vc6"
#elif (_MSC_VER == 1300)
#  define OBJHANDLER_LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define OBJHANDLER_LIB_TOOLSET "vc71"
#else
#  define OBJHANDLER_LIB_TOOLSET "vc" BOOST_STRINGIZE(_MSC_VER)
#endif


/*** libraries to be linked ***/

// select thread opt:
#ifdef _MT
#  define OBJHANDLER_LIB_THREAD_OPT "-mt"
#else
#  define OBJHANDLER_LIB_THREAD_OPT
#endif

// select linkage opt:
#ifdef _DLL
#  if defined(_DEBUG)
#    define OBJHANDLER_LIB_RT_OPT "-gd"
#  else
#    define OBJHANDLER_LIB_RT_OPT
#  endif
#else
#  if defined(_DEBUG)
#    define OBJHANDLER_LIB_RT_OPT "-sgd"
#  else
#    define OBJHANDLER_LIB_RT_OPT "-s"
#  endif
#endif

#define OBJHANDLER_LIB_NAME "ObjectHandler-" OBJHANDLER_LIB_TOOLSET OBJHANDLER_LIB_THREAD_OPT OBJHANDLER_LIB_RT_OPT "-" OBJHANDLER_LIB_VERSION ".lib"

#pragma comment(lib, OBJHANDLER_LIB_NAME)
#ifdef BOOST_LIB_DIAGNOSTIC
#  pragma message("Linking to lib file: " OBJHANDLER_LIB_NAME)
#endif

#endif
