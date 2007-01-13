
/*
 Copyright (C) 2005 Eric Ehlers
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

#ifndef xlsdk_autolink_hpp
#define xlsdk_autolink_hpp

// select toolset:
#if (_MSC_VER < 1300)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1300)
#  define XLSDK_LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define XLSDK_LIB_TOOLSET "vc71"
#elif (_MSC_VER == 1400)
#  define XLSDK_LIB_TOOLSET "vc80"
#else
#  error "unknown Microsoft compiler"
#endif

/*** libraries to be linked ***/

// select thread opt:
#ifdef _MT
#  define XLSDK_LIB_THREAD_OPT "-mt"
#else
#  define XLSDK_LIB_THREAD_OPT
#endif

// select linkage opt:
#ifdef _DLL
#  if defined(_DEBUG)
#    define XLSDK_LIB_RT_OPT "-gd"
#  else
#    define XLSDK_LIB_RT_OPT
#  endif
#else
#  if defined(_DEBUG)
#    define XLSDK_LIB_RT_OPT "-sgd"
#  else
#    define XLSDK_LIB_RT_OPT "-s"
#  endif
#endif

#define XLSDK_LIB_NAME "xlsdk-" XLSDK_LIB_TOOLSET XLSDK_LIB_THREAD_OPT XLSDK_LIB_RT_OPT ".lib"

#pragma comment(lib, XLSDK_LIB_NAME)
#ifdef BOOST_LIB_DIAGNOSTIC
#  pragma message("Linking to lib file: " XLSDK_LIB_NAME)
#endif

#endif

