
/*
 Copyright (C) 2006 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_c_autolink_hpp
#define qla_c_autolink_hpp

#pragma warning(disable : 4996)

// select toolset:
#if (_MSC_VER < 1300)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1300)
#  define LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define LIB_TOOLSET "vc71"
#elif (_MSC_VER == 1400)
#  define LIB_TOOLSET "vc80"
#elif (_MSC_VER == 1500)
#  define LIB_TOOLSET "vc90"
#else
#  error "unknown Microsoft compiler"
#endif

/*** libraries to be linked ***/

// select thread opt:
#ifdef _MT
#  define LIB_THREAD_OPT "-mt"
#else
#  define LIB_THREAD_OPT
#endif

// select linkage opt:
#ifdef _DLL
#  if defined(_DEBUG)
#    define LIB_RT_OPT "-gd"
#  else
#    define LIB_RT_OPT
#  endif
#else
#  if defined(_DEBUG)
#    define LIB_RT_OPT "-sgd"
#  else
#    define LIB_RT_OPT "-s"
#  endif
#endif

#define OH_LIB_NAME "ObjectHandler-" LIB_TOOLSET LIB_THREAD_OPT LIB_RT_OPT "-0_9_6.lib"
#define QL_LIB_NAME "QuantLib-" LIB_TOOLSET LIB_THREAD_OPT LIB_RT_OPT "-0_9_6.lib"

#pragma message("Linking to lib file: " OH_LIB_NAME)
#pragma comment(lib, OH_LIB_NAME)
#pragma message("Linking to lib file: " QL_LIB_NAME)
#pragma comment(lib, QL_LIB_NAME)

#endif

