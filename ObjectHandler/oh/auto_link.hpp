/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2004 Ferdinando Ametrano

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

#ifndef oh_autolink_hpp
#define oh_autolink_hpp

#include <oh/ohdefines.hpp>

// select toolset:
#if (_MSC_VER < 1300)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1300)
#  define OBJHANDLER_LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define OBJHANDLER_LIB_TOOLSET "vc71"
#elif (_MSC_VER == 1400)
#  define OBJHANDLER_LIB_TOOLSET "vc80"
#elif (_MSC_VER == 1500)
#  define OBJHANDLER_LIB_TOOLSET "vc90"
#elif (_MSC_VER == 1600)
#  define OBJHANDLER_LIB_TOOLSET "vc100"
#else
#  error "unknown Microsoft compiler"
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

// select OH lib
#if defined(XLL_STATIC)
#  define OBJHANDLER_LIB_TYPE "-xllib"
#elif defined(XLL_IMPORTS)
#  define OBJHANDLER_LIB_TYPE "-xll"
#else
#  define OBJHANDLER_LIB_TYPE ""
#endif


#define OBJHANDLER_LIB_NAME "ObjectHandler" OBJHANDLER_LIB_TYPE "-" OBJHANDLER_LIB_TOOLSET OBJHANDLER_LIB_THREAD_OPT OBJHANDLER_LIB_RT_OPT "-" OBJHANDLER_LIB_VERSION ".lib"

#pragma comment(lib, OBJHANDLER_LIB_NAME)
#ifdef BOOST_LIB_DIAGNOSTIC
#  pragma message("Will (need to) link to lib file: " OBJHANDLER_LIB_NAME)
#endif

#endif

