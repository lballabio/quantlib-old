
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2010 Eric Ehlers

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

#ifndef log4cxx_autolink_hpp
#define log4cxx_autolink_hpp

// select toolset:
#if (_MSC_VER < 1200)
#  error "unsupported Microsoft compiler"
#elif (_MSC_VER == 1200)
#  define LOG4CXX_LIB_TOOLSET "vc6"
#elif (_MSC_VER == 1300)
#  define LOG4CXX_LIB_TOOLSET "vc7"
#elif (_MSC_VER == 1310)
#  define LOG4CXX_LIB_TOOLSET "vc71"
#elif (_MSC_VER == 1400)
#  define LOG4CXX_LIB_TOOLSET "vc80"
#elif (_MSC_VER == 1500)
#  define LOG4CXX_LIB_TOOLSET "vc90"
#elif (_MSC_VER == 1600)
#  define LOG4CXX_LIB_TOOLSET "vc100"
#elif (_MSC_VER == 1700)
#  define LOG4CXX_LIB_TOOLSET "vc110"
#else
#  error "unknown Microsoft compiler"
#endif

/*** libraries to be linked ***/

// select thread opt:
#ifdef _MT
#  define LOG4CXX_LIB_THREAD_OPT "-mt"
#else
#  define LOG4CXX_LIB_THREAD_OPT
#endif

// select linkage opt:
#ifdef _DLL
#  if defined(_DEBUG)
#    define LOG4CXX_LIB_RT_OPT "-gd"
#  else
#    define LOG4CXX_LIB_RT_OPT
#  endif
#else
#  if defined(_DEBUG)
#    define LOG4CXX_LIB_RT_OPT "-sgd"
#  else
#    define LOG4CXX_LIB_RT_OPT "-s"
#  endif
#endif

#define LOG4CXX_CURRENT_VERSION "-0_10_0"

#define LOG4CXX_LIB_NAME "log4cxxs-" LOG4CXX_LIB_TOOLSET LOG4CXX_LIB_THREAD_OPT LOG4CXX_LIB_RT_OPT LOG4CXX_CURRENT_VERSION ".lib"
#define APR_LIB_NAME "apr-" LOG4CXX_LIB_TOOLSET LOG4CXX_LIB_THREAD_OPT LOG4CXX_LIB_RT_OPT LOG4CXX_CURRENT_VERSION ".lib"
#define APRUTIL_LIB_NAME "aprutil-" LOG4CXX_LIB_TOOLSET LOG4CXX_LIB_THREAD_OPT LOG4CXX_LIB_RT_OPT LOG4CXX_CURRENT_VERSION ".lib"

#pragma comment(lib, LOG4CXX_LIB_NAME)
#pragma comment(lib, APR_LIB_NAME)
#pragma comment(lib, APRUTIL_LIB_NAME)

#ifdef BOOST_LIB_DIAGNOSTIC
#  pragma message("Linking to lib file: " LOG4CXX_LIB_NAME)
#  pragma message("Linking to lib file: " APR_LIB_NAME)
#  pragma message("Linking to lib file: " APRUTIL_LIB_NAME)
#endif

#endif

