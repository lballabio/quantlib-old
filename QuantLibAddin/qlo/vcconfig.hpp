
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

/*
Visual Studio configuration - format symbol COMPILER_STRING
for runtime version info.
*/

#ifndef qla_vcconfig_hpp
#define qla_vcconfig_hpp

#if (_MSC_VER == 1300)
#  define VC_VERSION "7.0"
#elif (_MSC_VER == 1310)
#  define VC_VERSION "7.1"
#elif (_MSC_VER == 1400)
#  define VC_VERSION "8.0"
#else
#  define VC_VERSION "(version unknown)"
#endif

#ifdef _MT
#  define RUNTIME_THREAD "Multithreaded"
#else
#  define RUNTIME_THREAD "Single-threaded"
#endif

#ifdef _DLL
#  define RUNTIME_LINKAGE "Dynamic"
#else
#  define RUNTIME_LINKAGE "Static"
#endif

#if defined(_DEBUG)
#  define RUNTIME_CONFIG "Debug Configuration"
#else
#  define RUNTIME_CONFIG "Release Configuration"
#endif

#define COMPILER_STRING " - MS VC++ " VC_VERSION " - " RUNTIME_THREAD " " RUNTIME_LINKAGE " Runtime library - " RUNTIME_CONFIG

#endif

