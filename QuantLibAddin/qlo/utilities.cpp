
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/utilities.hpp>
#include <qlo/qladdindefines.hpp>

#if defined BOOST_MSVC       // Microsoft Visual C++
#  define BOOST_LIB_DIAGNOSTIC
#  include <ql/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#  include <qlo/vcconfig.hpp>
#else
#  define COMPILER_STRING
#endif

#define VERSION_STRING "QuantLibXL " QLADDIN_VERSION
#define VERSION_STRING_VERBOSE "QuantLibXL " QLADDIN_VERSION COMPILER_STRING " - " __DATE__ " " __TIME__

namespace QuantLibAddin {

    std::string qlVersion() {
        return QL_VERSION;
    }

    std::string qlAddinVersion() {
        return QLADDIN_VERSION;
    }

    std::string qlxlVersion(bool verbose) {
        if (verbose)
            return VERSION_STRING_VERBOSE;
        else
            return VERSION_STRING;
    }

}
