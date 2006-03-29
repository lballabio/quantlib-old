
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005 Eric Ehlers

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

#ifndef qladdin_hpp
#define qladdin_hpp

#include <qla/qladdindefines.hpp>

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors 
   (Metrowerks, for example) also #define _MSC_VER
*/
#if defined BOOST_MSVC       // Microsoft Visual C++
#  include <qla/autolink.hpp>
#endif

#include <qla/utilities.hpp>

#include <qla/capfloor.hpp>
#include <qla/couponvectors.hpp>
#include <qla/instruments.hpp>
#include <qla/interpolation.hpp>
#include <qla/options.hpp>
#include <qla/processes.hpp>
#include <qla/schedule.hpp>
#include <qla/shortratemodels.hpp>
#include <qla/simpleswap.hpp>
#include <qla/swap.hpp>
#include <qla/termstructures.hpp>
#include <qla/utilities.hpp>
#include <qla/volatilities.hpp>
#include <qla/xibor.hpp>

#endif

