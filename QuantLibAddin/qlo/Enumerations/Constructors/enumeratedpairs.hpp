
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006 Marco Bianchetti
 Copyright (C) 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Chiara Fornarola
 Copyright (C) 2007 Katiuscia Manzoni
 Copyright (C) 2005 Plamen Neykov

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

#ifndef qla_enumpairctors_hpp
#define qla_enumpairctors_hpp

#include <qlo/qladdindefines.hpp>
#include <qlo/Enumerations/Factories/termstructuresfactory.hpp>

namespace QuantLibAddin {

    /* *** YieldTermStructures *** */

    /* *** Discount based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);

    /* *** ZeroYield based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);

    /* *** ForwardRate based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_BACKWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_FORWARDFLAT_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LOGLINEAR_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_CUBIC_PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> > &rateHelpers,
            const QuantLib::DayCounter &dayCounter);

}

#endif

