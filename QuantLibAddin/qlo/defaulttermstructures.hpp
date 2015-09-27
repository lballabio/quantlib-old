/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2009 Ferdinando Ametrano

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

#ifndef qla_defaulttermstructure_hpp
#define qla_defaulttermstructure_hpp

#include <qlo/termstructures.hpp>

#include <ql/types.hpp>
#include <ql/time/businessdayconvention.hpp>

namespace QuantLib {

    class Calendar;
    class DayCounter;
    class Period;
    class Quote;
    class Date;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {

    OH_OBJ_CLASS(HazardRateStructure, DefaultProbabilityTermStructure);

    class FlatHazardRate : public HazardRateStructure {
      public:
        FlatHazardRate(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::Handle<QuantLib::Quote>& hazardRate,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    OH_OBJ_CLASS(DefaultDensityStructure, DefaultProbabilityTermStructure);

    class BaseCorrelationTermStructure : public CorrelationTermStructure {
      public:
        BaseCorrelationTermStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,

            const std::string& interpolType,
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention bdc,
            const std::vector<QuantLib::Period>& tenors,
            const std::vector<QuantLib::Real>& lossLevel,
            const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& correls,
            const QuantLib::DayCounter& dayCounter,

            bool permanent);
        QuantLib::Real correlation(const QuantLib::Date& d, QuantLib::Real lossLevel);
        const std::string& interpolType() const {return interpolType_;}
    private:
        // to be used in the casting to the QL object methods, alternatively use the same
        //   technique as in QLAddIn::InterpolatedYieldCurve
        const std::string interpolType_; // or a local enumerator
    };

    // Bootstrap real world probabilities to a hazard rate curve.
    QuantLib::Real probabilityToHazardRate(
        QuantLib::Probability p, 
        const QuantLib::Date& d,
        const QuantLib::DayCounter& dc);

}

#endif
