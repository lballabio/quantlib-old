
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_termstructures_hpp
#define qla_termstructures_hpp

#include <oh/objecthandler.hpp>
#include <qlo/interpolation.hpp>
#include <ql/time/frequency.hpp>
#include <ql/compounding.hpp>
#include <ql/handle.hpp>

namespace QuantLib {
    class Calendar;
    class YieldTermStructure;
    class DayCounter;
    class Date;
}

namespace QuantLibAddin {
     
    class TermStructure : public Extrapolator {};

    class YieldTermStructure : public TermStructure {};

    class PiecewiseYieldCurve : public YieldTermStructure {
      public:
        PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<std::string>& handlesRateHelper,
            const QuantLib::DayCounter& dayCounter,
            const std::string& traitsID,
            const std::string& interpolatorID,
            QuantLib::Real accuracy);
    };

    class DiscountCurve : public YieldTermStructure {
      public:
        DiscountCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector<double> &dfs,
            const QuantLib::DayCounter &dayCounter);
    };

    class ZeroCurve : public YieldTermStructure {
      public:
        ZeroCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &zeroRates,
            const QuantLib::DayCounter &dayCounter);
    };

    class ForwardCurve : public YieldTermStructure {
      public:
        ForwardCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &forwardRates,
            const QuantLib::DayCounter &dayCounter);
    };

    class FlatForward : public YieldTermStructure {
      public:
        FlatForward(const long &nDays,
                    const QuantLib::Calendar &calendar,
                    QuantLib::Rate forward,
                    const QuantLib::DayCounter &dayCounter,
                    QuantLib::Compounding compounding,
                    QuantLib::Frequency frequency);
    };

    class ForwardSpreadedTermStructure : public YieldTermStructure {
      public:
        ForwardSpreadedTermStructure(
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const double &spread);
    };

    class ImpliedTermStructure : public YieldTermStructure {
      public:
        ImpliedTermStructure(
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Date& referenceDate);
    };

}

#endif
