
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef qla_volatilities_hpp
#define qla_volatilities_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>

#include <ql/voltermstructures/interestrate/abcdatmvolcurve.hpp>

namespace QuantLib {
    class BlackVolTermStructure;
    class Date;
    class DayCounter;
    class Matrix;
}

namespace QuantLibAddin {

    OH_LIB_CLASS(BlackVolTermStructure, QuantLib::BlackVolTermStructure)

    class BlackConstantVol : public BlackVolTermStructure {
      public:
        BlackConstantVol(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            QuantLib::Volatility volatility,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    class BlackVarianceSurface : public BlackVolTermStructure {
      public:
        BlackVarianceSurface(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Rate>& strikes,
            const QuantLib::Matrix& vols,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    class AbcdAtmVolCurve: public ObjectHandler::LibraryObject<QuantLib::AbcdAtmVolCurve> {
    public:
      AbcdAtmVolCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& cal,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > & volatilities,
            QuantLib::BusinessDayConvention bdc,
            const QuantLib::DayCounter& dc,
            bool permanent);
    };
}

#endif

