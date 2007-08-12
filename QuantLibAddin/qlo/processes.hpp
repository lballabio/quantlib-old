
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#ifndef qla_processes_hpp
#define qla_processes_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/termstructures/volatilities/equityfx/blackconstantvol.hpp>
#include <ql/termstructures/volatilities/equityfx/blackvariancesurface.hpp>

namespace QuantLib {
    class GeneralizedBlackScholesProcess;
    class BlackVolTermStructure;
    class DayCounter;
    class Date;
}

namespace QuantLibAddin {

    class GeneralizedBlackScholesProcess : public ObjectHandler::LibraryObject<QuantLib::GeneralizedBlackScholesProcess> {
      public:
        GeneralizedBlackScholesProcess(
            const boost::shared_ptr<QuantLib::BlackVolTermStructure>& blackVolTermStructureP,
            QuantLib::Real underlying,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Date& settlementDate,
            QuantLib::Real riskFreeRate,
            QuantLib::Spread dividendYield);
    };

}

#endif
