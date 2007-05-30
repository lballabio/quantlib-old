
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

#ifndef qla_forwardvanillaoption_hpp
#define qla_forwardvanillaoption_hpp

#include <qlo/baseinstruments.hpp>
#include <qlo/processes.hpp>
#include <ql/instruments/forwardvanillaoption.hpp>

namespace QuantLibAddin {

    class ForwardVanillaOption : public OneAssetOption {
    public:
        ForwardVanillaOption(
            const double& moneyness,
            QuantLib::Date resetDate,
            const boost::shared_ptr < QuantLib::GeneralizedBlackScholesProcess > &blackScholesProcess,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff> &payoff,
            const boost::shared_ptr < QuantLib::Exercise > &exercise,
            const boost::shared_ptr<QuantLib::PricingEngine> &pricingEngine);
    };

}

#endif

