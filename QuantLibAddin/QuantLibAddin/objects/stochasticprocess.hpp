
/*
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

#ifndef qla_stochasticprocess_hpp
#define qla_stochasticprocess_hpp

#include <ObjectHandler/objhandler.hpp>
#include <ql/stochasticprocess.hpp>

namespace QuantLibAddin {

    class StochasticProcess : public ObjHandler::Object {
    public:

        StochasticProcess(
            const float &underlying,
            const std::string &dayCounterID,
            const long &settlementDate,
            const float &riskFreeRate,
            const float &dividendYield,
            const float &volatility);
//        ~StochasticProcess();
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(stochasticProcess_);
        }
    private:
        boost::shared_ptr<QuantLib::BlackScholesProcess> stochasticProcess_;
    };

}

#endif

