
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef qla_basketoption_hpp
#define qla_basketoption_hpp

#include <qla/processes.hpp>
#include <ql/Instruments/basketoption.hpp>

namespace QuantLibAddin {

    class BasketOption : public ObjHandler::Object {
    public:
        BasketOption(
            const std::vector < std::string > &handleBlackScholesVector,
            const std::string &basketID,
            const std::vector < std::vector < double > > &correlations,
            const std::string &optionTypeID,
            const double &strike,
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engineID,
            const long &timeSteps);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(basketOption_);
        }
    private:
        boost::shared_ptr<QuantLib::BasketOption> basketOption_;
    };

}

#endif

