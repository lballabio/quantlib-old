
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

#ifndef qla_cliquetoption_hpp
#define qla_cliquetoption_hpp

#include <qla/objects/stochasticprocess.hpp>
#include <ql/Instruments/cliquetoption.hpp>

namespace QuantLibAddin {

    class CliquetOption : public ObjHandler::Object {
    public:
        CliquetOption::CliquetOption(
            const boost::shared_ptr<StochasticProcess> &stochasticProcess,
            const std::vector < long > &resetDatesLong,
            const std::string &optionTypeID,
            const float &strike,
            const long &exerciseDate,
            const std::string &engineID,
            const long &timeSteps);
    //    ~CliquetOption();
        void setEngine(
            const std::string &engineName,
            const long &timeSteps);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(cliquetOption_);
        }
    private:
        boost::shared_ptr<QuantLib::CliquetOption> cliquetOption_;
    };

}

#endif

