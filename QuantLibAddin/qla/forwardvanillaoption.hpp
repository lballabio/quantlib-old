
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

#ifndef qla_forwardvanillaoption_hpp
#define qla_forwardvanillaoption_hpp

#include <qla/stochasticprocess.hpp>
#include <ql/Instruments/forwardvanillaoption.hpp>

namespace QuantLibAddin {

    class ForwardVanillaOption : public ObjHandler::Object {
    public:
        ForwardVanillaOption(va_list list);
    //    ~ForwardVanillaOption();
        void setEngine(
            const std::string &engineName,
            const long &timeSteps);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(forwardVanillaOption_);
        }
    private:
        boost::shared_ptr<QuantLib::ForwardVanillaOption> forwardVanillaOption_;
    };

}

#endif

