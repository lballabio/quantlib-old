
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

#ifndef qla_volatilities_hpp
#define qla_volatilities_hpp

#include <oh/objhandler.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/Volatilities/blackvariancesurface.hpp>

namespace QuantLibAddin {

    class BlackVolTermStructure : public ObjHandler::Object {
    public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(blackVolTermStructure_);
        }
    protected:
        boost::shared_ptr<QuantLib::BlackVolTermStructure> blackVolTermStructure_;
    };

    class BlackConstantVol : public BlackVolTermStructure {
    public:
        BlackConstantVol(ObjHandler::ArgumentStack &args);
    };

    class BlackVarianceSurface : public BlackVolTermStructure {
    public:
        BlackVarianceSurface(ObjHandler::ArgumentStack &args);
    };

}

#endif

