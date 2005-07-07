
/*
 Copyright (C) 2005 Aurelien Chanudet

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

#ifdef HAVE_CONFIG_H
#include <qla/config.hpp>
#endif
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>
#include <ql/termstructure.hpp>

namespace QuantLibAddin {
    
    Vasicek::Vasicek(ObjHandler::ArgumentStack& arguments) {
        double sigma    = OH_POP_ARGUMENT(double, arguments);
        double lambda   = OH_POP_ARGUMENT(double, arguments);
        double b        = OH_POP_ARGUMENT(double, arguments);
        double a        = OH_POP_ARGUMENT(double, arguments);
        
        model_ = boost::shared_ptr<QuantLib::Vasicek>(
            new QuantLib::Vasicek(a, b, lambda, sigma));
    }
    
    HullWhite::HullWhite(ObjHandler::ArgumentStack& arguments) {
        double sigma                        = OH_POP_ARGUMENT(double, arguments);
        double a                            = OH_POP_ARGUMENT(double, arguments);
        std::string handleTermStructure     = OH_POP_ARGUMENT(std::string, arguments);
        
        boost::shared_ptr<YieldTermStructure> termStructure =
            OH_GET_OBJECT(YieldTermStructure, handleTermStructure);
        if (!termStructure)
            QL_FAIL("HullWhite: error retrieving object " + handleTermStructure);
        
        boost::shared_ptr<QuantLib::YieldTermStructure> termStructureP =
            OH_GET_REFERENCE(QuantLib::YieldTermStructure, termStructure);
        
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);
                
        model_ = boost::shared_ptr<QuantLib::HullWhite>(
            new QuantLib::HullWhite(termStructureH, a, sigma));
    }
    
}

