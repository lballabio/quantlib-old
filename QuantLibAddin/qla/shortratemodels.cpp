
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
#include <qla/optionutils.hpp>
#include <qla/shortratemodels.hpp>
#include <qla/termstructures.hpp>

#include <ql/termstructure.hpp>

namespace QuantLibAddin {
    
    Vasicek::Vasicek(ObjHandler::ArgStack& args) {
        double sigma = ObjHandler::Args<double>::popArg(args);
        double lambda = ObjHandler::Args<double>::popArg(args);
        double b = ObjHandler::Args<double>::popArg(args);
        double a = ObjHandler::Args<double>::popArg(args);
        
        model_ = boost::shared_ptr<QuantLib::Vasicek>(
            new QuantLib::Vasicek(a, b, lambda, sigma));
    }
    
    HullWhite::HullWhite(ObjHandler::ArgStack& args) {
        double sigma = ObjHandler::Args<double>::popArg(args);
        double a = ObjHandler::Args<double>::popArg(args);
        std::string handleTermStructure = ObjHandler::Args<std::string>::popArg(args);
        
        boost::shared_ptr<YieldTermStructure> termStructure =
            boost::dynamic_pointer_cast<YieldTermStructure>(
                QL_OBJECT_GET(handleTermStructure));
        if (!termStructure)
            QL_FAIL("HullWhite: error retrieving object " + handleTermStructure);
        
        boost::shared_ptr<QuantLib::YieldTermStructure> termStructureP =
            boost::static_pointer_cast<QuantLib::YieldTermStructure>(
                termStructure->getReference());
        
        QuantLib::Handle<QuantLib::YieldTermStructure> termStructureH(termStructureP);
                
        model_ = boost::shared_ptr<QuantLib::HullWhite>(
            new QuantLib::HullWhite(termStructureH, a, sigma));
    }
    
}