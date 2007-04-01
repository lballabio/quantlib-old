
/*
 Copyright (C) 2005 Eric Ehlers
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

#ifndef qla_shortratemodels_hpp
#define qla_shortratemodels_hpp

#include <oh/objhandler.hpp>
#include <qlo/termstructures.hpp>
#include <ql/shortratemodels/onefactormodels/vasicek.hpp>
#include <ql/shortratemodels/onefactormodels/hullwhite.hpp>

namespace QuantLibAddin {

    class AffineModel : public ObjHandler::LibraryObject<QuantLib::AffineModel> {
    };

    class Vasicek : public AffineModel {
      public:
        Vasicek(
            const double &a,
            const double &b,
            const double &lambda,
            const double &sigma);
    };

    class HullWhite : public AffineModel {
      public:
        HullWhite(
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const double &a,
            const double &sigma);
    };

}

#endif

