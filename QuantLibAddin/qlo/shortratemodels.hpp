/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_shortratemodels_hpp
#define qla_shortratemodels_hpp

#include <qlo/termstructures.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    template <class T>
    class Handle;

    class AffineModel;
}

namespace QuantLibAddin {

    OH_LIB_CLASS(AffineModel, QuantLib::AffineModel);

    class Vasicek : public AffineModel {
      public:
          Vasicek(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  QuantLib::Real a,
                  QuantLib::Real b,
                  QuantLib::Real lambda,
                  QuantLib::Real sigma,
                  bool permanent);
    };

    class HullWhite : public AffineModel {
      public:
        HullWhite(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                  QuantLib::Real a,
                  QuantLib::Real sigma,
                  bool permanent);
    };

}

#endif

