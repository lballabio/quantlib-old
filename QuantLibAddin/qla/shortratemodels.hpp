
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

#include <ql/ShortRateModels/OneFactorModels/vasicek.hpp>
#include <ql/ShortRateModels/OneFactorModels/hullwhite.hpp>

namespace QuantLibAddin {

    class AffineModel : public ObjHandler::Object {
      public:
        AffineModel(const boost::shared_ptr < InstanceName > &instanceName) : ObjHandler::Object(instanceName) {}
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(model_);
        }
      protected:
        boost::shared_ptr<QuantLib::AffineModel> model_;
    };

    class Vasicek : public AffineModel {
      public:
        Vasicek(
            const boost::shared_ptr < InstanceName > &instanceName,
            const double &a,
            const double &b,
            const double &lambda,
            const double &sigma);
    };

    class HullWhite : public AffineModel {
      public:
        HullWhite(
            const boost::shared_ptr < InstanceName > &instanceName,
            const std::string &handleTermStructure,
            const double &a,
            const double &sigma);
    };

}

#endif

