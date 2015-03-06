/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/cmsmarketcalibration.hpp>
#include <qlo/cmsmarket.hpp>
#include <qlo/swaptionvolstructure.hpp>

#include <boost/timer.hpp>

using boost::shared_ptr;
using ObjectHandler::LibraryObject;
using QuantLib::EndCriteria;
using QuantLib::OptimizationMethod;

namespace QuantLibAddin {

     CmsMarketCalibration::CmsMarketCalibration(
        const shared_ptr<ObjectHandler::ValueObject>& properties,
        QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& volCube,
        shared_ptr<QuantLib::CmsMarket>& cmsMarket,
        const QuantLib::Matrix& weights,
        QuantLib::CmsMarketCalibration::CalibrationType calibrationType,
        bool permanent)
    : LibraryObject<QuantLib::CmsMarketCalibration>(properties, permanent) {
        
        libraryObject_ = shared_ptr<QuantLib::CmsMarketCalibration>(new
            QuantLib::CmsMarketCalibration(volCube,
                                           cmsMarket,
                                           weights,
                                           calibrationType));         
     }
        
    std::vector<std::vector<ObjectHandler::property_t> >
    CmsMarketCalibration::getSparseSabrParameters() {
        return getSabrParameters(libraryObject_->sparseSabrParameters_);
    }

    std::vector<std::vector<ObjectHandler::property_t> >
    CmsMarketCalibration::getDenseSabrParameters() {
        return getSabrParameters(libraryObject_->denseSabrParameters_);
    }

    std::vector<std::vector<ObjectHandler::property_t> >
    CmsMarketCalibration::getCmsMarket() {
       return browseCmsMarket(libraryObject_->browseCmsMarket_);
    }

   QuantLib::Array
   CmsMarketCalibration::compute(const shared_ptr<EndCriteria>& endCriteria,
                                 const shared_ptr<OptimizationMethod>& method,
                                 const QuantLib::Array& guess,
                                 bool isMeanReversionFixed) {
        boost::timer t;
        t.restart();
        QuantLib::Array result = libraryObject_->compute(endCriteria,
                                method,
                                guess,
                                isMeanReversionFixed);
        elapsed_ = t.elapsed();
        return result;
   }

}
