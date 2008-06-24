
/*  
 Copyright (C) 2008 Nazcatech sprl Belgium
 Copyright (C) 2008 Plamen Neykov
 
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

#include <qlo/serialization/processor.hpp>
#include <qlo/handle.hpp>
#include <qlo/pricingengines.hpp>
#include <qlo/leg.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/conversions/varianttodate.hpp>
#include <qlo/extrapolator.hpp>
#include <qlo/baseinstruments.hpp>
#include <qlo/index.hpp>

namespace QuantLibAddin {

    std::string InstrumentProcessor::process(const ObjectHandler::SerializationFactory& factory,
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {
        
        ObjectHandler::StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
        boost::shared_ptr<Instrument> instrument =
            boost::dynamic_pointer_cast<Instrument>(object.second);
        if (instrument && valueObject->hasProperty("EngineID")) {
            std::string pricingEngineID = ObjectHandler::convert2<std::string>(
                valueObject->getProperty("EngineID"), "EngineID");
            OH_GET_OBJECT(pricingEngineObjPtr, pricingEngineID, QuantLibAddin::PricingEngine)
            instrument->setPricingEngine(pricingEngineObjPtr);
        }
                
        return object.first;
    }

    std::string RelinkableHandleProcessor::process(const ObjectHandler::SerializationFactory& factory,
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {

        ObjectHandler::StrObjectPair link;

        link.first = boost::get<std::string>(valueObject->getProperty("CurrentLink"));
        valueObject->setProperty("CurrentLink", std::string(""));

        ObjectHandler::StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
        
        link.second = object.second;
        handles.push_back(link);

        return object.first;
    }

    void RelinkableHandleProcessor::postProcess() const {
        for(ObjectHandler::HandlesList::iterator i = handles.begin(); i != handles.end(); ++i) {
            boost::dynamic_pointer_cast<QuantLibAddin::RelinkableHandle>(i->second)->linkTo(i->first);
        }
        handles.clear();
    }

    std::string LegProcessor::process(const ObjectHandler::SerializationFactory& factory,
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {
        
        ObjectHandler::StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
        boost::shared_ptr<Leg> inst = boost::dynamic_pointer_cast<Leg>(object.second);
        if (inst && valueObject->hasProperty("UserLegIDs")) {
            std::vector<boost::shared_ptr<QuantLibAddin::FloatingRateCouponPricer> > legs2;
            std::vector<std::string> legs =
                ObjectHandler::vector::convert2<std::string>(valueObject->getProperty("UserLegIDs"), "UserLegIDs");
            for (std::vector<std::string>::const_iterator i = legs.begin(); i!= legs.end(); ++i) {
                OH_GET_OBJECT(leg, *i, QuantLibAddin::FloatingRateCouponPricer)
                legs2.push_back(leg);
            }
            inst->setCouponPricers(legs2);
        }
                
        return object.first;
    }

    std::string IndexProcessor::process(const ObjectHandler::SerializationFactory& factory,
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {
        
        ObjectHandler::StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
        boost::shared_ptr<Index> index = boost::dynamic_pointer_cast<Index>(object.second);
        if(index) {
            try {
                ObjectHandler::property_t dates = valueObject->getProperty("IndexFixingDates");
                ObjectHandler::property_t fixings = valueObject->getProperty("IndexFixingRates");
                std::vector<QuantLib::Date> vct_dates = 
                    ObjectHandler::vector::convert2<QuantLib::Date>(dates, "IndexFixingDates");
                std::vector<QuantLib::Rate> vct_fixings =
                    ObjectHandler::vector::convert2<QuantLib::Rate>(fixings, "IndexFixingRates");
                index->addFixings(vct_dates, vct_fixings, true, false);
            }
            catch(const std::exception& ) {}
        }
                
        return object.first;
    }


    std::string ExtrapolatorProcessor::process(const ObjectHandler::SerializationFactory& factory,
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
        bool overwriteExisting) const {
        ObjectHandler::StrObjectPair object = factory.createObject(valueObject, overwriteExisting);
        boost::shared_ptr<Extrapolator> extrapolator =
            boost::dynamic_pointer_cast<Extrapolator>(object.second);
        if (extrapolator && valueObject->hasProperty("UserExtrapolation")) {
            bool extrapolation = ObjectHandler::convert2<bool>(
                valueObject->getProperty("UserExtrapolation"), "UserExtrapolation");
            extrapolator->enableExtrapolation(extrapolation);
        }
                
        return object.first;
    }

}
