
/*  
 Copyright (C) 2007 Eric Ehlers
 
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

// This file was generated automatically by gensrc.py.
// Editing this file manually is not recommended.

#include <fstream>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/vector.hpp>
#include <Addins/Cpp/Serialization/serializationfactory.hpp>
#include <Addins/Cpp/Serialization/serialization_all.hpp>
#include <oh/repository.hpp>
#include <boost/filesystem.hpp>

namespace QuantLibAddinCpp {

    SerializationFactory &SerializationFactory::instance() {
        if (instance_) {
            SerializationFactory *ret = dynamic_cast<SerializationFactory*>(instance_);
            if (ret) return *ret;
        }
        OH_FAIL("Attempt to reference uninitialized SerializationFactory object");
    }

    template<class Archive>
    void tpl_register_classes(Archive& ar) {
    
            // ObjectHandler

            register_oh(ar);
    
            // Abcd

            register_abcd(ar);

            // Accounting Engines

            register_accountingengines(ar);

            // AlphaForm

            register_alphaform(ar);

            // Asset Swap

            register_assetswap(ar);

            // Bonds

            register_bonds(ar);

            // Brownian Generator

            register_browniangenerators(ar);

            // Caps/Floors

            register_capfloor(ar);

            // Caplet Volatility Term Structures

            register_capletvolstructure(ar);

            // Cms Market

            register_cmsmarket(ar);

            // Cms Market Calibration

            register_cmsmarketcalibration(ar);

            // correlation

            register_correlation(ar);

            // Coupon Vectors

            register_couponvectors(ar);

            // Ctsmmcapletcalibration

            register_ctsmmcapletcalibration(ar);

            // CurveState

            register_curvestate(ar);

            // Driftcalculators

            register_driftcalculators(ar);

            // Evolution Description

            register_evolutiondescription(ar);

            // Exercise

            register_exercise(ar);

            // Forward Rate Agreement

            register_forwardrateagreement(ar);

            // Indices

            register_index(ar);

            // Interpolation

            register_interpolation(ar);

            // Leg

            register_leg(ar);

            // Market Model Evolvers

            register_marketmodelevolvers(ar);

            // MarketModels

            register_marketmodels(ar);

            // Math

            register_mathf(ar);

            // Optimization

            register_optimization(ar);

            // Options

            register_options(ar);

            // Payoffs

            register_payoffs(ar);

            // Pricing Engines

            register_pricingengines(ar);

            // Processes

            register_processes(ar);

            // Products

            register_products(ar);

            // Quotes

            register_quotes(ar);

            // Random Sequence Generator

            register_randomsequencegenerator(ar);

            // Range Accrual

            register_rangeaccrual(ar);

            // RateHelper

            register_ratehelpers(ar);

            // Schedules

            register_schedule(ar);

            // SequenceStatistics

            register_sequencestatistics(ar);

            // Short Rate Models

            register_shortratemodels(ar);

            // SimpleCashFlow

            register_simplecashflow(ar);

            // Smile Section Structures

            register_smilesection(ar);

            // Statistics

            register_statistics(ar);

            // Swap

            register_swap(ar);

            // Swaption

            register_swaption(ar);

            // Swaption Volatility Term Structures

            register_swaptionvolstructure(ar);

            // Term Structures

            register_termstructures(ar);

            // Vanilla Swap

            register_vanillaswap(ar);

            // Volatilities

            register_volatilities(ar);

            // Market Model Volatility

            register_volatility(ar);


    }

    void register_in(boost::archive::xml_iarchive& ia) {
        
        tpl_register_classes(ia);
    }

    void register_out(boost::archive::xml_oarchive& oa) {
        
        tpl_register_classes(oa);
    }


    void SerializationFactory::saveObject(
        const boost::shared_ptr<ObjectHandler::Object> &object,
        const char *path) const {

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa);
        oa << boost::serialization::make_nvp("oh_object", object->properties());
    }

    void SerializationFactory::saveObject(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
        const char *path) const {

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa);
        std::vector<boost::shared_ptr<ObjectHandler::Object> >::const_iterator i;
        for (i=objectList.begin(); i!=objectList.end(); ++i){
            // We need to supply a name for the object to be serialized.
            // The objectID isn't suitable because certain values of objectID are
            // invalid as XML tags e.g. values beginning with numeric characters.
            // For our purposes the tag is ignored, so just supply a dummy string.
            //std::string objectID = boost::any_cast<std::string>(valueObject->getProperty("objectID"));
            //oa << boost::serialization::make_nvp(objectID.c_str(), valueObject);
            oa << boost::serialization::make_nvp("oh_object", (*i)->properties());
        }
    }

    void SerializationFactory::saveObject2(
        const std::vector<boost::shared_ptr<ObjectHandler::Object> >& objectList,
        const char *path,
        bool forceOverwrite) const {

        OH_REQUIRE(forceOverwrite || !boost::filesystem::exists(path),
            "Cannot overwrite output file : " << path);

        std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
        std::vector<boost::shared_ptr<ObjectHandler::Object> >::const_iterator i;
        for (i=objectList.begin(); i!=objectList.end(); ++i)
            valueObjects.push_back((*i)->properties());

        std::ofstream ofs(path);
        boost::archive::xml_oarchive oa(ofs);
        register_out(oa);
        oa << boost::serialization::make_nvp("object_list", valueObjects);
    }

    boost::shared_ptr<ObjectHandler::Object> SerializationFactory::loadObject(
        const std::string &objectID,
        const char *path) const {

        std::ifstream ifs(path);
        boost::archive::xml_iarchive ia(ifs);
        register_in(ia);
        boost::shared_ptr<ObjectHandler::ValueObject> valueObject;
        ia >> boost::serialization::make_nvp(objectID.c_str(), valueObject);
        // This VO has picked up the ID of the old VO that was deserialized.
        // Override this value with the new ID supplied by the caller.
        valueObject->setProperty("objectID", objectID);
        CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
        OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
        Creator creator = j->second;
        boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
        ObjectHandler::Repository::instance().storeObject(objectID, object);
        return object;
    }

    std::vector<boost::shared_ptr<ObjectHandler::Object> > SerializationFactory::loadObject(
        const std::vector<std::string> &idList,
        const char *path) const {

        std::vector<boost::shared_ptr<ObjectHandler::Object> > returnValues;
        std::ifstream ifs(path);
        boost::archive::xml_iarchive ia(ifs);
        register_in(ia);

        std::vector<std::string>::const_iterator i;
        for (i=idList.begin(); i!=idList.end(); ++i) {
            boost::shared_ptr<ObjectHandler::ValueObject> valueObject;
            ia >> boost::serialization::make_nvp(i->c_str(), valueObject);
            // This VO has picked up the ID of the old VO that was deserialized.
            // Override this value with the new ID supplied by the caller.
            valueObject->setProperty("objectID", *i);
            CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
            OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
            Creator creator = j->second;
            boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
            ObjectHandler::Repository::instance().storeObject(*i, object);
            returnValues.push_back(object);
        }
        return returnValues;
    }

    void SerializationFactory::processPath(
        const std::string &path,
        std::vector<boost::shared_ptr<ObjectHandler::Object> > &returnValues) const {

        try {

            std::ifstream ifs(path.c_str());
            boost::archive::xml_iarchive ia(ifs);
            register_in(ia);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > valueObjects;
                ia >> boost::serialization::make_nvp("object_list", valueObjects);

            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >::const_iterator i;
            for (i=valueObjects.begin(); i!=valueObjects.end(); ++i) {
                boost::shared_ptr<ObjectHandler::ValueObject> valueObject = *i;
                CreatorMap::const_iterator j = creatorMap_().find(valueObject->className());
                OH_REQUIRE(j != creatorMap_().end(), "No creator for class " << valueObject->className());
                Creator creator = j->second;
                boost::shared_ptr<ObjectHandler::Object> object = creator(valueObject);
                std::string objectID = boost::any_cast<std::string>(valueObject->getProperty("objectID"));
                ObjectHandler::Repository::instance().storeObject(objectID, object);
                returnValues.push_back(object);
            }

        } catch (const std::exception &e) {
            OH_FAIL("Error deserializing file " << path << ": " << e.what());
        }
    }
        
    std::vector<boost::shared_ptr<ObjectHandler::Object> > SerializationFactory::loadObject2(
        const char *path) const {

        OH_REQUIRE(boost::filesystem::exists(path), "Invalid path : " << path);

        std::vector<boost::shared_ptr<ObjectHandler::Object> > returnValues;

        if (boost::filesystem::is_directory(path)) {

            for (boost::filesystem::directory_iterator itr(path); 
                itr!=boost::filesystem::directory_iterator(); ++itr) {

                if (boost::filesystem::is_regular(itr->path().string()))
                    processPath(itr->path().string(), returnValues);

            }

        } else {
            processPath(path, returnValues);
        }

        return returnValues;
    }

}

