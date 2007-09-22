
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

#ifndef example_serialization_factory_hpp
#define example_serialization_factory_hpp

#include <oh/ohdefines.hpp>
#include <ExampleObjects/Serialization/examplefactory.hpp>
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>

namespace ExampleAddin {

    class ExampleFactory : public AccountExample::ExampleFactory {
    public:
        static ExampleFactory &instance();
    private:
        virtual int saveObject(
            const std::vector<boost::shared_ptr<ObjectHandler::Object> >&objectList,
            const char *path,
            bool forceOverwrite) const;
        virtual std::vector<std::string> loadObject(
            const char *path,
            bool overwriteExisting) const;
        void processPath(
            const std::string &path,
            bool overwriteExisting,
            std::vector<std::string> &processedIDs) const;
        std::string processObject(
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;
        virtual std::string saveObjectString(
            const std::vector<boost::shared_ptr<ObjectHandler::Object> >&,
            bool forceOverwrite);
        virtual std::vector<std::string> loadObjectString(
            const std::string &xml,
            bool overwriteExisting);
    };

    // Wrappers for template tpl_register_classes.
    void register_in(boost::archive::xml_iarchive&);
    void register_out(boost::archive::xml_oarchive&);

}

#endif

