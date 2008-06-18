/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
Copyright (C) 2007 Eric Ehlers
Copyright (C) 2008 Nazcatech sprl Belgium

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

/*! \file
\brief Class SerializationFactory - A Singleton wrapping the boost::serialization interface
*/

#ifndef oh_processor_hpp
#define oh_processor_hpp

#include <map>
#include <string>
#include <list>

namespace ObjectHandler {

    typedef std::pair<std::string, boost::shared_ptr<ObjectHandler::Object> > StrObjectPair;
    typedef std::set<std::string> Category;
    typedef std::list<StrObjectPair> HandlesList;

    //boost::shared_ptr<Object> createRange(const boost::shared_ptr<ValueObject>&);

    class Processor {
    public:
        virtual std::string process(const SerializationFactory&,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const = 0;

        virtual void postProcess() const = 0;
        virtual ~Processor() {}
    };

    class DefaultProcessor : public Processor {

        std::string process(const SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const {

                StrObjectPair object = factory.createObject(valueObject, overwriteExisting);                    
                return object.first;
        }

        void postProcess() const {}
    };
    typedef boost::shared_ptr<ObjectHandler::Processor> ProcessorPtr;

    class DLL_API ProcessorFactory {

    public:
        static std::map<std::string, ProcessorPtr> processorMap_;
/*
        static int categoryIndex(const boost::shared_ptr<ObjectHandler::ValueObject>& obj, bool forComparison = false) {
            int endIndex = forComparison? LAST_SORTABLE_CATEGORY_INDEX: nrCategories;
            for(int i = 0; i < endIndex; ++i)
                if(categories[i].find(obj->className()) != categories[i].end())
                    return i;

            return nrCategories;
        }
*/
        static ProcessorPtr getProcessor(const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject) {
            return processorMap_[valueObject->processorName()];
        }

        static void postProcess() {
            std::map<std::string, ProcessorPtr>::iterator it = processorMap_.begin();
            for(; it != processorMap_.end(); ++it)
                it->second->postProcess();
        }
    };

}

#endif

