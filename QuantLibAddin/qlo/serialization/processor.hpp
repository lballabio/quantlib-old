
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
#ifndef qla_processor_hpp
#define qla_processor_hpp

#include <oh/repository.hpp>
#include <oh/processor.hpp>

namespace QuantLibAddin {

    class InstrumentProcessor : public ObjectHandler::Processor {

        std::string process(const ObjectHandler::SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;

        void postProcess() const {}
    };

    class RelinkableHandleProcessor : public ObjectHandler::Processor {

        std::string process(const ObjectHandler::SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;

        void postProcess() const;

    private:
        mutable ObjectHandler::HandlesList handles;
    };

    class LegProcessor : public ObjectHandler::Processor {

        std::string process(const ObjectHandler::SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;

        void postProcess() const {}
    };

    class IndexProcessor : public ObjectHandler::Processor {

        std::string process(const ObjectHandler::SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;

        void postProcess() const {}
    };

    class ExtrapolatorProcessor : public ObjectHandler::Processor {

        std::string process(const ObjectHandler::SerializationFactory& factory,
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;

        void postProcess() const {}
    };

}

#endif
