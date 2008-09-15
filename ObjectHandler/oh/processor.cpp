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
\brief Class ProcessorFactory - A Singleton class
*/
#include <oh/processor.hpp>

namespace ObjectHandler {

    ProcessorFactory *ProcessorFactory::instance_;

    std::map<std::string, ProcessorPtr> processorMap_;

    ProcessorFactory::ProcessorFactory(){
        instance_ = this;
    }

    ProcessorFactory &ProcessorFactory::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized ProcessorFactory object");
        return *instance_;
    }

    bool ProcessorFactory::storeProcessor(std::string name, ProcessorPtr& ptr){
        OH_REQUIRE(ptr, "Attempt to reference undefined Processor object");
        processorMap_[name] = ptr;
        return true;
    }

    ProcessorPtr ProcessorFactory::getProcessor(const boost::shared_ptr<ValueObject> &valueObject) {
        return processorMap_[valueObject->processorName()];
    }

    void ProcessorFactory::postProcess() {
        std::map<std::string, ProcessorPtr>::iterator it = processorMap_.begin();
        for(; it != processorMap_.end(); ++it)
            it->second->postProcess();
    }

}
