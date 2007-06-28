
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

/*! \file
    \brief Class Factory - xxx
*/

#ifndef addin_factory_hpp
#define addin_factory_hpp

#include <oh/ohdefines.hpp>
#include <ExampleObjects/Serialization/examplefactory.hpp>

namespace ExampleAddin {

    class ExampleFactory : public AccountExample::ExampleFactory {
    public:
        static ExampleFactory &instance();
    private:
		virtual void saveObject(const boost::shared_ptr<ObjectHandler::Object>&, const char *path) const;
		virtual void saveObject(const std::vector<boost::shared_ptr<ObjectHandler::Object> >&, const char *path) const;
		virtual boost::shared_ptr<ObjectHandler::Object> loadObject(const std::string &objectID, const char *path) const;
		virtual std::vector<boost::shared_ptr<ObjectHandler::Object> > loadObject(const std::vector<std::string> &idList, const char *path) const;
    };


}

#endif
