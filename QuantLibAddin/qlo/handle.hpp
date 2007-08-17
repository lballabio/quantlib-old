
/*
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_handle_hpp
#define qla_handle_hpp

#include <oh/object.hpp>
#include <ql/handle.hpp>

namespace QuantLibAddin {

    template <class T>
    class RelinkableHandle : public ObjectHandler::Object {
      public:
        RelinkableHandle(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<T> &observable,
            bool permanent) : ObjectHandler::Object(properties, permanent) {

            if (observable) {
                handle_.linkTo(observable);
                //currentLink_ = boost::any_cast<std::string>(propertyValue("currentLink"));
                currentLink_ = "dummyID";
            }
        }
        void linkTo(const boost::shared_ptr<T> &observable) {
        //void linkTo(const boost::shared_ptr<ObjectHandler::Object> &object) {

            //boost::shared_ptr<T> observable;
            //object->getLibraryObject(observable);

            QL_REQUIRE(observable,
                       "Error relinking handle of type " << typeid(this).name()
                       << " - input object is null");
            handle_.linkTo(observable);

            //currentLink_ = boost::any_cast<std::string>(object->propertyValue("ObjectID"));
            currentLink_ = "newDummyID";
        }
        QuantLib::RelinkableHandle<T> getHandle() {
            return handle_;
        }
        std::string currentLink() {
            //return boost::any_cast<std::string>(propertyValue("currentLink"));
            return currentLink_;
        }
      private:
        QuantLib::RelinkableHandle<T> handle_;
        std::string currentLink_;
    };

}

#endif

