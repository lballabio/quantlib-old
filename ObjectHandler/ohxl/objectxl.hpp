
/*
 Copyright (C) 2007 Eric Ehlers

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

#ifndef oh_objectxl_hpp
#define oh_objectxl_hpp

#include <oh/object.hpp>

namespace ObjectHandler {

    class CallingRange;

    class ObjectXL : public Object {        
    public:

        ObjectXL(const std::string &id, const boost::shared_ptr<Object> &object);

#ifdef COMPILING_XLL_DYNAMIC
        static DLL_API boost::shared_ptr<ObjectXL> create(
            const std::string &id, const boost::shared_ptr<Object> &object);
#endif

        virtual ~ObjectXL() {}

        const std::string &id() const {
            return id_;
        }
        const std::string &idFull() const {
            return idFull_;
        }
        boost::shared_ptr<Object> object() const {
            return object_;
        }

        void setCallingRange(const boost::shared_ptr<CallingRange> &callingRange);
        std::string callerKey() const;
        std::string callerAddress() const;

        static std::string ObjectXL::getStub(const std::string &objectID);

        virtual void dump(std::ostream &out);

    private:
        boost::shared_ptr<Object> object_;
        boost::shared_ptr<CallingRange> callingRange_;
        std::string id_;
        std::string idFull_;
    };

}

#endif
