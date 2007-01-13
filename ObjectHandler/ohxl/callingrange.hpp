
/*
 Copyright (C) 2006 Eric Ehlers

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

/*! \file
    \brief CallingRange class
*/

#ifndef ohxl_callingrange_hpp
#define ohxl_callingrange_hpp

#include <oh/object.hpp>
#include <oh/iless.hpp>
#include <ohxl/rangereference.hpp>
#include <map>

namespace ObjHandler {

    class CallingRange {
    public:
        CallingRange();
        ~CallingRange();
        bool isValid() const;
        const std::string &getKey() const {
            return key_;
        }
        void deleteObject(const std::string &objectID, 
                          boost::shared_ptr<Object> object);
        void registerObject(const std::string &objectID, 
                            boost::shared_ptr<Object> object);
        void clearResidentObjects(bool deletePermanent);
        void update();
        std::string updateCount();
        friend std::ostream &operator<<(std::ostream&, const CallingRange&);
        bool empty() {
            return (residentObjects_.empty());
        }
        void setErrorMessage(const std::string &errorMessage, const bool &append);
        const std::string errorMessage() const {
            return errorMessage_;
        }
        void clearError() {
            errorMessage_ = "";
        }
        std::string getAddressString() const;
        bool contains(const RangeReference&);
    private:
        static int keyCount_;
        static std::string getKeyCount();
        std::string key_;
        std::string errorMessage_;
        bool busy_;
        int updateCount_;
        void setInvocationCount();
        int invocationCount_;
        std::map<std::string, boost::shared_ptr<Object>, my_iless > residentObjects_;
        boost::shared_ptr<RangeReference> rangeReference_;
    };

}

#endif
