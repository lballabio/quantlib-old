
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef ohxl_callingrange_hpp
#define ohxl_callingrange_hpp

#include <oh/iless.hpp>
#include <ohxl/objectxl.hpp>
#include <string>
#include <map>

namespace ObjectHandler {

    class CallingRange {
        friend std::ostream &operator<<(std::ostream&, const boost::shared_ptr<CallingRange>&);
    public:
        CallingRange();
        ~CallingRange();
        bool valid() const;
        const std::string &key() const { return key_; }
        std::string updateCount();
        static int keyWidth() { return KEY_WIDTH; }
        void registerObject(boost::shared_ptr<ObjectXL> objectXL);
        void clearResidentObjects(bool deletePermanent);
        bool empty() { return residentObjects_.empty(); }
        std::string addressString() const;
    private:
        static int keyCount_;
        static std::string getKeyCount();
        static const int KEY_WIDTH;
        std::string key_;
        int updateCount_;
        typedef std::map<std::string, boost::shared_ptr<ObjectXL>, my_iless > ObjectXLMap;
        ObjectXLMap residentObjects_;
    };

    std::ostream &operator<<(std::ostream&, const boost::shared_ptr<CallingRange>&);
}


#endif

