
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

#ifndef oh_vo_range_hpp
#define oh_vo_range_hpp

#include <oh/valueobject.hpp>
#include <string>
#include <vector>
#include <boost/any.hpp>

namespace ObjectHandler { namespace ValueObjects {

    class ohRange : public ObjectHandler::ValueObject {
        friend class boost::serialization::access;
    public:
        ohRange() {}
        ohRange(
            const std::string& objectID,
            const std::vector<double>& values) :
            ValueObject(objectID, "ohRange"),
            values_(values) {}

        std::vector<std::string> getPropertyNames() const {
            std::vector<std::string> ret;
            ret.push_back("objectID");
            ret.push_back("className");
            ret.push_back("values");
            return ret;
        }

        boost::any getProperty(const std::string &name) const {
            if(name == "objectID") return objectID_;
            else if(name == "className") return className_;
            else if(name == "values") return values_;
            else
                OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
        }

    protected:

        std::vector<double> values_;
        
        template<class Archive>
        void serialize(Archive& ar, const unsigned int) {
        boost::serialization::void_cast_register<ohRange, ObjectHandler::ValueObject>(this, this);
            ar  & boost::serialization::make_nvp("objectID", objectID_)
                & boost::serialization::make_nvp("className", className_)
                & boost::serialization::make_nvp("values", values_);
        }
    };

} }

#endif

