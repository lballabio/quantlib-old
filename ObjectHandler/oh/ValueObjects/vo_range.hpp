
/*  
 Copyright (C) 2005, 2006 Plamen Neykov
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

// This file was generated automatically by gensrc.py.
// Editing this file manually is not recommended.

#ifndef oh_vo_range_hpp
#define oh_vo_range_hpp

#include <oh/valueobject.hpp>
#include <string>
#include <vector>
#include <boost/any.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace ObjectHandler { namespace ValueObjects {

    class ohRange : public ObjectHandler::ValueObject {
        friend class boost::serialization::access;
    public:
        ohRange() {}
        ohRange(
            const std::string& ObjectId,
            const std::vector<std::vector <double> >& Values,
            bool Permanent) :
        ObjectHandler::ValueObject(ObjectId, "ohRange", Permanent),
        Values_(Values),
        Permanent_(Permanent) {}

        std::vector<std::string> getPropertyNames() const {
            std::vector<std::string> ret;
            ret.push_back("ObjectId");
            ret.push_back("ClassName");
            ret.push_back("Values");
            ret.push_back("Permanent");
            return ret;
        }

        boost::any getProperty(const std::string& name) const {
            std::string nameUpper = boost::algorithm::to_upper_copy(name);
            if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
                return objectId_;
            else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
                return className_;
            else if(strcmp(nameUpper.c_str(), "VALUES")==0)
                return Values_;
            else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
                return Permanent_;
            else
                OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
        }

        void setProperty(const std::string& name, const boost::any& value) {
            std::string nameUpper = boost::algorithm::to_upper_copy(name);
            if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
                objectId_ = boost::any_cast<std::string>(value);
            else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
                className_ = boost::any_cast<std::string>(value);
            else if(strcmp(nameUpper.c_str(), "VALUES")==0)
                Values_ = boost::any_cast<std::vector<std::vector<double> > >(value);
            else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
                Permanent_ = boost::any_cast<bool>(value);
            else
                OH_FAIL("Error: attempt to set non-existent Property: '" + name + "'");
        }

    protected:

        std::vector<std::vector <double> > Values_;
        bool Permanent_;
        
        template<class Archive>
        void serialize(Archive& ar, const unsigned int) {
        boost::serialization::void_cast_register<ohRange, ObjectHandler::ValueObject>(this, this);
            ar  & boost::serialization::make_nvp("ObjectId", objectId_)
                & boost::serialization::make_nvp("ClassName", className_)
                & boost::serialization::make_nvp("Values", Values_)
                & boost::serialization::make_nvp("Permanent", Permanent_);
        }
    };



} }

#endif

