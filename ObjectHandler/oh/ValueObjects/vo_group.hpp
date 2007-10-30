
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

#ifndef oh_vo_group_hpp
#define oh_vo_group_hpp

#include <oh/valueobject.hpp>
#include <string>
#include <vector>
#include <boost/any.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace ObjectHandler { namespace ValueObjects {

    class ohGroup : public ObjectHandler::ValueObject {
        friend class boost::serialization::access;
    public:
        ohGroup() {}
        ohGroup(
            const std::string& ObjectId,
            const std::vector<std::string>& ObjectIdList,
            bool Permanent) :
        ObjectHandler::ValueObject(ObjectId, "ohGroup", Permanent),
        ObjectIdList_(ObjectIdList),
        Permanent_(Permanent) {}

        std::vector<std::string> getPropertyNames() const {
            std::vector<std::string> ret;
            ret.push_back("ObjectId");
            ret.push_back("ClassName");
            ret.push_back("ObjectIdList");
            ret.push_back("Permanent");
            return ret;
        }

        boost::any getProperty(const std::string& name) const {
            std::string nameUpper = boost::algorithm::to_upper_copy(name);
            if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
                return objectId_;
            else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
                return className_;
            else if(strcmp(nameUpper.c_str(), "OBJECTIDLIST")==0)
                return ObjectIdList_;
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
            else if(strcmp(nameUpper.c_str(), "OBJECTIDLIST")==0)
                ObjectIdList_ = boost::any_cast<std::vector<std::string> >(value);
            else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
                Permanent_ = boost::any_cast<bool>(value);
            else
                OH_FAIL("Error: attempt to set non-existent Property: '" + name + "'");
        }

    protected:

        std::vector<std::string> ObjectIdList_;
        bool Permanent_;
        
        template<class Archive>
        void serialize(Archive& ar, const unsigned int) {
        boost::serialization::void_cast_register<ohGroup, ObjectHandler::ValueObject>(this, this);
            ar  & boost::serialization::make_nvp("ObjectId", objectId_)
                & boost::serialization::make_nvp("ClassName", className_)
                & boost::serialization::make_nvp("ObjectIdList", ObjectIdList_)
                & boost::serialization::make_nvp("Permanent", Permanent_);
        }
    };



} }

#endif

