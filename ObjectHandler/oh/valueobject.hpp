/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2008 Plamen Neykov
 Copyright (C) 2007, 2008 Eric Ehlers
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
    \brief Class ValueObject - Captures the inputs to an Object
*/

#ifndef oh_valueobject_hpp
#define oh_valueobject_hpp

#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <oh/property.hpp>
#include <boost/serialization/access.hpp>

namespace ObjectHandler {

    //! Capture the values of the arguments passed to the Object constructor.
    /*! For each class derived from Object there is a corresponding ValueObject
        class.  The source code of classes derived from ValueObject is generated
        automatically by gensrc.  Each Object holds a reference to its
        ValueObject.

        The ValueObject captures a snapshot of the input arguments that were
        passed to the Object's constructor.  These values can be used later
        for serialization or run-time interrogation of the Object.
    */
    class ValueObject {
    public:
        //! \name Structors
        //@{
        //! Default constructor, required by boost::serialization.
        /*! Member variables are initialized with null values, these will be
            populated during deserialization.
        */
        ValueObject() : objectId_(""), className_(""), permanent_(false) {}
        //! Standard constructor called by derived classes.
        ValueObject(
            const std::string &objectId,
            const std::string &className,
            bool permanent)
            : objectId_(objectId), className_(className), permanent_(permanent) {}
        //! Empty virtual destructor.
        virtual ~ValueObject() {}
        //@}

        //! \name Inspectors
        //@{
        //! Retrieve the names of the properties stored in the ValueObject.
        std::set<std::string> getPropertyNames() const;
        virtual std::vector<std::string> getPropertyNamesVector() const = 0;
        virtual const std::set<std::string>& getSystemPropertyNames() const = 0;

        //! Retrieve the value of a property given its name.
        property_t getProperty(const std::string& name) const;
        virtual property_t getSystemProperty(const std::string& name) const = 0;
        // for now only checks for user properties not system properties
        bool hasProperty(const std::string& name) const;

        //! Set the value of a named property.
        void setProperty(const std::string& name, const property_t& value);
        virtual void setSystemProperty(const std::string& name, const property_t& value) = 0;

        //! Retrieve the object id of the underlying object in the object repository
        const std::string& objectId() const { return objectId_; }
        //@}

        //! \name className
        //@{
        //! Name of this ValueObject's class, used by class SerializationFactory.
        const std::string &className() const { return className_; }
        //@}

        //! \name processorName
        //@{
        //! the name of the processor that is required for that ValueObject;
        /*
            For ValueObject classes which do not require special processing, 
            processorName() returns DefaultProcessor.
        */
        virtual std::string processorName()  { return "DefaultProcessor"; }
        //@}

        //! \name getRelationObs
        //@{
        //! Retrieve the list relatedIDs_.
        const std::set<std::string>& getRelationObs() { return relatedIDs_;}
        //@}

    protected:
        std::string objectId_;
        std::string className_;
        bool permanent_;
        //! Name Value pair map representing the user defined properties.
        std::map<std::string, property_t> userProperties;
        //relation the observable objects
        std::set<std::string> relatedIDs_;

        //void processVariant(const property_t& variantID);
        void processVariant(property_t variantID);
        void processVariant(const std::vector<property_t>& vecVariantID);
        void processVariant(const std::vector<std::vector<property_t> >& vecVariantIDs);
        void processRelatedID(const std::string& relatedID);
    };

    inline void ValueObject::processRelatedID(const std::string& relatedID) {
        if (!relatedID.empty())
            relatedIDs_.insert(relatedID);
    }

    //inline void ValueObject::processVariant(const property_t& variantID){
    inline void ValueObject::processVariant(property_t variantID){
        if (std::string *objectID = boost::get<std::string>(&variantID))
            processRelatedID(*objectID);
    }

    inline void ValueObject::processVariant(const std::vector<property_t>& vecVariantID){
        std::vector<property_t>::const_iterator iterator = vecVariantID.begin();
        for(; iterator != vecVariantID.end(); ++iterator)
            processVariant(*iterator);
    }

    inline void ValueObject::processVariant(const std::vector<std::vector<property_t> >& vecVariantIDs){
        std::vector<std::vector<property_t> >::const_iterator iterator = vecVariantIDs.begin();
        for(; iterator != vecVariantIDs.end(); ++iterator){
            processVariant(*iterator);
        }

    }

    inline std::set<std::string> ValueObject::getPropertyNames() const {
        std::set<std::string> ret(getSystemPropertyNames());
        for(std::map<std::string, property_t>::const_iterator i
            = userProperties.begin(); i != userProperties.end(); ++i)
            ret.insert(i->first);
        return ret;
    }

    inline property_t ValueObject::getProperty(const std::string& name) const {
        if(userProperties.find(name) != userProperties.end())
            return userProperties.find(name)->second;
        else
            return getSystemProperty(name);
    }

    inline bool ValueObject::hasProperty(const std::string& name) const {
        return userProperties.find(name) != userProperties.end();
    }

    inline void ValueObject::setProperty(const std::string& name, const property_t& value) {
        const std::set<std::string>& sysNames = getSystemPropertyNames();
        if(sysNames.find(name) != sysNames.end())
            setSystemProperty(name, value);
        else
            userProperties[name] = value;
    }
}

#endif

