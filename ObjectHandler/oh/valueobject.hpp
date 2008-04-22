/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Plamen Neykov
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
#include <set>
#include <oh/types.hpp>
#include <oh/variant.hpp>
#include <boost/any.hpp>
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
        virtual std::vector<std::string> getPropertyNames() const = 0;
        //! Retrieve the value of a property given its name.
        virtual boost::any getProperty(const std::string& name) const = 0;
        //! Set the value of a named property.
        virtual void setProperty(const std::string& name, const boost::any& value) = 0;
        //@}

        //! \name Serialization
        //@{
        //! Name of this ValueObject's class, used by class SerializationFactory.
        const std::string &className() const { return className_; }
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
		//relation the observable objects
		std::set<std::string> relatedIDs_;

		void processVariant(const ObjectHandler::Variant& variantID);
		void processVariant(const std::vector<ObjectHandler::Variant>& vecVariantID);
		void processVariant(const std::vector<std::vector<ObjectHandler::Variant> >& vecVariantIDs);
        void processRelatedID(const std::string& relatedID);
    };

    inline void ValueObject::processRelatedID(const std::string& relatedID) {
        if (!relatedID.empty())
            relatedIDs_.insert(relatedID);
    }

	inline void ValueObject::processVariant(const ObjectHandler::Variant& variantID){

		if(variantID.type() == ObjectHandler::String)
            processRelatedID(variantID);
	}

	inline void ValueObject::processVariant(const std::vector<ObjectHandler::Variant>& vecVariantID){
		std::vector<ObjectHandler::Variant>::const_iterator iterator = vecVariantID.begin();
		for(; iterator != vecVariantID.end(); ++iterator)
			processVariant(*iterator);
	}

	inline void ValueObject::processVariant(const std::vector<std::vector<ObjectHandler::Variant> >& vecVariantIDs){
		std::vector<std::vector<ObjectHandler::Variant> >::const_iterator iterator = vecVariantIDs.begin();
		for(; iterator != vecVariantIDs.end(); ++iterator){
			processVariant(*iterator);
		}

	}
}

#endif

