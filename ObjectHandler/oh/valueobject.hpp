
/*
 Copyright (C) 2006 Plamen Neykov
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
    \brief Class ValueObject - Captures the inputs to an Object
*/

#ifndef oh_valueobject_hpp
#define oh_valueobject_hpp

#include <string>
#include <vector>
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
        ValueObject() : objectID_(""), className_(""), permanent_(false) {}
        //! Standard constructor called by derived classes.
        ValueObject(
            const std::string &objectID,
            const std::string &className,
            bool permanent)
            : objectID_(objectID), className_(className), permanent_(permanent) {}
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
    protected:
        std::string objectID_;
        std::string className_;
        bool permanent_;
    };

}

#endif

