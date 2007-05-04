
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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
    \brief Class Object - Define interface for Objects to be stored in the Repository.
*/

#ifndef oh_object_hpp
#define oh_object_hpp

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <oh/ohdefines.hpp>
#include <oh/valueobject.hpp>
#include <oh/exception.hpp>
#include <oh/Conversions/anytostream.hpp>
#include <vector>
#include <string>
#include <sstream>
#include <iomanip>

/*! \def OBJECT_LOG_COLUMN_WIDTH
    The width of a column of data written to the log file.
*/
#define OBJECT_LOG_COLUMN_WIDTH 20

namespace ObjectHandler {

    //! Interface for Objects to be stored in the ObjectHandler Repository.
    /*! Objects are constructed by the client and passed to
        Repository::storeObject() for storage in the Repository.

        All member functions of this class are implemented inline.  This is not
        for performance reasons; the entire implementation of the class must appear
        in the header file in order to allow derived classes to inherit from Object
        across DLL boundaries on the Windows platform.
    */
    class Object {        
    public:
        //! \name Structors
        //@{

        //! Default constructor.
        /*! Construct an Object.
            To store the resulting Object in the ObjectHandler, call
                Repository::instance().storeObject(objectID, object);
        */
        Object() : permanent_(false) {}

        //! Empty virtual destructor.
        virtual ~Object() {}
        //@}

        //! \name Value Objects
        //@{
        //! Set the ValueObject associated with this Object.
        void setProperties(const boost::shared_ptr<ValueObject>& p) {
            mProps = p;
        }

        //! Retrieve a vector of property names.
        /*! Retrieve property names from associated ValueObject.
            Return an empty vector if the Object has no properties.
        */
        std::vector<std::string> propertyNames() const;

        //! Retrieve the value of a given property.
        /*! Forward the request to the associated ValueObject.
            Throw an exception if the Object has no property by that name.
        */
        boost::any propertyValue(const std::string &propertyName) const;
        //@}

        //! \name Permanent Objects
        //@{
        //! Set the "permanent" flag to True for this Object.
        /*! Permanent Objects remain in the Repository after a call to
            Repository::deleteAllObjects() or Repository::collectGarbage().

            This feature allows a finer level of granularity in maintaining
            the contents of the Repository.
        */
        void setPermanent() { permanent_ = true; }
        //! Query the value of the "permanent" flag.
        const virtual bool &permanent() const { return permanent_; }
        //@}

        //! \name Logging
        //@{
        //! "dump" - Write this Object's properties (from the ValueObject) to the given stream.
        /*! This function is called by the logging framework.  Derived classes may
            override this function if additional information is available.
        */
        virtual void dump(std::ostream &out);
        //@}

    private:
        // The ValueObject associated with this Object.
        boost::shared_ptr<ValueObject> mProps;
        // Flag to indicate whether this Object is permanent.
        bool permanent_;
        // Operator = declared but not implemented - assignment is not supported.
        Object& operator= (const Object&);
        // Copy ctor declared but not implemented - copy construction is not supported.
        Object(const Object&);
    };

    inline std::vector<std::string> Object::propertyNames() const {
        std::vector<std::string> ret;
		if (mProps)
			ret = mProps->getPropertyNames();
			
        return ret;
    }

    inline boost::any Object::propertyValue(const std::string &propertyName) const {
		if (mProps)
			return mProps->getProperty(propertyName);
        OH_FAIL("ObjectHandler error: attempt to retrieve property "
            << "with unknown name '" << propertyName << "'");
    }

    inline void Object::dump(std::ostream &out) {
        out << std::endl;
        std::vector<std::string> propertyNames = this->propertyNames();
        for (std::vector<std::string>::const_iterator i = propertyNames.begin(); 
                i != propertyNames.end(); ++i) {
	        std::string propertyName = *i;
	        boost::any propertyValue = this->propertyValue(propertyName);
            out << "property = " << std::left << std::setw(OBJECT_LOG_COLUMN_WIDTH) << propertyName;
            out << " value = " << std::left << std::setw(OBJECT_LOG_COLUMN_WIDTH) << propertyValue << std::endl;
        }
        out << "permanent = " << std::left << std::setw(OBJECT_LOG_COLUMN_WIDTH) 
            << std::boolalpha << permanent_ << std::endl;
        out << std::endl;
    }

    inline std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<Object> &object) {
        object->dump(out);
        return out;
    }

}

#endif

