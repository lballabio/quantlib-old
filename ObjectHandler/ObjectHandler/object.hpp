
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

/*! \file object.hpp
    \brief Object class
*/

#ifndef object_hpp
#define object_hpp

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <ObjectHandler/property.hpp>
#include <vector>
#include <string>

namespace ObjHandler {

	//! shared pointer to any
	/*! used to hold the value for
		each of the Object's properties
	*/
    typedef boost::shared_ptr<boost::any> any_ptr;
	//! Property string/any pair
	/*! the string names the property
		and the any holds the corresponding value
	*/
    typedef Property<std::string, any_ptr> ObjectProperty;
	//! Property vector
	/*! each Object is described by a vector of properties
		which is maintained dynamically throughout
		the life of the object
	*/
    typedef std::vector<ObjectProperty> Properties;

	//! Object class
	/*! abstract base class
		implementing interface for objects
		to be stored in the Object Handler
	*/
    class Object {
    public:
		//! \name constructors & destructors
		//@{
		//! default constructor
		/*! construct an object
			to store the resulting object 
			in the Object Handler, call
				ObjectHandler::instance().storeObject(handle, object);
		*/
        Object() {}
		//! default destructor
	    virtual ~Object() {}
		//@}
		//! \name object interrogation
		//@{
		//! acquire a reference to underlying object
		/*! returns a reference to the client object
			stored in the Object
			this is a shared pointer to void which
			must be recast appropriately
		*/
	    virtual boost::shared_ptr<void> getReference() const = 0;
		//! return the Object's property vector
		/*! returns the property vector
			describing the underlying object
		*/
	    Properties getProperties();
		//@}
    protected:
	    Properties properties_;
    };
}

#endif
