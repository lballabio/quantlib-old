
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

#ifndef oh_object_hpp
#define oh_object_hpp

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <oh/property.hpp>
#include <vector>
#include <string>

namespace ObjHandler {

    //! Shared pointer to any.
    /*! Used to hold the value for
        each of the Object's properties.
    */
    typedef boost::shared_ptr<boost::any> any_ptr;
    //! Property string/any pair.
    /*! The string names the property
        and the any holds the corresponding value.
    */
    typedef Property<std::string, any_ptr> ObjectProperty;
    //! Property vector.
    /*! Each Object is described by a vector of properties
        which is maintained dynamically throughout
        the life of the Object.
    */
    typedef std::vector<ObjectProperty> Properties;

    //! Object class.
    /*! Abstract base class
        implementing interface for Objects
        to be stored in the ObjectHandler.
    */
    class Object {
    public:
        //! \name Constructors & Destructors
        //@{
        //! Default constructor.
        /*! Construct an Object.
            To store the resulting Object 
            in the ObjectHandler, call
                ObjectHandler::instance().storeObject(handle, object);
        */
        Object() {}
        //! Default destructor.
        virtual ~Object() {}
        //@}
        //! \name Object interrogation
        //@{
        //! Acquire a reference to underlying Object.
        /*! Returns a reference to the client object
            stored in the Object.
            This is a shared pointer to void which
            must be recast appropriately.
        */
        virtual boost::shared_ptr<void> getReference() const = 0;
        //! Return the Object's property vector.
        /*! Returns the property vector
            describing the underlying Object.
        */
        const Properties& getProperties() const;
        //@}
    protected:
        Properties properties_;
    };
}

#endif

