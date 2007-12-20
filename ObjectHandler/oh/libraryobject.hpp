/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Eric Ehlers
 
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
    \brief A customization of the Object class, suitable for most applications
*/

#ifndef oh_libraryobject_hpp
#define oh_libraryobject_hpp

#include <oh/object.hpp>

namespace ObjectHandler {

    //! Customizes the Object class for most typical uses.
    /*! LibraryObject is a template class, the template argument specifying the
        class of the underlying object to which the LibraryObject holds a reference.

        Object subclasses requiring customized processing of the reference to
        the contained library object should bypass LibraryObject and inherit
        directly from Object.
    */
    template <class LibraryClass>
    class LibraryObject : public Object {
    public:
        //! Ctor.
        LibraryObject(
            const boost::shared_ptr<ValueObject>& properties,
            bool permanent)
            : Object(properties, permanent) {}

        //! Retrieve the underlying reference and recast it to the specified class.
        template <class LibraryDerivedClass>
        void getLibraryObject(boost::shared_ptr<LibraryDerivedClass> &ret) const {
            ret = boost::dynamic_pointer_cast<LibraryDerivedClass>(libraryObject_);
            OH_REQUIRE(ret, "Error retrieving library object - unable to convert reference"
                    << " from type " << std::endl << "    " << typeid(LibraryClass).name()
                    << " to type "   << std::endl << "    " << typeid(LibraryDerivedClass).name());
        }
        //! Override for special case where no recast is necessary.
        void getLibraryObject(boost::shared_ptr<LibraryClass> &ret) const {
            ret = libraryObject_;
        }
    protected:
        //! A reference to the underlying object.
        boost::shared_ptr<LibraryClass> libraryObject_;
    };

}

#endif

