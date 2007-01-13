
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
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
    \brief Object class
*/

#ifndef oh_object_hpp
#define oh_object_hpp

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <oh/valueobject.hpp>
#include <oh/objhandlerdefines.hpp>
#include <oh/exception.hpp>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>

// get a boost shared pointer to a class derived from Object
#define OH_GET_OBJECT( NAME, ID, OBJECT_CLASS ) \
    boost::shared_ptr< OBJECT_CLASS > NAME; \
    ObjHandler::ObjectHandler::instance().retrieveObject(NAME, ID);

// get a boost shared pointer to the client library object referenced by an ObjHandler::Object
#define OH_GET_REFERENCE( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    NAME ## temp->getLibraryObject(NAME);

// OH_GET_REFERENCE_DEFAULT - like OH_GET_REFERENCE but only attempt retrieval if id supplied
#define OH_GET_REFERENCE_DEFAULT( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    if (!ID.empty()) { \
        OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
        NAME ## temp->getLibraryObject(NAME); \
    }

// get a direct reference to the underlying object wrapped by the ObjHandler::Object
#define OH_GET_UNDERLYING( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    const LIBRARY_CLASS &NAME = *(NAME ## temp.get());

// like OH_GET_UNDERLYING but without const qualifier
#define OH_GET_UNDERLYING_NONCONST( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    LIBRARY_CLASS &NAME = *(NAME ## temp.get());

namespace ObjHandler {
    //! Interface for Objects to be stored in the ObjectHandler.
    /*! Objects are constructed by the client and passed to
        ObjHandler::storeObject() for storage in the repository.
    */
    class Object {        
    public:
        //! \name Constructors & Destructors
        //@{

        //! Default constructor.
        /*! Construct an Object.
            To store the resulting Object in the ObjectHandler, call
                ObjectHandler::instance().storeObject(objectID, object);
        */
        Object() : anonymous_(false), permanent_(false) {};

        //! Default destructor.
        virtual ~Object() {};

        //@}

        //! \name Object interrogation
        //@{

        //! Retrieve vector of property names.
        /*! Returns empty vector if Object has no properties.
        */
        std::vector < std::string > propertyNames() const;

        //! Retrieve value of given property.
        /*! Throws exception if Object has no property by that name.
        */
        boost::any propertyValue(const std::string &propertyName) const;

        friend std::ostream &operator<<(std::ostream&, const Object &object);
        //@}

        void setProperties(const boost::shared_ptr<ValueObject>& p) {
            mProps = p;
        }

        const bool &anonymous() const {
            return anonymous_;
        }
        void setAnonymous() {
            anonymous_ = true;
        }

        const bool &permanent() const {
            return permanent_;
        }
        void setPermanent() {
            permanent_ = true;
        }
    private:
        boost::shared_ptr<ValueObject> mProps;
        Object& operator= (const Object&);
        Object(const Object&);
        bool anonymous_;
        bool permanent_;
    };

    template < typename T >
    std::ostream& operator<<(std::ostream& out, std::vector < T > &v) {
        out << std::endl;
        for (typename std::vector< T >::const_iterator i = v.begin(); i != v.end(); ++i)
            out << *i << std::endl;
        return out;
    }

    /*
    Helper template Object subclass implementing behavior required by
    the majority of classes derived from Object for managing
    the reference to the contained library object.

    Object subclasses requiring customized processing of the reference to
    the contained library object should bypass LibraryObject and inherit
    directly from Object.
    */
    template <class LibraryClass>
    class LibraryObject : public Object {
    public:
        template <class LibraryDerivedClass>
        void getLibraryObject(boost::shared_ptr<LibraryDerivedClass> &ret) const {
            ret = boost::dynamic_pointer_cast<LibraryDerivedClass>(libraryObject_);
            if (!ret) {
                std::ostringstream msg;
                msg << "Error retrieving library object - unable to convert reference"
                    << " from type " << std::endl << "    " << typeid(LibraryClass).name()
                    << " to type "   << std::endl << "    " << typeid(LibraryDerivedClass).name();
                throw Exception(msg.str());
            }
        }
        void getLibraryObject(boost::shared_ptr<LibraryClass> &ret) const {
            ret = libraryObject_;
        }
    protected:
        boost::shared_ptr<LibraryClass> libraryObject_;
    };

}

#endif
