
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

/*! \file
    \brief ObjectHandlerBase class
*/

#ifndef oh_objecthandlerbase_hpp
#define oh_objecthandlerbase_hpp

#include <oh/object.hpp>
#include <map>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {
    //! Object pointer
    /*! A boost shared pointer to an Object.
    */
    typedef boost::shared_ptr<Object> obj_ptr;

    //! Object list
    /*! A map of string/obj_ptr pairs representing all of the Objects
        maintained in the ObjectHandler.
    */
    typedef std::map<std::string, obj_ptr> ObjectList;

    //! Global Object repository.
    /*! Maintains a repository of Objects.  Objects may be 
        created/amended/destroyed by the client application.
    */
    class ObjectHandlerBase {
    public:
        //! \name storing / retrieving / deleting Objects
        //@{
        //! Store Object with given handle.
        /*! Any existing Object with that handle is deleted.
        */
        virtual const std::string storeObject(const std::string &handle,
            const obj_ptr &object);
        //! Retrieve Object with given handle.
        /*! Throws exception if no Object exists with that handle.
        */
        virtual obj_ptr retrieveObject(const std::string &handle) const;
        //! Retrieve property names of Object with given handle.
        /*! Throws exception if no Object exists with that handle.
            Returns empty vector if given Object has no properties.
        */
        virtual const std::vector < std::string > propertyNames(
            const std::string &handle) const;
        //! Retrieve value of given property from given Object.
        /*! Throws exception if no Object exists with that handle.
            or if given Object has no property by that name.
        */
        /*virtual const boost::any propertyValue(const std::string &handle,
            const std::string &propertyName) const;*/
        //! Delete Object with given handle.
        /*! Does nothing if no Object exists with that handle.
        */
        virtual void deleteObject(const std::string &handle);
        //! Delete all Objects in repository.
        /*! Does nothing if repository is already empty.
        */
        virtual void deleteAllObjects();
        //@}
        //! \name utilities
        //@{
        //! Log dump of ObjectHandler.
        /*! Write all objects in ObjectHandler 
            to output stream.
        */
        virtual void dump(std::ostream&);
        //! Count of all Objects in repository.
        virtual const int objectCount();
        //! List handles of all Objects in repository.
        /*! Returns empty list if repository is empty.
        */
        virtual const std::vector < std::string > handleList();
        //@}
        virtual ~ObjectHandlerBase() {}
    protected:
        ObjectList objectList_;
        ObjectHandlerBase() {}
    };

}

#endif

