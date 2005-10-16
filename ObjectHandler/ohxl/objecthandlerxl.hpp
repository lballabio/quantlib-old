
/*
 Copyright (C) 2005 Eric Ehlers

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
    \brief ObjectHandlerXL class
*/

#ifndef oh_objecthandlerxl_hpp
#define oh_objecthandlerxl_hpp

#include <oh/objecthandlerbase.hpp>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {
    //! Global Object repository.
    /*! Maintains a repository of Objects.
        Objects may be created/amended/destroyed
        by the client application.
    */
    class ObjectHandlerXL : public ObjectHandlerBase {
    public:
        //! \name storing / retrieving / deleting Objects
        //@{
        //! Store Object with given handle.
        /*! Any existing Object with that handle
            is deleted.
        */
        virtual const std::string storeObject(
            const std::string &handleStub,
            const obj_ptr &object);
        //! Delete Object with given handle.
        /*! Does nothing if no Object exists
            with that handle.
        */

        virtual void deleteObject(const std::string &handle);
        //! Delete all Objects in repository.
        /*! Does nothing if repository
            is already empty.
        */
        virtual void deleteAllObjects();
        virtual void setGcEnabled(const bool &newValue);
        virtual const bool &getGcEnabled();
        virtual void collectGarbage();
        ObjectHandlerXL() : key_(0), gcEnabled_(true) {}
    private:
        int key_;
        bool gcEnabled_;
        const std::string getKey();
        const std::string parseHandle(const std::string &handle);
        void deleteName(const std::string &handle);
        bool nameIsValid(const std::string &handle);
        // ~ObjectHandlerXL() {}
    };

}

#endif

