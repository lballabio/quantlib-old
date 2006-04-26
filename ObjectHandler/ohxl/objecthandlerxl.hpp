
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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
        virtual obj_ptr retrieveObject(const std::string &name) const;
        virtual void collectGarbage();
        void deleteKey(const std::string &key);
    private:
        bool nameIsFull(const std::string &name) const;
        obj_ptr retrieveObjectNameStub(const std::string &nameStub) const;
    };

}

#endif

