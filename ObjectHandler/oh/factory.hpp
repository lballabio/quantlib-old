
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
    \brief Factory function to instantiate an Object in ObjectHandler
*/

#ifndef oh_factory_hpp
#define oh_factory_hpp

#include <oh/objecthandler.hpp>

namespace ObjHandler {

    //! Template class implementing a factory function for constructing Objects.
    /*! Accepts a classname, handle, and argument stack
        and instantiates the corresponding Object in ObjectHandler.
    */
    template < class T >
        class Factory {
        public:
        //! Create an Object.
        /*! Instantiates an Object in ObjectHandler.
            Object is stored under given handle.
            The argument stack is passed to the Object's constructor.
            The Object's Property vector is returned.
        */
        static const Properties& makeObject(
                const std::string &handle,
                ArgStack &args) {
            obj_ptr object = obj_ptr(new T(args));
            ObjectHandler::instance().storeObject(handle, object);
            return object->getProperties();
        }
    };

}

#endif

