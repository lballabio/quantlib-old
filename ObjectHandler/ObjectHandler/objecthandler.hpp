
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

/*! \file objecthandler.hpp
    \brief ObjectHandler class
*/

#ifndef objecthandler_hpp
#define objecthandler_hpp

#include <ObjectHandler/utilities.hpp>
#include <ObjectHandler/singleton.hpp>
#include <map>

namespace ObjHandler {

	//! Object pointer
	/*! a boost shared pointer to an Object
	*/
    typedef boost::shared_ptr<Object> obj_ptr;
	//! Object list
	/*! a map of string/obj_ptr pairs
		representing all of the Objects
		maintained in the Object Handler
	*/
    typedef std::map<std::string, obj_ptr> ObjectList;

    //! Object Handler class
	/*! maintains a repository of objects
        objects may be created/amended/destroyed
		by client application
	*/
    class ObjectHandler : public Singleton<ObjectHandler> {
		friend class Singleton<ObjectHandler>;
    public:
		//! \name storing & retrieving objects
		//@{
		//! store object with given handle
		/*! any existing object with that handle
			is deleted
		*/
        void storeObject(const std::string &handle,
                         const obj_ptr &object);
		//! retrieve object with given handle
		/*! returns null if no object exists
			with that handle
		*/
	    obj_ptr retrieveObject(const std::string &handle) const;
		//@}
    private:
        ObjectHandler() {}
		// ~ObjectHandler() {}
	    ObjectList objectList_;
    };

}

#endif
