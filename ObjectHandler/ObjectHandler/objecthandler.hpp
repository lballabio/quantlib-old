
/*
 Copyright (C) 2004 Eric Ehlers

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

#ifndef objecthandler_h
#define objecthandler_h

#include <ObjectHandler/objecthandlerversion.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER

*/
#ifdef BOOST_MSVC
#  include <ObjectHandler/autolink.hpp>
#endif

#include <ObjectHandler/utilities.hpp>

#include <map>


namespace ObjHandler {

    typedef boost::shared_ptr<Object> obj_ptr;
    typedef map<string, obj_ptr> ObjectList;

    class ObjectHandler {
    public:
        void storeObject(const std::string &handle,
                        const obj_ptr &object);
	    obj_ptr retrieveObject(const std::string &handle);
        ObjectHandler() {}
        ~ObjectHandler() {}
    private:
	    ObjectList objectList_;
    };

}

#endif
