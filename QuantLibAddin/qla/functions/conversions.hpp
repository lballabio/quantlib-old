
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

#ifndef qla_conversions_hpp
#define qla_conversions_hpp

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    template < typename T >
    void handleVectorToObjectVector(
            const std::vector<std::string > &v,
            std::vector < boost::shared_ptr< T > > &ret) {
        std::vector<std::string >::const_iterator i;
        for (i = v.begin(); i != v.end(); i++) {
            std::string handle = *i;
            boost::shared_ptr< T > objPtr =
                boost::dynamic_pointer_cast< T >
                (ObjHandler::ObjectHandler::instance().retrieveObject(handle));
            if (!objPtr)
                throw ObjHandler::Exception(
                    "handleVectorToObjectVector: error retrieving object " + handle);
            ret.push_back(objPtr);
        }
    }

}

#endif

