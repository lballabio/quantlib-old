
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <ql/qldefines.hpp>
#include <string>
// capture value of QL_VERSION before utilities.hpp #undefs it
const std::string QL_VERSION_KEEP = QL_VERSION;
#include <qla/utilities.hpp>
#include <sstream>

using namespace ObjHandler;

namespace QuantLibAddin {

    std::string QL_VERSION() {
        std::ostringstream s;
        s << "QuantLib version " << QL_VERSION_KEEP;
        return s.str();
    }

    std::string QL_OH_VERSION() {
        std::ostringstream s;
        s << "ObjectHandler version " << OBJHANDLER_VERSION;
        return s.str();
    }

    const Properties& QL_QUERY(
            const std::string &handle) {
        boost::shared_ptr<Object> object =
                ObjectHandler::instance().retrieveObject(handle);
        if (!object)
                throw Exception("error retrieving object " + handle);
        return object->getProperties();
    }

}

