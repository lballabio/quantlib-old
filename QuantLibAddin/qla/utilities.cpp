
/*
 Copyright (C) 2005 Plamen Neykov
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
#include <qla/enumfactory.hpp>
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

    const std::vector<std::string>& getRegisteredEnums() {
        static std::vector<std::string> ret
            = EnumTypeFactory::instance().getAllRegisteredEnums();
        return ret;
    }

    const std::vector<std::string>& getEnumMembers(const std::string& id) {
        static std::vector<std::string> ret;
        ret = EnumTypeFactory::instance().getEnumElements(id);
        return ret;
    }

    QuantLib::Date createQLDate(long date) {
        if(date) 
            return QuantLib::Date(date);
        else 
            return QuantLib::Date();
    }
}


