
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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
#include <qla/utilities.hpp>
#include <qla/typeregistry.hpp>
#include <sstream>

namespace QuantLibAddin {

    std::string qlVersion() {
        return QL_VERSION;
    }

    const std::vector<std::string>& qlListRegisteredEnums() {
        static std::vector<std::string> ret
            = EnumRegistry::instance().getAllRegisteredTypes();
        return ret;
    }

    const std::vector<std::string>& qlListEnum(const std::string& id) {
        static std::vector<std::string> ret;
        ret = EnumRegistry::instance().getTypeElements(id);
        return ret;
    }

    const std::vector<std::string>& qlListRegisteredTypes() {
        static std::vector<std::string> ret
            = ComplexTypeRegistry::instance().getAllRegisteredTypes();
        return ret;
    }

    const std::vector<std::string>& qlListType(const std::string& id) {
        static std::vector<std::string> ret;
        ret = ComplexTypeRegistry::instance().getTypeElements(id);
        return ret;
    }

}

