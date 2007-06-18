
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file
    \brief Class Object - Define interface for Objects to be stored in the Repository
*/


#include <oh/object.hpp>
#include <oh/valueobject.hpp>
#include <oh/Conversions/anytostream.hpp>

namespace ObjectHandler {

       std::vector<std::string> Object::propertyNames() const {
        std::vector<std::string> ret;
		if (mProps)
			ret = mProps->getPropertyNames();
			
        return ret;
    }

    boost::any Object::propertyValue(const std::string &propertyName) const {
		if (mProps)
			return mProps->getProperty(propertyName);
        OH_FAIL("ObjectHandler error: attempt to retrieve property "
            << "with unknown name '" << propertyName << "'");
    }

    void Object::dump(std::ostream &out) {
        out << std::endl;
        std::vector<std::string> propertyNames = this->propertyNames();
        for (std::vector<std::string>::const_iterator i = propertyNames.begin(); 
                i != propertyNames.end(); ++i) {
	        std::string propertyName = *i;
	        boost::any propertyValue = this->propertyValue(propertyName);
            out << "property = " << std::left << std::setw(logColumnWidth_) << propertyName;
            out << " value = " << std::left << std::setw(logColumnWidth_) << propertyValue << std::endl;
        }
        out << "permanent = " << std::left << std::setw(logColumnWidth_) 
            << std::boolalpha << permanent_ << std::endl;
        out << std::endl;
    }

}


