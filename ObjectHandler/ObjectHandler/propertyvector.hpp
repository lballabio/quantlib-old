
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

/*
	definition of Object Property vector
	this is in a separate header file because it's all an addin needs to know
*/

#ifndef propertyvector_h
#define propertyvector_h

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <ObjectHandler/property.hpp>
#include <vector>
#include <string>

namespace ObjHandler {

    typedef boost::shared_ptr<boost::any> any_ptr;
    typedef Property<std::string, any_ptr> ObjectProperty;
    typedef std::vector<ObjectProperty> Properties;

}

#endif
