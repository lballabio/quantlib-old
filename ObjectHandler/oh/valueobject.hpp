
/*
 Copyright (C) 2006 Plamen Neykov

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
    \brief ValueObject class
*/

#ifndef oh_valueobject_hpp
#define oh_valueobject_hpp

#include <string>
#include <vector>
#include <boost/any.hpp>

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors 
   (Metrowerks, for example) also #define _MSC_VER
*/
#if defined BOOST_MSVC       // Microsoft Visual C++
#pragma warning(disable:4231)
#endif

namespace ObjHandler {

	class ValueObject {
	public:
		virtual std::vector<std::string> getPropertyNames() const = 0;
		virtual boost::any getProperty(const std::string& name) const = 0;
        virtual ~ValueObject() {}
	};

}

#endif

