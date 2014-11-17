/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Plamen Neykov

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
    \brief Generic conversion functions - used to convert from property_t or OPER to an C++ data type
*/

#ifndef oh_Conversions_convert2_hpp
#define oh_Conversions_convert2_hpp

namespace ObjectHandler {

    template<class value_t, class container_t>
    value_t convert2(const container_t& c) {
        try {
            return c.operator value_t();
        }
        catch(const std::exception& e) {
            OH_FAIL("Unable to convert type '" << c.type().name()
                << "' to type '" << typeid(value_t).name() << "' - " << e.what());
        }
    }

    template<class value_t, class container_t>
    value_t convert2(const container_t& c, const std::string &parameterName) {
        try {
            return convert2<value_t, container_t>(c);
        }
        catch(const std::exception& e) {
            OH_FAIL("Error converting parameter '" << parameterName << "' : '" << e.what());
        }
    }

    template<class value_t, class container_t>
    value_t convert2(const container_t& c, const std::string &parameterName, const value_t& defaultValue) {
        try {
            if(c.missing())    
                return defaultValue;
            return convert2<value_t, container_t>(c);
        }
        catch(const std::exception& e) {
            OH_FAIL("Error converting parameter '" << parameterName << "' : '" << e.what());
        }
    }

    template<class value_t, class container_t>
    value_t convert2(const container_t& c, const std::string &parameterName, 
            const value_t& defaultValue, const value_t& errorValue) {
        try {
            if(c.error())    
                return errorValue;
            return convert2<value_t, container_t>(c, parameterName, defaultValue);
        }
        catch(const std::exception&) {
            return errorValue;
        }
    }

}

#endif

