/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Plamen Neykov
 Copyright (C) 2014 Eric Ehlers

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

    /*! \name convert2
    The functions in this file are used by addin functions to convert their
    inputs into types recognized by the underlying library.

    All of these functions are implemented as overrides of template function
    "convert2".

    The reason that these functions are called "convert2" is because they
    replace an old function "convert" which has now been deleted.

    These functions operate on an input value of type container_t.
    \li On C++, container_t is a variant.
    \li On Excel, container_t is an XLOPER.
    */
    //@{
    //! Convert the input value from type container_t to type value_t.
    /*! If the attempt fails then wrap the exception with additional info and
        rethrow.
    */
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

    //! Convert the input value from type container_t to type value_t.
    /*! This override allows the caller to specify the parameter name, which
        makes the error message clearer in the event of an error.  Otherwise
        this function behaves the same as the one above.
    */
    template<class value_t, class container_t>
    value_t convert2(const container_t& c, const std::string &parameterName) {
        try {
            return convert2<value_t, container_t>(c);
        }
        catch(const std::exception& e) {
            OH_FAIL("Error converting parameter '" << parameterName << "' : '" << e.what());
        }
    }

    //! Convert the input value from type container_t to type value_t.
    /*! This function allows the caller to provide a default value in the event
        that the input is missing, which on Excel is true for inputs of type \#NA.
    */
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

    //! Convert the input value from type container_t to type value_t.
    /*! This function allows the caller to provide an error value which will be
        returned in the case of an exception.  This function does not throw.

        An error may occur in two cases:

        \li If the input value has an error state.  On Excel this is true for \#NUM, \#REF, etc.
        \li If the attempted conversion throws.
    */
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
    //@}
}

#endif

