
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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
    \brief Miscellaneous validation routines
*/

#ifndef ohxl_conversions_validations_hpp
#define ohxl_conversions_validations_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <string>

namespace ObjectHandler {

    //! Determine whether the given range contains an error value.
    DLL_API void validateRange(const OPER *xRange, const std::string &name);
    //! Convert the std::string to the char*.  
    /*! Assumes that the caller has allocated XL_MAX_STR_LEN bytes for the char *.
    */
    DLL_API void stringToChar(const std::string&, char*);

}

#endif

