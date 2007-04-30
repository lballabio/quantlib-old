
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef ohxl_conversions_scalartooper_hpp
#define ohxl_conversions_scalartooper_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <string>
#include <boost/any.hpp>

namespace ObjectHandler {

    // FIXME emergency hack to support automatic coercion of input strings 
    // to vectors thru a call to ohSplit().  Normally the functions below 
    // set xlbitDLLFree but this needs to be disabled in the above case.
    // This is accomplished with input variable dllToFree which defaults
    // to true to preserve the old behavior.

    DLL_API void scalarToOper(const long &value, OPER &xLong, bool dllToFree = true);
    DLL_API void scalarToOper(const double &value, OPER &xDouble, bool dllToFree = true);
    DLL_API void scalarToOper(const bool &value, OPER &xBoolean, bool dllToFree = true);
    DLL_API void scalarToOper(const std::string &value, OPER &xString, bool dllToFree = true);
    DLL_API void scalarToOper(const boost::any &value, OPER &xAny, bool dllToFree = true);

}

#endif
