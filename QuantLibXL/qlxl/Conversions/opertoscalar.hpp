
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qlxl_conversions_opertoscalar_hpp
#define qlxl_conversions_opertoscalar_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <ql/time/date.hpp>
#include <ql/quotes/simplequote.hpp>

namespace ObjectHandler {

    void operToScalar(const OPER &xScalar, QuantLib::Date &ret);
    void operToScalar(const OPER &xScalar, QuantLib::Period &ret);
    void operToScalar(const OPER &xScalar, QuantLib::Size &ret);
    void operToScalar(const OPER &xScalar, QuantLib::RelinkableHandle<QuantLib::Quote> &ret);
    void operToScalar(const OPER &xScalar, boost::shared_ptr<QuantLib::Quote> &ret);

    //void cppToLibrary(const long &in, QuantLib::Date &ret);
    void cppToLibrary(const std::string &in, QuantLib::Period &ret);
    void cppToLibrary(const long &in, QuantLib::Size &ret);

    template <class T, class EnumRegistry>
    T operToScalarEnum(const OPER &xScalar,
                       const std::string &paramName,
                       const std::string &defaultValue) {
        std::string id =
            callOperToScalar<std::string>(xScalar, paramName, defaultValue);
        return EnumRegistry()(id);
    }

}

#endif
