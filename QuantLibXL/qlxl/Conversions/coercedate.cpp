
/*
 Copyright (C) 2006 Eric Ehlers

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

#include <qlxl/Conversions/coercedate.hpp>
#include <qlxl/Conversions/opertoscalar.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <qlo/typefactory.hpp>

namespace ObjHandler {

    bool operToDate(
            const OPER &in,
            QuantLib::Date &out) {
        if ((in.xltype & (xltypeMissing | xltypeNil | xltypeErr)))
            OH_FAIL("input value is invalid");
        if (in.xltype & xltypeNum) {
            out = QuantLib::Date(in.val.num);
            return true;
        } else if (in.xltype & xltypeStr) {
            std::string periodStr;
            operToScalar(in, periodStr);
            QuantLib::Period period;
            cppToLibrary(periodStr, period);
            QuantLib::Date date = QuantLib::Settings::instance().evaluationDate();
            out = date + period;
            return true;
        } else {
            return false;
        }
    }

    bool CoerceDate::inputMissing(const OPER &in) {
        return ((in.xltype & xltypeNil)
            ||  (in.xltype & xltypeMissing)
            || ((in.xltype & xltypeErr) && (in.val.err == xlerrNA)));
    }

}
