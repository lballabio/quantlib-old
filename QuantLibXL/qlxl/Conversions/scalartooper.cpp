
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#include <qlxl/Conversions/scalartooper.hpp>
#include <ohxl/Conversions/scalartooper.hpp>
#include <ql/utilities/null.hpp>

namespace ObjectHandler {

    void scalarToOper(const QuantLib::InterestRate &value, OPER &xScalar, bool dllToFree, bool expandVector) {
        scalarToOper(value.rate(), xScalar);
    }

    void scalarToOper(const QuantLib::Date &value, OPER &xScalar, bool dllToFree, bool expandVector) {
        scalarToOper(value.serialNumber(), xScalar);
    }

    void scalarToOper(const QuantLib::Frequency &value, OPER &xScalar, bool dllToFree, bool expandVector) {
        std::ostringstream s;
        s << value;
        scalarToOper(s.str(), xScalar);
    }

    void scalarToOper2(const QuantLib::Real &value, OPER &xScalar) {
        if (abs(value) == QuantLib::Null<QuantLib::Real>()) {
            xScalar.xltype = xltypeErr;
            xScalar.val.err = xlerrNA;
        } else
            scalarToOper(value, xScalar);
    }

}

