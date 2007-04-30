
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

#include <qlxl/Conversions/matrixtooper.hpp>
#include <ohxl/Conversions/scalartooper.hpp>
#include <oh/exception.hpp>

namespace ObjectHandler {

    void matrixToOper(const QuantLib::Matrix &m, OPER &xMatrix) {
        if (m.empty()) {
            xMatrix.xltype = xltypeErr;
            xMatrix.val.err = xlerrNA;
            return;
        }
        xMatrix.val.array.rows = m.rows();
        xMatrix.val.array.columns = m.columns();
        xMatrix.val.array.lparray = new OPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
        if (!xMatrix.val.array.lparray)
            throw Exception("matrixToOper: error on call to new");
        xMatrix.xltype = xltypeMulti | xlbitDLLFree;
        for (unsigned int i=0; i<m.rows(); ++i)
            for (unsigned int j=0; j<m.columns(); ++j)
                scalarToOper(m[i][j], xMatrix.val.array.lparray[i * m.columns() + j]);
    }

}

