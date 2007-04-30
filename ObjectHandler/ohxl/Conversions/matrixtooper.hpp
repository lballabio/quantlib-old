
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

#ifndef ohxl_conversions_matrixtooper_hpp
#define ohxl_conversions_matrixtooper_hpp

#include <ohxl/Conversions/scalartooper.hpp>
#include <vector>

namespace ObjectHandler {

    template <class T>
    void matrixToOper(const std::vector<std::vector<T> > &vv, OPER &xMatrix) {
        if (vv.empty() || vv[0].empty()) {
            xMatrix.xltype = xltypeErr;
            xMatrix.val.err = xlerrNA;
            return;
        }
        xMatrix.val.array.rows    = vv.size();
        xMatrix.val.array.columns = vv[0].size();
        xMatrix.val.array.lparray = new OPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
        xMatrix.xltype = xltypeMulti | xlbitDLLFree;
        for (unsigned int i=0; i<vv.size(); ++i) {
            std::vector<T> v = vv[i];
            for (unsigned int j=0; j<v.size(); ++j)
                scalarToOper(v[j], xMatrix.val.array.lparray[i * v.size() + j]);
        }
    }

}

#endif

