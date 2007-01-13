
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

#include <qlxl/Conversions/opertomatrix.hpp>

namespace ObjHandler {

    QuantLib::Matrix operToMatrix(const FP &fpVector) {
        QuantLib::Matrix m(fpVector.rows, fpVector.columns);
        for (int i=0; i<fpVector.rows; i++)
            for (int j=0; j<fpVector.columns; j++)
                m[i][j] = fpVector.array[i * fpVector.columns + j];
        return m;
    }

    //std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >
    //fpToMatrixHandle(const FP &fpVector) {
    //    std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > > ret;
    //    for (int i=0; i<fpVector.rows; i++) {
    //        std::vector<QuantLib::Handle<QuantLib::Quote> > row;
    //        for (int j=0; j<fpVector.columns; j++) {
    //            row.push_back(
    //                QuantLib::Handle<QuantLib::Quote>(
    //                boost::shared_ptr<QuantLib::Quote>(
    //                new QuantLib::SimpleQuote(
    //                fpVector.array[i*fpVector.columns+j]))));
    //        }
    //        ret.push_back(row);
    //    }
    //    return ret;
    //}

}

