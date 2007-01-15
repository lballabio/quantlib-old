
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

#ifndef qla_calc_loop_hpp
#define qla_calc_loop_hpp

namespace QuantLibAddin {

    template<class LoopFunction, class InputType, class OutputType>
    void loopIteration(LoopFunction &loopFunction, const ANY &anyIn, ANY &anyOut) {
        InputType inputItem;
        calcToScalar(inputItem, anyIn);
        OutputType returnItem = loopFunction(inputItem);
        scalarToCalc(anyOut, returnItem);
    }

    template<class LoopFunction, class InputType, class OutputType>
    void loop(LoopFunction &loopFunction, const SEQSEQ(ANY) &anyIn, SEQSEQ(ANY) &anyOut) {

        // if the input is a scalar then call the function once & return
        if (anyIn.getLength() == 1 && anyIn[0].getLength() == 1) {
            anyOut.realloc(1);
            anyOut[0].realloc(1);
            loopIteration<LoopFunction, InputType, OutputType>(
                loopFunction, anyIn[0][0], anyOut[0][0]);
            return;
        }

        anyOut.realloc(anyIn.getLength());
        for (int i=0; i<anyIn.getLength(); i++) {
            anyOut[i].realloc(anyIn[i].getLength());
            for (int j=0; j<anyIn[i].getLength(); j++) {
                loopIteration<LoopFunction, InputType, OutputType>(
                    loopFunction, anyIn[i][j], anyOut[i][j]);
            }
        }
    }
}

#endif

