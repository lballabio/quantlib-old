
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

/*! \file
    \brief Support for Excel functions which loop on an input value
*/

#ifndef qlcpp_loop_hpp
#define qlcpp_loop_hpp

#include <vector>

namespace QuantLibAddinCpp {

    template<class LoopFunction, class InputType, class OutputType>
    std::vector<OutputType> loop(
        LoopFunction &loopFunction, 
        const std::vector<InputType> &vIn) {

        std::vector<OutputType> vOut;
        vOut.reserve(vIn.size());
        typename std::vector<InputType>::const_iterator i;
        for (i = vIn.begin(); i != vIn.end(); ++i) {
            vOut.push_back(loopFunction(*i));
        }
        return vOut;
    }

}

#endif

