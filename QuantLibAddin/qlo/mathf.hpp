/*
 Copyright (C) 2006 Ferdinando Ametrano

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


#ifndef qla_mathf_hpp
#define qla_mathf_hpp

#include <oh/objhandler.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

namespace QuantLibAddin {

    inline double normDist(double x,
                         double mean,
                         double stdDev,
                         bool cumulative) {
        if (cumulative) {
            return QuantLib::CumulativeNormalDistribution(mean, stdDev)(x);
        } else {
            return QuantLib::NormalDistribution(mean, stdDev)(x);
        }
    }

    inline double normSDist(double x) {
        return QuantLib::CumulativeNormalDistribution(0.0, 1.0)(x);
    }

    inline double normInv(double prob,
                        double mean,
                        double stdDev) {
        return QuantLib::InverseCumulativeNormal(mean, stdDev)(prob);
    }

    inline double normSInv(double prob) {
        return QuantLib::InverseCumulativeNormal(0.0, 1.0)(prob);
    }

}

#endif
