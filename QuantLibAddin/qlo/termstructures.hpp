/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006, 2007, 2009, 2010 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_termstructures_hpp
#define qla_termstructures_hpp

#include <qlo/extrapolator.hpp>

namespace QuantLib {

    class OptionletVolatilityStructure;
    class CapFloorTermVolatilityStructure;

    class SwaptionVolatilityStructure;

    class DefaultProbabilityTermStructure;

    class InflationTermStructure;

    class VolatilityTermStructure;

    class YieldTermStructure;

}

namespace QuantLibAddin {
     
    OH_OBJ_CLASS(TermStructure, Extrapolator);
        OH_OBJ_CLASS(YieldTermStructure,              TermStructure);
        OH_OBJ_CLASS(DefaultProbabilityTermStructure, TermStructure);
        OH_OBJ_CLASS(CorrelationTermStructure, TermStructure);
        OH_OBJ_CLASS(InflationTermStructure,          TermStructure);
        OH_OBJ_CLASS(VolatilityTermStructure,         TermStructure);
            OH_OBJ_CLASS(BlackAtmVolCurve,                VolatilityTermStructure);
                OH_OBJ_CLASS(BlackVolSurface, BlackAtmVolCurve);
                    OH_OBJ_CLASS(InterestRateVolSurface, BlackVolSurface);
            OH_OBJ_CLASS(BlackVolTermStructure,           VolatilityTermStructure);
            OH_OBJ_CLASS(SwaptionVolatilityStructure,     VolatilityTermStructure);
                OH_OBJ_CLASS(SwaptionVolatilityDiscrete, SwaptionVolatilityStructure);
                    OH_OBJ_CLASS(SwaptionVolatilityCube, SwaptionVolatilityDiscrete);
            OH_OBJ_CLASS(OptionletVolatilityStructure,    VolatilityTermStructure);
            OH_OBJ_CLASS(CapFloorTermVolatilityStructure, VolatilityTermStructure);
}

#endif
