/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006, 2007, 2009 Ferdinando Ametrano
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

    class YieldTermStructure;

    class OptionletVolatilityStructure;
    class CapFloorTermVolatilityStructure;

    class SwaptionVolatilityStructure;

    class DefaultProbabilityTermStructure;

    class InflationTermStructure;
}

namespace QuantLibAddin {
     
    OH_OBJ_CLASS(TermStructure, Extrapolator);

    OH_OBJ_CLASS(YieldTermStructure, TermStructure);

    OH_OBJ_CLASS(OptionletVolatilityStructure, TermStructure);
    OH_OBJ_CLASS(CapFloorTermVolatilityStructure, TermStructure);

    OH_OBJ_CLASS(SwaptionVolatilityStructure, TermStructure);

    OH_OBJ_CLASS(DefaultProbabilityTermStructure, TermStructure);

    OH_OBJ_CLASS(InflationTermStructure, TermStructure);

}

#endif
