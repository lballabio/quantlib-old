
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_black_model_i
#define quantlib_black_model_i

%include common.i
%include marketelements.i
%include termstructures.i

%{
using QuantLib::BlackModel;
%}

%template(BlackModel) Handle<BlackModel>;
%extend Handle<BlackModel> {
    Handle<BlackModel>(const RelinkableHandle<MarketElement>& volatility,
                       const RelinkableHandle<TermStructure>& termStructure) {
        return new Handle<BlackModel>(
            new BlackModel(volatility,termStructure));
    }
}


#endif
