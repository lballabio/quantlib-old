
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_swaption_i
#define quantlib_swaption_i

%include options.i
%include marketelements.i
%include termstructures.i
%include swap.i
%include blackmodel.i

%{
using QuantLib::Instruments::Swaption;
typedef Handle<Instrument> SwaptionHandle;
%}

%rename(Swaption) SwaptionHandle;
class SwaptionHandle : public Handle<Instrument> {
  public:
    %extend {
        SwaptionHandle(const Handle<Instrument>& simpleSwap,
                       const Exercise& exercise,
                       const RelinkableHandle<TermStructure>& termStructure,
                       const Handle<PricingEngine>& engine) {
            Handle<SimpleSwap> swap = simpleSwap;
            QL_REQUIRE(!swap.isNull(),
                       "Swaption: simple swap required");
            return new SwaptionHandle(new Swaption(swap,exercise,
                                                   termStructure,engine));
        }
    }
};


// pricing engines

%{
using QuantLib::Pricers::BlackSwaption;
typedef Handle<PricingEngine> BlackSwaptionEngineHandle;
%}

%rename(BlackSwaptionEngine) BlackSwaptionEngineHandle;
class BlackSwaptionEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        BlackSwaptionEngineHandle(const Handle<BlackModel>& model) {
            return new BlackSwaptionEngineHandle(new BlackSwaption(model));
        }
    }
};



#endif
