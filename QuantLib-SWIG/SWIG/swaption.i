
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_swaption_i
#define quantlib_swaption_i

%include options.i
%include marketelements.i
%include termstructures.i
%include swap.i
%include blackmodel.i

%{
using QuantLib::Swaption;
typedef Handle<Instrument> SwaptionHandle;
%}

%rename(Swaption) SwaptionHandle;
class SwaptionHandle : public Handle<Instrument> {
  public:
    %extend {
        SwaptionHandle(const Handle<Instrument>& simpleSwap,
                       const Handle<Exercise>& exercise,
                       const RelinkableHandle<TermStructure>& termStructure,
                       const Handle<PricingEngine>& engine) {
            Handle<SimpleSwap> swap = 
                 boost::dynamic_pointer_cast<SimpleSwap>(simpleSwap);
            QL_REQUIRE(!IsNull(swap),
                       "Swaption: simple swap required");
            return new SwaptionHandle(new Swaption(swap,exercise,
                                                   termStructure,engine));
        }
    }
};


// pricing engines

%{
using QuantLib::BlackSwaption;
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
