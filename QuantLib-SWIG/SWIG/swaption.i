
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
typedef boost::shared_ptr<Instrument> SwaptionPtr;
%}

%rename(Swaption) SwaptionPtr;
class SwaptionPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        SwaptionPtr(const boost::shared_ptr<Instrument>& simpleSwap,
                    const boost::shared_ptr<Exercise>& exercise,
                    const Handle<YieldTermStructure>& termStructure,
                    const boost::shared_ptr<PricingEngine>& engine) {
            boost::shared_ptr<SimpleSwap> swap =
                 boost::dynamic_pointer_cast<SimpleSwap>(simpleSwap);
            QL_REQUIRE(swap, "simple swap required");
            return new SwaptionPtr(new Swaption(swap,exercise,
                                                termStructure,engine));
        }
    }
};


// pricing engines

%{
using QuantLib::BlackSwaptionEngine;
typedef boost::shared_ptr<PricingEngine> BlackSwaptionEnginePtr;
%}

%rename(BlackSwaptionEngine) BlackSwaptionEnginePtr;
class BlackSwaptionEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BlackSwaptionEnginePtr(const boost::shared_ptr<BlackModel>& model) {
            return new BlackSwaptionEnginePtr(new BlackSwaptionEngine(model));
        }
    }
};


#endif
