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

#ifndef quantlib_cap_floor_i
#define quantlib_cap_floor_i

%include options.i
%include marketelements.i
%include termstructures.i
%include cashflows.i
%include blackmodel.i

%{
using QuantLib::Instruments::VanillaCap;
using QuantLib::Instruments::VanillaFloor;
using QuantLib::Instruments::VanillaCollar;
typedef Handle<Instrument> VanillaCapHandle;
typedef Handle<Instrument> VanillaFloorHandle;
typedef Handle<Instrument> VanillaCollarHandle;
%}

%rename(Cap) VanillaCapHandle;
class VanillaCapHandle : public Handle<Instrument> {
  public:
    %extend {
        VanillaCapHandle(const std::vector<Handle<CashFlow> >& leg,
                         const std::vector<double>& capRates,
                         const RelinkableHandle<TermStructure>& h,
                         const Handle<PricingEngine>& engine) {
            return new VanillaCapHandle(
                new VanillaCap(leg,capRates,h,engine));
        }
    }
};

%rename(Floor) VanillaFloorHandle;
class VanillaFloorHandle : public Handle<Instrument> {
  public:
    %extend {
        VanillaFloorHandle(const std::vector<Handle<CashFlow> >& leg,
                           const std::vector<double>& floorRates,
                           const RelinkableHandle<TermStructure>& h,
                           const Handle<PricingEngine>& engine) {
            return new VanillaFloorHandle(
                new VanillaFloor(leg,floorRates,h,engine));
        }
    }
};

%rename(Collar) VanillaCollarHandle;
class VanillaCollarHandle : public Handle<Instrument> {
  public:
    %extend {
        VanillaCollarHandle(const std::vector<Handle<CashFlow> >& leg,
                            const std::vector<double>& capRates,
                            const std::vector<double>& floorRates,
                            const RelinkableHandle<TermStructure>& h,
                            const Handle<PricingEngine>& engine) {
            return new VanillaCollarHandle(
                new VanillaCollar(leg,capRates,floorRates, h,engine));
        }
    }
};

%{
using QuantLib::Pricers::BlackCapFloor;
typedef Handle<PricingEngine> BlackCapFloorEngineHandle;
%}

%rename(BlackCapFloorEngine) BlackCapFloorEngineHandle;
class BlackCapFloorEngineHandle : public Handle<PricingEngine> {
  public:
    %extend {
        BlackCapFloorEngineHandle(const Handle<BlackModel>& model) {
            return new BlackCapFloorEngineHandle(new BlackCapFloor(model));
        }
    }
};


#endif
