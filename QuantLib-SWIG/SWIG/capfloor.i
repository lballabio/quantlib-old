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

#ifndef quantlib_cap_floor_i
#define quantlib_cap_floor_i

%include options.i
%include marketelements.i
%include termstructures.i
%include cashflows.i
%include blackmodel.i

%{
using QuantLib::Cap;
using QuantLib::Floor;
using QuantLib::Collar;
typedef Handle<Instrument> CapHandle;
typedef Handle<Instrument> FloorHandle;
typedef Handle<Instrument> CollarHandle;
%}

%rename(Cap) CapHandle;
class CapHandle : public Handle<Instrument> {
  public:
    %extend {
        CapHandle(const std::vector<Handle<CashFlow> >& leg,
                  const std::vector<double>& capRates,
                  const RelinkableHandle<TermStructure>& h,
                  const Handle<PricingEngine>& engine) {
            return new CapHandle(
                new Cap(leg,capRates,h,engine));
        }
    }
};

%rename(Floor) FloorHandle;
class FloorHandle : public Handle<Instrument> {
  public:
    %extend {
        FloorHandle(const std::vector<Handle<CashFlow> >& leg,
                    const std::vector<double>& floorRates,
                    const RelinkableHandle<TermStructure>& h,
                    const Handle<PricingEngine>& engine) {
            return new FloorHandle(
                new Floor(leg,floorRates,h,engine));
        }
    }
};

%rename(Collar) CollarHandle;
class CollarHandle : public Handle<Instrument> {
  public:
    %extend {
        CollarHandle(const std::vector<Handle<CashFlow> >& leg,
                     const std::vector<double>& capRates,
                     const std::vector<double>& floorRates,
                     const RelinkableHandle<TermStructure>& h,
                     const Handle<PricingEngine>& engine) {
            return new CollarHandle(
                new Collar(leg,capRates,floorRates, h,engine));
        }
    }
};

%{
using QuantLib::BlackCapFloor;
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
