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
%include types.i

%{
using QuantLib::Cap;
using QuantLib::Floor;
using QuantLib::Collar;
typedef boost::shared_ptr<Instrument> CapPtr;
typedef boost::shared_ptr<Instrument> FloorPtr;
typedef boost::shared_ptr<Instrument> CollarPtr;
%}

%rename(Cap) CapPtr;
class CapPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        CapPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
               const std::vector<Rate>& capRates,
               const RelinkableHandle<TermStructure>& h,
               const boost::shared_ptr<PricingEngine>& engine) {
            return new CapPtr(new Cap(leg,capRates,h,engine));
        }
    }
};

%rename(Floor) FloorPtr;
class FloorPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        FloorPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
                 const std::vector<Rate>& floorRates,
                 const RelinkableHandle<TermStructure>& h,
                 const boost::shared_ptr<PricingEngine>& engine) {
            return new FloorPtr(new Floor(leg,floorRates,h,engine));
        }
    }
};

%rename(Collar) CollarPtr;
class CollarPtr : public boost::shared_ptr<Instrument> {
  public:
    %extend {
        CollarPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
                  const std::vector<Rate>& capRates,
                  const std::vector<Rate>& floorRates,
                  const RelinkableHandle<TermStructure>& h,
                  const boost::shared_ptr<PricingEngine>& engine) {
            return new CollarPtr(new Collar(leg,capRates,floorRates,h,engine));
        }
    }
};

%{
using QuantLib::BlackCapFloorEngine;
typedef boost::shared_ptr<PricingEngine> BlackCapFloorEnginePtr;
%}

%rename(BlackCapFloorEngine) BlackCapFloorEnginePtr;
class BlackCapFloorEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BlackCapFloorEnginePtr(const boost::shared_ptr<BlackModel>& model) {
            return new BlackCapFloorEnginePtr(new BlackCapFloorEngine(model));
        }
    }
};


#endif
