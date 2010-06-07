/*
 Copyright (C) 2010 Joseph Wang
 Copyright (C) 2010 StatPro Italia srl

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

#ifndef quantlib_inflation_i
#define quantlib_inflation_i

%include termstructures.i

%{
  using QuantLib::Seasonality;
  using QuantLib::MultiplicativePriceSeasonality;
  typedef boost::shared_ptr<Seasonality> MultiplicativePriceSeasonalityPtr;
%}

%ignore Seasonality;
class Seasonality {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("correct-zero-rate") correctZeroRate;
    %rename("correct-yoy-rate")  correctYoYRate;
    %rename("is-consistent")     isConsistent;
    #endif
  public:
    virtual Rate correctZeroRate(const Date &d, const Rate r,
                                 const InflationTermStructure& iTS) const = 0;
    virtual Rate correctYoYRate(const Date &d, const Rate r,
                                const InflationTermStructure& iTS) const = 0;
    virtual bool isConsistent(const InflationTermStructure& iTS);
};

%template(Seasonality) boost::shared_ptr<Seasonality>;

class MultiplicativePriceSeasonalityPtr
    : public boost::shared_ptr<Seasonality> {
  public:
    %extend {
        MultiplicativePriceSeasonalityPtr(
                                const Date& seasonalityBaseDate,
                                Frequency frequency,
                                const std::vector<Rate>& seasonalityFactors) {
            return new MultiplicativePriceSeasonalityPtr(
                       new MultiplicativePriceSeasonality(seasonalityBaseDate,
                                                          frequency,
                                                          seasonalityFactors));
        }
    }
};

%{
  using QuantLib::InflationTermStructure;
  using QuantLib::YoYInflationTermStructure;
  using QuantLib::ZeroInflationTermStructure;
%}


%ignore InflationTermStructure;
class InflationTermStructure : public Extrapolator {
    #if defined(SWIGRUBY)
    %rename("indexIsInterpolated?")   indexIsInterpolated;
    %rename("setSeasonality!")        setSeasonality;
    %rename("hasSeasonality?")        hasSeasonality;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("observation-lag")        observationLag;
    %rename("frequency")              frequency;
    %rename("index-is-interpolated?") indexIsInterpolated;
    %rename("base-rate")              baseRate;
    %rename("nominal-term-structure") nominalTermStructure;
    %rename("base-date")              baseDate;
    %rename("seasonality-set!")       setSeasonality;
    %rename("has-seasonality?")       hasSeasonality;
    #endif
  public:
    virtual Period observationLag() const;
    virtual Frequency frequency() const;
    virtual bool indexIsInterpolated() const;
    virtual Rate baseRate() const;
    virtual Handle<YieldTermStructure> nominalTermStructure() const;
    virtual Date baseDate() const = 0;
    void setSeasonality(const boost::shared_ptr<Seasonality>& seasonality =
                                            boost::shared_ptr<Seasonality>());
    boost::shared_ptr<Seasonality> seasonality() const;
    bool hasSeasonality() const;
};

%ignore YoYInflationTermStructure;
class YoYInflationTermStructure : public InflationTermStructure {
  public:
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("yoy-rate") yoyRate;
    #endif
    Rate yoyRate(const Date &d, const Period& instObsLag = Period(-1,Days),
                 bool forceLinearInterpolation = false,
                 bool extrapolate = false) const;
};

%template(YoYInflationTermStructure)
    boost::shared_ptr<YoYInflationTermStructure>;
IsObservable(boost::shared_ptr<YoYInflationTermStructure>);

%template(YoYInflationTermStructureHandle) Handle<YoYInflationTermStructure>;
IsObservable(Handle<YoYInflationTermStructure>);
%template(RelinkableYoYInflationTermStructureHandle)
    RelinkableHandle<YoYInflationTermStructure>;


%ignore ZeroInflationTermStructure;
class ZeroInflationTermStructure : public InflationTermStructure {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("zero-rate") zeroRate;
    #endif
  public:
    Rate zeroRate(const Date &d, const Period& instObsLag = Period(-1,Days),
                  bool forceLinearInterpolation = false,
                  bool extrapolate = false) const;
};

%template(ZeroInflationTermStructure)
    boost::shared_ptr<ZeroInflationTermStructure>;
IsObservable(boost::shared_ptr<ZeroInflationTermStructure>);

%template(ZeroInflationTermStructureHandle) Handle<ZeroInflationTermStructure>;
IsObservable(Handle<ZeroInflationTermStructure>);
%template(RelinkableZeroInflationTermStructureHandle)
    RelinkableHandle<ZeroInflationTermStructure>;


%{
using QuantLib::YoYInflationCapFloor;
using QuantLib::YoYInflationCap;
using QuantLib::YoYInflationFloor;
using QuantLib::YoYInflationCollar;

typedef boost::shared_ptr<Instrument> YoYInflationCapFloorPtr;
typedef boost::shared_ptr<Instrument> YoYInflationCapPtr;
typedef boost::shared_ptr<Instrument> YoYInflationFloorPtr;
typedef boost::shared_ptr<Instrument> YoYInflationCollarPtr;
%}

%rename(YoYInflationCapFloor) YoYInflationCapFloorPtr;
class YoYInflationCapFloorPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("implied-volatility") impliedVolatility;
    #endif
  public:
     %extend {
         Volatility impliedVolatility(
                               Real price,
                               const Handle<YoYInflationTermStructure>& curve,
                               Volatility guess,
                               Real accuracy = 1.0e-4,
                               Size maxEvaluations = 100,
                               Volatility minVol = 1.0e-7,
                               Volatility maxVol = 4.0) const {
             return boost::dynamic_pointer_cast<YoYInflationCapFloor>(*self)->
                 impliedVolatility(price, curve, guess, accuracy,
                                   maxEvaluations, minVol, maxVol);
         }
     }
};

%rename(YoYInflationCap) YoYInflationCapPtr;
class YoYInflationCapPtr : public YoYInflationCapFloorPtr {
  public:
    %extend {
        YoYInflationCapPtr(
                const std::vector<boost::shared_ptr<CashFlow> >& leg,
                const std::vector<Rate>& capRates) {
            return new YoYInflationCapPtr(new YoYInflationCap(leg,capRates));
        }
    }
};

%rename(YoYInflationFloor) YoYInflationFloorPtr;
class YoYInflationFloorPtr : public YoYInflationCapFloorPtr {
  public:
    %extend {
        YoYInflationFloorPtr(
                const std::vector<boost::shared_ptr<CashFlow> >& leg,
                const std::vector<Rate>& floorRates) {
            return new YoYInflationFloorPtr(
                                       new YoYInflationFloor(leg,floorRates));
        }
    }
};

%rename(YoYInflationCollar) YoYInflationCollarPtr;
class YoYInflationCollarPtr : public YoYInflationCapFloorPtr {
  public:
    %extend {
        YoYInflationCollarPtr(
                const std::vector<boost::shared_ptr<CashFlow> >& leg,
                const std::vector<Rate>& capRates,
                const std::vector<Rate>& floorRates) {
            return new YoYInflationCollarPtr(
                             new YoYInflationCollar(leg,capRates,floorRates));
        }
    }
};

#endif
