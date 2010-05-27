/*
 Copyright (C) 2000-2010 RiskMap srl

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

%include options.i
%include marketelements.i
%include termstructures.i
%include cashflows.i
%include volatilities.i

%{
  using QuantLib::Seasonality;
  using QuantLib::MultiplicativePriceSeasonality;
  using QuantLib::InflationTermStructure;
  using QuantLib::YoYInflationTermStructure;
%}

class Seasonality {
 public:
  virtual Rate correctZeroRate(const Date &d, const Rate r,
		       const InflationTermStructure& iTS) const = 0;
  virtual Rate correctYoYRate(const Date &d, const Rate r,
		      const InflationTermStructure& iTS) const = 0;
  virtual bool isConsistent(const InflationTermStructure& iTS);
};

class  MultiplicativePriceSeasonality : public Seasonality {
 public:
  MultiplicativePriceSeasonality();

  MultiplicativePriceSeasonality(const Date& seasonalityBaseDate,
				 const Frequency frequency,
				 const std::vector<Rate> seasonalityFactors);

  void set(const Date& seasonalityBaseDate, const Frequency frequency,
	   const std::vector<Rate> seasonalityFactors);
  
  Date seasonalityBaseDate() const;
  Frequency frequency() const;
  std::vector<Rate> seasonalityFactors() const;
  //! The factor returned is NOT normalized relative to ANYTHING.
  Rate seasonalityFactor(const Date &d) const;
};

%{
%}

%ignore InflationTermStructure;
class InflationTermStructure : public Extrapolator {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("observation-lag")     observationLag;
    %rename("frequency") frequency;
    %rename("index-is-interpolated")   indexIsInterpolated;
    %rename("max-time")        baseRate;
    %rename("zero-rate")       nominalTermStructure;
    %rename("base-date")    baseDate;
    %rename("has-seasonality")    hasSeasonality;
    #endif
  public:
    virtual Period observationLag() const;
    virtual Frequency frequency() const;
    virtual bool indexIsInterpolated() const;
    virtual Rate baseRate() const;
    virtual Handle<YieldTermStructure> nominalTermStructure() const;
    virtual Date baseDate() const = 0;
    virtual bool hasSeasonality() const;
};

%template(InflationTermStructure) boost::shared_ptr<InflationTermStructure>;
IsObservable(boost::shared_ptr<InflationTermStructure>);

%template(InflationTermStructureHandle) Handle<InflationTermStructure>;
IsObservable(Handle<InflationTermStructure>);
%template(RelinkableInflationTermStructureHandle)
RelinkableHandle<InflationTermStructure>;


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
        Volatility impliedVolatility(Real price,
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
        YoYInflationCapPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
               const std::vector<Rate>& capRates) {
            return new YoYInflationCapPtr(new YoYInflationCap(leg,capRates));
        }
    }
};

%rename(YoYInflationFloor) YoYInflationFloorPtr;
class YoYInflationFloorPtr : public YoYInflationCapFloorPtr {
  public:
    %extend {
        YoYInflationFloorPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
                 const std::vector<Rate>& floorRates) {
            return new YoYInflationFloorPtr(new YoYInflationFloor(leg,floorRates));
        }
    }
};

%rename(YoYInflationCollar) YoYInflationCollarPtr;
class YoYInflationCollarPtr : public YoYInflationCapFloorPtr {
  public:
    %extend {
        YoYInflationCollarPtr(const std::vector<boost::shared_ptr<CashFlow> >& leg,
                  const std::vector<Rate>& capRates,
                  const std::vector<Rate>& floorRates) {
            return new YoYInflationCollarPtr(new YoYInflationCollar(leg,capRates,floorRates));
        }
    }
};
#endif
