/*
 Copyright (C) 2010 Joseph Wang
 Copyright (C) 2010, 2011, 2014 StatPro Italia srl

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
    %rename("day-counter")            dayCounter;
    %rename("reference-date")         referenceDate;
    %rename("max-date")               maxDate;
    %rename("max-time")               maxTime;
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
    DayCounter dayCounter() const;
    Calendar calendar() const;
    Date referenceDate() const;
    Date maxDate() const;
    Time maxTime() const;
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



// inflation indexes

%fragment("zeroinflationindex", "header") {
%#include <ql/indexes/inflationindex.hpp>
%#include <ql/indexes/inflation/all.hpp>
using QuantLib::Region;
using QuantLib::CustomRegion;
using QuantLib::InflationIndex;
using QuantLib::ZeroInflationIndex;
using QuantLib::YoYInflationIndex;
typedef boost::shared_ptr<Index> InflationIndexPtr;
typedef boost::shared_ptr<Index> ZeroInflationIndexPtr;
typedef boost::shared_ptr<Index> YoYInflationIndexPtr;
}
%fragment("zeroinflationindex");

class Region {
  public:
    std::string name() const;
    std::string code() const;
  protected:
    Region();
};

class CustomRegion : public Region {
  public:
    CustomRegion(const std::string& name,
                 const std::string& code);
};

%rename(InflationIndex) InflationIndexPtr;
class InflationIndexPtr : public boost::shared_ptr<Index> {
  protected:
    InflationIndexPtr();
  public:
    %extend {
        bool interpolated() const {
            return boost::dynamic_pointer_cast<InflationIndex>(*self)
                ->interpolated();
        }
        Frequency frequency() const {
            return boost::dynamic_pointer_cast<InflationIndex>(*self)
                ->frequency();
        }
        Period availabilityLag() const {
            return boost::dynamic_pointer_cast<InflationIndex>(*self)
                ->availabilityLag();
        }
        Currency currency() const {
            return boost::dynamic_pointer_cast<InflationIndex>(*self)
                ->currency();
        }
    }
};

%rename(ZeroInflationIndex) ZeroInflationIndexPtr;
class ZeroInflationIndexPtr : public InflationIndexPtr {
  public:
    %extend {
      ZeroInflationIndexPtr(const std::string& familyName,
                            const Region& region,
                            bool revised,
                            bool interpolated,
                            Frequency frequency,
                            const Period& availabilityLag,
                            const Currency& currency,
                            const Handle<ZeroInflationTermStructure>& h =
                                       Handle<ZeroInflationTermStructure>()) {
          return new ZeroInflationIndexPtr(
              new ZeroInflationIndex(familyName, region, revised, interpolated,
                                     frequency, availabilityLag, currency, h));
      }
    }
};

%rename(YoYInflationIndex) YoYInflationIndexPtr;
class YoYInflationIndexPtr : public InflationIndexPtr {
  protected:
    YoYInflationIndexPtr();
};

%define export_zii_instance(Name)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public ZeroInflationIndexPtr {
  public:
    %extend {
      Name##Ptr(bool interpolated,
                const Handle<ZeroInflationTermStructure>& h =
                                    Handle<ZeroInflationTermStructure>()) {
          return new Name##Ptr(new Name(interpolated,h));
      }
    }
};
%enddef

%define export_yii_instance(Name)
%fragment("Name","header") {
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
}
%fragment("Name");
%rename(Name) Name##Ptr;
class Name##Ptr : public YoYInflationIndexPtr {
  public:
    %extend {
      Name##Ptr(bool interpolated,
                const Handle<YoYInflationTermStructure>& h =
                                    Handle<YoYInflationTermStructure>()) {
          return new Name##Ptr(new Name(interpolated,h));
      }
    }
};
%enddef

export_zii_instance(EUHICP);
export_zii_instance(EUHICPXT);
export_zii_instance(FRHICP);
export_zii_instance(UKRPI);
export_zii_instance(USCPI);
export_zii_instance(ZACPI);

export_yii_instance(YYEUHICP);
export_yii_instance(YYEUHICPXT);
export_yii_instance(YYFRHICP);
export_yii_instance(YYUKRPI);
export_yii_instance(YYUSCPI);
export_yii_instance(YYZACPI);

// utilities

%{
    using QuantLib::CPI;
%}

struct CPI {
    enum InterpolationType { AsIndex, Flat, Linear };
};

// bootstrapped curves

%{
typedef QuantLib::BootstrapHelper<ZeroInflationTermStructure> ZeroHelper;
typedef QuantLib::BootstrapHelper<YoYInflationTermStructure> YoYHelper;

using QuantLib::ZeroCouponInflationSwapHelper;
typedef boost::shared_ptr<ZeroHelper> ZeroCouponInflationSwapHelperPtr;

using QuantLib::YearOnYearInflationSwapHelper;
typedef boost::shared_ptr<YoYHelper> YearOnYearInflationSwapHelperPtr;
%}

%template(ZeroHelper) boost::shared_ptr<ZeroHelper>;
%template(YoYHelper) boost::shared_ptr<YoYHelper>;

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<ZeroHelper> )
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<YoYHelper> )
#endif
namespace std {
    %template(ZeroHelperVector) vector<boost::shared_ptr<ZeroHelper> >;
    %template(YoYHelperVector) vector<boost::shared_ptr<YoYHelper> >;
}

%rename(ZeroCouponInflationSwapHelper) ZeroCouponInflationSwapHelperPtr;
class ZeroCouponInflationSwapHelperPtr : public boost::shared_ptr<ZeroHelper> {
  public:
    %extend {
        ZeroCouponInflationSwapHelperPtr(Rate rate,
                                         const Period& lag,
                                         const Date& maturity,
                                         const Calendar& calendar,
                                         BusinessDayConvention bdc,
                                         const DayCounter& dayCounter,
                                         const ZeroInflationIndexPtr& index) {
            Handle<Quote> quote(
                boost::shared_ptr<Quote>(new SimpleQuote(rate)));
            boost::shared_ptr<ZeroInflationIndex> zeroIndex =
                boost::dynamic_pointer_cast<ZeroInflationIndex>(index);
            return new ZeroCouponInflationSwapHelperPtr(
                new ZeroCouponInflationSwapHelper(quote,lag,maturity,
                                                  calendar,bdc,
                                                  dayCounter,zeroIndex));
        }
    }
};

%rename(YearOnYearInflationSwapHelper) YearOnYearInflationSwapHelperPtr;
class YearOnYearInflationSwapHelperPtr : public boost::shared_ptr<YoYHelper> {
  public:
    %extend {
        YearOnYearInflationSwapHelperPtr(Rate rate,
                                         const Period& lag,
                                         const Date& maturity,
                                         const Calendar& calendar,
                                         BusinessDayConvention bdc,
                                         const DayCounter& dayCounter,
                                         const YoYInflationIndexPtr& index) {
            Handle<Quote> quote(
                boost::shared_ptr<Quote>(new SimpleQuote(rate)));
            boost::shared_ptr<YoYInflationIndex> yoyIndex =
                boost::dynamic_pointer_cast<YoYInflationIndex>(index);
            return new YearOnYearInflationSwapHelperPtr(
                new YearOnYearInflationSwapHelper(quote,lag,maturity,
                                                  calendar,bdc,
                                                  dayCounter,yoyIndex));
        }
    }
};


%{
using QuantLib::PiecewiseZeroInflationCurve;
using QuantLib::PiecewiseYoYInflationCurve;
%}

%define export_piecewise_zero_inflation_curve(Name,Interpolator)
%{
typedef boost::shared_ptr<ZeroInflationTermStructure> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<ZeroInflationTermStructure> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") Name##Ptr;
    #endif
  public:
    %extend {
        Name##Ptr(
              const Date& referenceDate,
              const Calendar& calendar,
              const DayCounter& dayCounter,
              const Period& lag,
              Frequency frequency,
              bool indexIsInterpolated,
              Rate baseRate,
              const Handle<YieldTermStructure>& nominalTS,
              const std::vector<boost::shared_ptr<ZeroHelper> >& instruments,
              Real accuracy = 1.0e-12,
              const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseZeroInflationCurve<Interpolator>(
                                        referenceDate, calendar, dayCounter,
                                        lag, frequency, indexIsInterpolated,
                                        baseRate, nominalTS, instruments,
                                        accuracy, i));
        }
        const std::vector<Date>& dates() {
            typedef PiecewiseZeroInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Time>& times() {
            typedef PiecewiseZeroInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->times();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef PiecewiseZeroInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};
%enddef


%define export_piecewise_yoy_inflation_curve(Name,Interpolator)
%{
typedef boost::shared_ptr<YoYInflationTermStructure> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<YoYInflationTermStructure> {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") Name##Ptr;
    #endif
  public:
    %extend {
        Name##Ptr(
              const Date& referenceDate,
              const Calendar& calendar,
              const DayCounter& dayCounter,
              const Period& lag,
              Frequency frequency,
              bool indexIsInterpolated,
              Rate baseRate,
              const Handle<YieldTermStructure>& nominalTS,
              const std::vector<boost::shared_ptr<YoYHelper> >& instruments,
              Real accuracy = 1.0e-12,
              const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseYoYInflationCurve<Interpolator>(
                                        referenceDate, calendar, dayCounter,
                                        lag, frequency, indexIsInterpolated,
                                        baseRate, nominalTS, instruments,
                                        accuracy, i));
        }
        const std::vector<Date>& dates() {
            typedef PiecewiseYoYInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Time>& times() {
            typedef PiecewiseYoYInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->times();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef PiecewiseYoYInflationCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};
%enddef

export_piecewise_zero_inflation_curve(PiecewiseZeroInflation,Linear);

export_piecewise_yoy_inflation_curve(PiecewiseYoYInflation,Linear);


// utilities

%inline %{

    Date inflationBaseDate(const Date& referenceDate,
                           const Period& observationLag,
                           Frequency frequency,
                           bool indexIsInterpolated) {
        if (indexIsInterpolated) {
            return referenceDate - observationLag;
        } else {
            return QuantLib::inflationPeriod(referenceDate - observationLag,
                                             frequency).first;
        }
    }

%}


// inflation instruments

%{
using QuantLib::ZeroCouponInflationSwap;
using QuantLib::YearOnYearInflationSwap;
typedef boost::shared_ptr<Instrument> ZeroCouponInflationSwapPtr;
typedef boost::shared_ptr<Instrument> YearOnYearInflationSwapPtr;
%}

#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_ZeroCouponInflationSwap) ZeroCouponInflationSwap;
#else
%ignore ZeroCouponInflationSwap;
#endif
class ZeroCouponInflationSwap {
  public:
    enum Type { Receiver = -1, Payer = 1 };
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    ZeroCouponInflationSwap();
#endif
};

%rename(ZeroCouponInflationSwap) ZeroCouponInflationSwapPtr;
class ZeroCouponInflationSwapPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    #endif
  public:
    %extend {
        static const ZeroCouponInflationSwap::Type Receiver =
            ZeroCouponInflationSwap::Receiver;
        static const ZeroCouponInflationSwap::Type Payer =
            ZeroCouponInflationSwap::Payer;
        ZeroCouponInflationSwapPtr(
                       ZeroCouponInflationSwap::Type type,
                       Real nominal,
                       const Date& start,
                       const Date& maturity,
                       const Calendar& calendar,
                       BusinessDayConvention convention,
                       const DayCounter& dayCounter,
                       Rate fixedRate,
                       const ZeroInflationIndexPtr& index,
                       const Period& lag,
                       bool adjustInfObsDates = false,
                       Calendar infCalendar = Calendar(),
                       BusinessDayConvention infConvention = Following) {
            boost::shared_ptr<ZeroInflationIndex> zeroIndex =
                boost::dynamic_pointer_cast<ZeroInflationIndex>(index);
            return new ZeroCouponInflationSwapPtr(
                new ZeroCouponInflationSwap(type, nominal, start, maturity,
                                            calendar, convention, dayCounter,
                                            fixedRate, zeroIndex, lag,
                                            adjustInfObsDates,
                                            infCalendar, infConvention));
        }
        Rate fairRate() {
            return boost::dynamic_pointer_cast<ZeroCouponInflationSwap>(*self)
                ->fairRate();
        }
        std::vector<boost::shared_ptr<CashFlow> > fixedLeg() {
            return boost::dynamic_pointer_cast<ZeroCouponInflationSwap>(*self)
                ->fixedLeg();
        }
        std::vector<boost::shared_ptr<CashFlow> > inflationLeg() {
            return boost::dynamic_pointer_cast<ZeroCouponInflationSwap>(*self)
                ->inflationLeg();
        }
        ZeroCouponInflationSwap::Type type() {
            return boost::dynamic_pointer_cast<ZeroCouponInflationSwap>(*self)
                ->type();
        }
    }
};


#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_YearOnYearInflationSwap) YearOnYearInflationSwap;
#else
%ignore YearOnYearInflationSwap;
#endif
class YearOnYearInflationSwap {
  public:
    enum Type { Receiver = -1, Payer = 1 };
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    YearOnYearInflationSwap();
#endif
};

%rename(YearOnYearInflationSwap) YearOnYearInflationSwapPtr;
class YearOnYearInflationSwapPtr : public boost::shared_ptr<Instrument> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fair-rate")        fairRate;
    #endif
  public:
    %extend {
        static const YearOnYearInflationSwap::Type Receiver =
            YearOnYearInflationSwap::Receiver;
        static const YearOnYearInflationSwap::Type Payer =
            YearOnYearInflationSwap::Payer;
        YearOnYearInflationSwapPtr(
                   YearOnYearInflationSwap::Type type,
                   Real nominal,
                   const Schedule& fixedSchedule,
                   Rate fixedRate,
                   const DayCounter& fixedDayCounter,
                   const Schedule& yoySchedule,
                   const YoYInflationIndexPtr& index,
                   const Period& lag,
                   Spread spread,
                   const DayCounter& yoyDayCounter,
                   const Calendar& paymentCalendar,
                   BusinessDayConvention paymentConvention = Following) {
            boost::shared_ptr<YoYInflationIndex> yoyIndex =
                boost::dynamic_pointer_cast<YoYInflationIndex>(index);
            return new YearOnYearInflationSwapPtr(
                new YearOnYearInflationSwap(type, nominal, fixedSchedule,
                                            fixedRate, fixedDayCounter,
                                            yoySchedule, yoyIndex, lag, spread,
                                            yoyDayCounter, paymentCalendar,
                                            paymentConvention));
        }
        Rate fairRate() {
            return boost::dynamic_pointer_cast<YearOnYearInflationSwap>(*self)
                ->fairRate();
        }
    }
};


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
