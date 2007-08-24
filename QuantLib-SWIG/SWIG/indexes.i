
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl

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

#ifndef quantlib_indexes_i
#define quantlib_indexes_i

%include date.i
%include calendars.i
%include daycounters.i
%include currencies.i
%include types.i
%include termstructures.i
%include timeseries.i
%include vectors.i

%{
using QuantLib::IndexManager;
%}

class IndexManager {
    #if defined(SWIGRUBY)
    %rename("hasHistory?")  hasHistory;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("has-history?") hasHistory;
    %rename("history-get")  getHistory;
    %rename("history-set!") setHistory;
    #endif
  private:
    IndexManager();
  public:
    static IndexManager& instance();
    void setHistory(const std::string& name, const TimeSeries<Real>& fixings);
    const TimeSeries<Real>& getHistory(const std::string& name) const;
    bool hasHistory(const std::string& name) const;
    std::vector<std::string> histories() const;
};


// base index class

%{
using QuantLib::Index;
%}

%ignore Index;
class Index {
    #if defined(SWIGRUBY)
    %rename("isValidFixingDate?") isValidFixingDate;
    %rename("addFixing!") addFixing;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("fixing-calendar") addFixing;
    %rename("is-valid-fixing-date?") isValidFixingDate;
    %rename("add-fixing") addFixing;
    #endif
  public:
    std::string name() const;
    Calendar fixingCalendar() const;
    bool isValidFixingDate(const Date& fixingDate) const;
    Real fixing(const Date& fixingDate,
                bool forecastTodaysFixing = false) const;
    void addFixing(const Date& fixingDate, Rate fixing);
};

%template(Index) boost::shared_ptr<Index>;
%extend boost::shared_ptr<Index> {
    #if defined(SWIGRUBY)
    %rename("addFixings!") addFixings;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("add-fixings") addFixings;
    #endif
    void addFixings(const std::vector<Date>& fixingDates,
                    const std::vector<Rate>& fixings) {
        (*self)->addFixings(fixingDates.begin(),fixingDates.end(),
                            fixings.begin());
    }
    std::string __str__() {
        if (*self)
            return (*self)->name()+" index";
        else
            return "Null index";
    }
}
IsObservable(boost::shared_ptr<Index>);

// interest-rate indexes
%{
using QuantLib::InterestRateIndex;
typedef boost::shared_ptr<Index> InterestRateIndexPtr;
%}

%rename(InterestRateIndex) InterestRateIndexPtr;
class InterestRateIndexPtr : public boost::shared_ptr<Index> {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("family-name")     familyName;
    %rename("settlement-days") settlementDays;
    %rename("day-counter")     dayCounter;
    #endif
  protected:
    InterestRateIndexPtr();
  public:
    %extend {
        std::string familyName() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->familyName();
        }
        Period tenor() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->tenor();
        }
        Natural fixingDays() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->fixingDays();
        }
        Date fixingDate(const Date& valueDate) {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->fixingDate(valueDate);
        }
        Currency currency() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->currency();
        }
        DayCounter dayCounter() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->dayCounter();
        }
        Rate forecastFixing(const Date& fixingDate) {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->forecastFixing(fixingDate);
        }
        Handle<YieldTermStructure> termStructure() {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->termStructure();
        }
        Date maturityDate(const Date& valueDate) {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->maturityDate(valueDate);
        }
        Date valueDate(const Date& fixingDate) {
            return boost::dynamic_pointer_cast<InterestRateIndex>(*self)
                ->valueDate(fixingDate);
        }
    }
};


// IborIndex indexes
%{
using QuantLib::IborIndex;
typedef boost::shared_ptr<Index> IborIndexPtr;
%}

%rename(IborIndex) IborIndexPtr;
class IborIndexPtr : public InterestRateIndexPtr {
    #if defined(SWIGRUBY)
    %rename("isAdjusted?") isAdjusted;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-adjusted?")            isAdjusted;
    %rename("business-day-convention") businessDayConvention;
    #endif
  public:
    %extend {
        IborIndexPtr(const std::string& familyName,
                     const Period& tenor,
                     Integer settlementDays,
                     const Currency& currency,
                     const Calendar& calendar,
                     BusinessDayConvention convention,
                     bool endOfMonth,
                     const DayCounter& dayCounter,
                     const Handle<YieldTermStructure>& h =
                                    Handle<YieldTermStructure>()) {
            return new IborIndexPtr(new IborIndex(familyName, tenor,
                                                  settlementDays,
                                                  currency, calendar,
                                                  convention,
                                                  endOfMonth,
                                                  dayCounter, h));
        }
        BusinessDayConvention businessDayConvention() {
            return boost::dynamic_pointer_cast<IborIndex>(*self)
                ->businessDayConvention();
        }
        bool endOfMonth() {
            return boost::dynamic_pointer_cast<IborIndex>(*self)->endOfMonth();
        }
    }
};

%define export_xibor_instance(Name)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public IborIndexPtr {
  public:
    %extend {
      Name##Ptr(const Period& tenor,
                const Handle<YieldTermStructure>& h =
                                    Handle<YieldTermStructure>()) {
          return new Name##Ptr(new Name(tenor,h));
      }
    }
};
%enddef

%define export_quoted_xibor_instance(Name,Base)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public Base##Ptr {
  public:
    %extend {
      Name##Ptr(const Handle<YieldTermStructure>& h =
                                    Handle<YieldTermStructure>()) {
          return new Name##Ptr(new Name(h));
      }
    }
};
%enddef


%{
using QuantLib::SwapIndex;
typedef boost::shared_ptr<Index> SwapIndexPtr;
%}

%rename(SwapIndex) SwapIndexPtr;
class SwapIndexPtr : public InterestRateIndexPtr {
    #if defined(SWIGRUBY)
    %rename("isAdjusted?") isAdjusted;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-adjusted?")            isAdjusted;
    %rename("business-day-convention") businessDayConvention;
    #endif
  public:
    %extend {
        SwapIndexPtr(const std::string& familyName,
               const Period& tenor,
               Integer settlementDays,
               Currency& currency,
                           const Calendar& calendar,
               const Period& fixedLegTenor,
               BusinessDayConvention fixedLegConvention,
                     const DayCounter& fixedLegDayCounter,
                     const IborIndexPtr& iborIndex) {
            boost::shared_ptr<IborIndex> xibor =
                boost::dynamic_pointer_cast<IborIndex>(iborIndex);
            return new SwapIndexPtr(new SwapIndex(familyName,
                                                  tenor, settlementDays,
                                                  currency, calendar,
                                                  fixedLegTenor,
                                                  fixedLegConvention,
                                                  fixedLegDayCounter,
                                                  xibor));
        }
        Period fixedLegTenor() {
            return boost::dynamic_pointer_cast<SwapIndex>(*self)
                ->fixedLegTenor();
        }
        BusinessDayConvention fixedLegConvention() {
            return boost::dynamic_pointer_cast<SwapIndex>(*self)
                 ->fixedLegConvention();
        }
        IborIndexPtr iborIndex() {
            return boost::dynamic_pointer_cast<SwapIndex>(*self)
                ->iborIndex();
        }
    }
};

%define export_swap_instance(Name)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public SwapIndexPtr {
  public:
    %extend {
      Name##Ptr(const Period &tenor,
                const Handle<YieldTermStructure>& h =
                                    Handle<YieldTermStructure>()) {
          return new Name##Ptr(new Name(tenor,h));
      }
    }
};
%enddef

%define export_quoted_swap_instance(Name,Base)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public Base##Ptr {
  public:
    %extend {
      Name##Ptr(const Handle<YieldTermStructure>& h =
                                    Handle<YieldTermStructure>()) {
          return new Name##Ptr(new Name(h));
      }
    }
};
%enddef



export_xibor_instance(AUDLibor);
export_xibor_instance(CADLibor);
export_xibor_instance(Cdor);
export_xibor_instance(CHFLibor);
export_xibor_instance(DKKLibor);

export_xibor_instance(Euribor);
export_quoted_xibor_instance(EuriborSW,Euribor);
export_quoted_xibor_instance(Euribor2W,Euribor);
export_quoted_xibor_instance(Euribor3W,Euribor);
export_quoted_xibor_instance(Euribor1M,Euribor);
export_quoted_xibor_instance(Euribor2M,Euribor);
export_quoted_xibor_instance(Euribor3M,Euribor);
export_quoted_xibor_instance(Euribor4M,Euribor);
export_quoted_xibor_instance(Euribor5M,Euribor);
export_quoted_xibor_instance(Euribor6M,Euribor);
export_quoted_xibor_instance(Euribor7M,Euribor);
export_quoted_xibor_instance(Euribor8M,Euribor);
export_quoted_xibor_instance(Euribor9M,Euribor);
export_quoted_xibor_instance(Euribor10M,Euribor);
export_quoted_xibor_instance(Euribor11M,Euribor);
export_quoted_xibor_instance(Euribor1Y,Euribor);

export_xibor_instance(Euribor365);
export_quoted_xibor_instance(Euribor365_SW,Euribor365);
export_quoted_xibor_instance(Euribor365_2W,Euribor365);
export_quoted_xibor_instance(Euribor365_3W,Euribor365);
export_quoted_xibor_instance(Euribor365_1M,Euribor365);
export_quoted_xibor_instance(Euribor365_2M,Euribor365);
export_quoted_xibor_instance(Euribor365_3M,Euribor365);
export_quoted_xibor_instance(Euribor365_4M,Euribor365);
export_quoted_xibor_instance(Euribor365_5M,Euribor365);
export_quoted_xibor_instance(Euribor365_6M,Euribor365);
export_quoted_xibor_instance(Euribor365_7M,Euribor365);
export_quoted_xibor_instance(Euribor365_8M,Euribor365);
export_quoted_xibor_instance(Euribor365_9M,Euribor365);
export_quoted_xibor_instance(Euribor365_10M,Euribor365);
export_quoted_xibor_instance(Euribor365_11M,Euribor365);
export_quoted_xibor_instance(Euribor365_1Y,Euribor365);

export_xibor_instance(EURLibor);
export_quoted_xibor_instance(EURLiborSW,EURLibor);
export_quoted_xibor_instance(EURLibor2W,EURLibor);
export_quoted_xibor_instance(EURLibor1M,EURLibor);
export_quoted_xibor_instance(EURLibor2M,EURLibor);
export_quoted_xibor_instance(EURLibor3M,EURLibor);
export_quoted_xibor_instance(EURLibor4M,EURLibor);
export_quoted_xibor_instance(EURLibor5M,EURLibor);
export_quoted_xibor_instance(EURLibor6M,EURLibor);
export_quoted_xibor_instance(EURLibor7M,EURLibor);
export_quoted_xibor_instance(EURLibor8M,EURLibor);
export_quoted_xibor_instance(EURLibor9M,EURLibor);
export_quoted_xibor_instance(EURLibor10M,EURLibor);
export_quoted_xibor_instance(EURLibor11M,EURLibor);
export_quoted_xibor_instance(EURLibor1Y,EURLibor);

export_xibor_instance(GBPLibor);
export_xibor_instance(Jibar);
export_xibor_instance(JPYLibor);
export_xibor_instance(NZDLibor);
export_xibor_instance(Tibor);
export_xibor_instance(TRLibor);
export_xibor_instance(USDLibor);
export_xibor_instance(Zibor);

export_swap_instance(EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA1Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA2Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA3Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA4Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA5Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA6Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA7Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA8Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA9Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA10Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA12Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA15Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA20Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA25Y,EuriborSwapFixA);
export_quoted_swap_instance(EuriborSwapFixA30Y,EuriborSwapFixA);

export_swap_instance(EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR1Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR2Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR3Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR4Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR5Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR6Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR7Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR8Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR9Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR10Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR12Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR15Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR20Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR25Y,EuriborSwapFixIFR);
export_quoted_swap_instance(EuriborSwapFixIFR30Y,EuriborSwapFixIFR);

export_swap_instance(EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA1Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA2Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA3Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA4Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA5Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA6Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA7Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA8Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA9Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA10Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA12Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA15Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA20Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA25Y,EurliborSwapFixA);
export_quoted_swap_instance(EurliborSwapFixA30Y,EurliborSwapFixA);

export_swap_instance(EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB1Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB2Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB3Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB4Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB5Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB6Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB7Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB8Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB9Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB10Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB12Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB15Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB20Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB25Y,EurliborSwapFixB);
export_quoted_swap_instance(EurliborSwapFixB30Y,EurliborSwapFixB);

export_swap_instance(EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR1Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR2Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR3Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR4Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR5Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR6Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR7Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR8Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR9Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR10Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR12Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR15Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR20Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR25Y,EurliborSwapFixIFR);
export_quoted_swap_instance(EurliborSwapFixIFR30Y,EurliborSwapFixIFR);


#endif
