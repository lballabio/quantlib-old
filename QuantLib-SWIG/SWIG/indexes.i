
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
        return (*self)->name()+" index";
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

export_swap_instance(EuriborSwapFixAvs3M);
export_swap_instance(EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA1Y,EuriborSwapFixAvs3M);
export_quoted_swap_instance(EuriborSwapFixA2Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA3Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA4Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA5Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA6Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA7Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA8Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA9Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA10Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA12Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA15Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA20Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA25Y,EuriborSwapFixAvs6M);
export_quoted_swap_instance(EuriborSwapFixA30Y,EuriborSwapFixAvs6M);

export_swap_instance(EuriborSwapFixIFRvs3M);
export_swap_instance(EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR1Y,EuriborSwapFixIFRvs3M);
export_quoted_swap_instance(EuriborSwapFixIFR2Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR3Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR4Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR5Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR6Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR7Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR8Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR9Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR10Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR12Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR15Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR20Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR25Y,EuriborSwapFixIFRvs6M);
export_quoted_swap_instance(EuriborSwapFixIFR30Y,EuriborSwapFixIFRvs6M);

export_swap_instance(EurliborSwapFixAvs3M);
export_swap_instance(EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA1Y,EurliborSwapFixAvs3M);
export_quoted_swap_instance(EurliborSwapFixA2Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA3Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA4Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA5Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA6Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA7Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA8Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA9Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA10Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA12Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA15Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA20Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA25Y,EurliborSwapFixAvs6M);
export_quoted_swap_instance(EurliborSwapFixA30Y,EurliborSwapFixAvs6M);

export_swap_instance(EurliborSwapFixBvs3M);
export_swap_instance(EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB1Y,EurliborSwapFixBvs3M);
export_quoted_swap_instance(EurliborSwapFixB2Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB3Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB4Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB5Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB6Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB7Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB8Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB9Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB10Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB12Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB15Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB20Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB25Y,EurliborSwapFixBvs6M);
export_quoted_swap_instance(EurliborSwapFixB30Y,EurliborSwapFixBvs6M);

export_swap_instance(EurliborSwapFixIFRvs3M);
export_swap_instance(EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR1Y,EurliborSwapFixIFRvs3M);
export_quoted_swap_instance(EurliborSwapFixIFR2Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR3Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR4Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR5Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR6Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR7Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR8Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR9Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR10Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR12Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR15Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR20Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR25Y,EurliborSwapFixIFRvs6M);
export_quoted_swap_instance(EurliborSwapFixIFR30Y,EurliborSwapFixIFRvs6M);


#endif
