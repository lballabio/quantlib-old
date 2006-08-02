
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005 StatPro Italia srl

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
  public:
    Rate fixing(const Date& fixingDate) const;
    std::string name() const;
    void addFixing(const Date& fixingDate, Rate fixing);
};

%template(Index) boost::shared_ptr<Index>;
%extend boost::shared_ptr<Index> {
    %extend {
        void addFixings(const std::vector<Date>& fixingDates,
                        const std::vector<Rate>& fixings) {
            (*self)->addFixings(fixingDates.begin(),fixingDates.end(),
                                fixings.begin());
        }
    }
    std::string __str__() {
        if (*self)
            return (*self)->name()+" index";
        else
            return "Null index";
    }
}

// Xibor indexes
%{
using QuantLib::Xibor;
typedef boost::shared_ptr<Index> XiborPtr;
%}

%rename(Xibor) XiborPtr;
class XiborPtr : public boost::shared_ptr<Index> {
    #if defined(SWIGRUBY)
    %rename("isAdjusted?") isAdjusted;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-adjusted?")            isAdjusted;
    %rename("business-day-convention") businessDayConvention;
    %rename("day-counter")             dayCounter;
    #endif
  public:
    %extend {
        XiborPtr(const std::string& familyName,
                 const Period& tenor,
                 Integer settlementDays,
                 const Currency& currency,
                 const Calendar& calendar,
                 BusinessDayConvention convention,
                 const DayCounter& dayCounter,
                 const Handle<YieldTermStructure>& h) {
            return new XiborPtr(new Xibor(familyName, tenor, settlementDays,
                                          currency, calendar, convention,
                                          dayCounter, h));
        }
        Period tenor() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->tenor();
        }
        Frequency frequency() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->frequency();
        }
        Currency currency() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->currency();
        }
        Calendar calendar() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->calendar();
        }
        bool isAdjusted() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->isAdjusted();
        }
        BusinessDayConvention businessDayConvention() {
            return boost::dynamic_pointer_cast<Xibor>(*self)
                 ->businessDayConvention();
        }
        DayCounter dayCounter() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->dayCounter();
        }
    }
};

%define export_xibor_instance(Name)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public XiborPtr {
  public:
    %extend {
      Name##Ptr(const Period& tenor,
                const Handle<YieldTermStructure>& h) {
          return new Name##Ptr(new Name(tenor,h));
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
export_xibor_instance(Euribor365);
export_xibor_instance(EURLibor);
export_xibor_instance(GBPLibor);
export_xibor_instance(Jibar);
export_xibor_instance(JPYLibor);
export_xibor_instance(NZDLibor);
export_xibor_instance(Tibor);
export_xibor_instance(TRLibor);
export_xibor_instance(USDLibor);
export_xibor_instance(Zibor);


#endif
