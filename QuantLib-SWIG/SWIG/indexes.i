
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

#ifndef quantlib_indexes_i
#define quantlib_indexes_i

%include date.i
%include calendars.i
%include daycounters.i
%include currencies.i
%include types.i
%include termstructures.i
%include history.i
%include vectors.i

%{
using QuantLib::IndexManager;
using QuantLib::XiborManager;
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
    void setHistory(const std::string& name, const History& fixings);
    const History& getHistory(const std::string& name) const;
    bool hasHistory(const std::string& name) const;
    std::vector<std::string> histories() const;
};

class XiborManager {
    #if defined(SWIGRUBY)
    %rename("hasHistory?")  hasHistory;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("has-history?") hasHistory;
    %rename("history-get")  getHistory;
    %rename("history-set!") setHistory;
    #endif
  private:
    XiborManager();
  public:
    static void setHistory(const std::string& name, const History& fixings);
    static const History& getHistory(const std::string& name);
    static bool hasHistory(const std::string& name);
    // static std::vector<std::string> histories();
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
};

%template(Index) boost::shared_ptr<Index>;
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(">string") boost::shared_ptr<Index>::__str__;
#endif
%extend boost::shared_ptr<Index> {
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
  protected:
    XiborPtr();
  public:
    %extend {
        Period tenor() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->tenor();
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

%define export_xibor_instance(Name,defaultDayCount)
%{
using QuantLib::Name;
typedef boost::shared_ptr<Index> Name##Ptr;
%}
%rename(Name) Name##Ptr;
class Name##Ptr : public XiborPtr {
  public:
    %extend {
      Name##Ptr(Integer n, TimeUnit units,
                const Handle<YieldTermStructure>& h,
                const DayCounter& dayCount = QuantLib::defaultDayCount()) {
          return new Name##Ptr(new Name(n,units,h,dayCount));
      }
    }
};
%enddef

export_xibor_instance(AUDLibor, Actual365);
export_xibor_instance(CADLibor, Actual365);
export_xibor_instance(CHFLibor, Actual360);
export_xibor_instance(Euribor, Actual360);
export_xibor_instance(GBPLibor, Actual365);
export_xibor_instance(JPYLibor, Actual360);
export_xibor_instance(USDLibor, Actual360);
export_xibor_instance(ZARLibor, Actual365);


#endif
