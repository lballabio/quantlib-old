
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
%include types.i
%include termstructures.i
%include history.i
%include stl.i

%{
using QuantLib::XiborManager;
%}

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
using QuantLib::Euribor;
using QuantLib::AUDLibor;
using QuantLib::GBPLibor;
using QuantLib::USDLibor;
using QuantLib::JPYLibor;
using QuantLib::CADLibor;
using QuantLib::CHFLibor;
using QuantLib::ZARLibor;
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
        XiborPtr(const std::string& name, Integer n, TimeUnit units,
                 const RelinkableHandle<TermStructure>& h) {
            std::string s = StringFormatter::toLowercase(name);
            if (s == "euribor")
                return new XiborPtr(new Euribor(n,units,h));
            else if (s == "audlibor")
                return new XiborPtr(new AUDLibor(n,units,h));
            else if (s == "gbplibor")
                return new XiborPtr(new GBPLibor(n,units,h));
            else if (s == "usdlibor")
                return new XiborPtr(new USDLibor(n,units,h));
            else if (s == "jpylibor")
                return new XiborPtr(new JPYLibor(n,units,h));
            else if (s == "cadlibor")
                return new XiborPtr(new CADLibor(n,units,h));
            else if (s == "chflibor")
                return new XiborPtr(new CHFLibor(n,units,h));
            else if (s == "zarlibor")
                return new XiborPtr(new ZARLibor(n,units,h));
            else
                QL_FAIL("unknown index: " + name);
            QL_DUMMY_RETURN(new XiborPtr);
        }
        Period tenor() {
            return boost::dynamic_pointer_cast<Xibor>(*self)->tenor();
        }
        CurrencyTag currency() {
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


#endif
