
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

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
using QuantLib::Indexes::XiborManager;
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

%template(Index) Handle<Index>;
#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(">string") Handle<Index>::__str__;
#endif
%extend Handle<Index> {
    Rate fixing(const Date& fixingDate) {
        return (*self)->fixing(fixingDate);
    }
    std::string name() {
        return (*self)->name();
    }
    std::string __str__() {
        if (!self->isNull())
            return (*self)->name()+" index";
        else
            return "Null index";
    }
}

// Xibor indexes
%{
using QuantLib::Indexes::Xibor;
using QuantLib::Indexes::Euribor;
using QuantLib::Indexes::AUDLibor;
using QuantLib::Indexes::GBPLibor;
using QuantLib::Indexes::USDLibor;
using QuantLib::Indexes::JPYLibor;
using QuantLib::Indexes::CADLibor;
using QuantLib::Indexes::CHFLibor;
using QuantLib::Indexes::ZARLibor;
typedef Handle<Index> XiborHandle;
%}

%rename(Xibor) XiborHandle;
class XiborHandle : public Handle<Index> {
    #if defined(SWIGRUBY)
    %rename("isAdjusted?")        isAdjusted;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-adjusted?")       isAdjusted;
    %rename("rolling-convention") rollingConvention;
    %rename("day-counter")        dayCounter;
    #endif
  public:
    %extend {
        XiborHandle(const std::string& name, int n, TimeUnit units,
                    const RelinkableHandle<TermStructure>& h) {
            std::string s = StringFormatter::toLowercase(name);
            if (s == "euribor")
                return new XiborHandle(new Euribor(n,units,h));
            else if (s == "audlibor")
                return new XiborHandle(new AUDLibor(n,units,h));
            else if (s == "gbplibor")
                return new XiborHandle(new GBPLibor(n,units,h));
            else if (s == "usdlibor")
                return new XiborHandle(new USDLibor(n,units,h));
            else if (s == "jpylibor")
                return new XiborHandle(new JPYLibor(n,units,h));
            else if (s == "cadlibor")
                return new XiborHandle(new CADLibor(n,units,h));
            else if (s == "chflibor")
                return new XiborHandle(new CHFLibor(n,units,h));
            else if (s == "zarlibor")
                return new XiborHandle(new ZARLibor(n,units,h));
            else
                throw Error("unknown index: " + name);
            QL_DUMMY_RETURN(new XiborHandle);
        }
        Period tenor() {
            return Handle<Xibor>(*self)->tenor();
        }
        Currency currency() {
            return Handle<Xibor>(*self)->currency();
        }
        Calendar calendar() {
            return Handle<Xibor>(*self)->calendar();
        }
        bool isAdjusted() {
            return Handle<Xibor>(*self)->isAdjusted();
        }
        RollingConvention rollingConvention() {
            return Handle<Xibor>(*self)->rollingConvention();
        }
        DayCounter dayCounter() {
            return Handle<Xibor>(*self)->dayCounter();
        }
    }
};


#endif
