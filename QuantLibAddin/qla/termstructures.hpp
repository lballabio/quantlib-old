
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_termstructures_hpp
#define qla_termstructures_hpp

#include <ql/termstructure.hpp>
#include <ql/calendar.hpp>
#include <ql/TermStructures/piecewiseflatforward.hpp>
#include <ql/TermStructures/forwardspreadedtermstructure.hpp>
#include <ql/TermStructures/ratehelpers.hpp>
#include <ql/Functions/termstructures.hpp>

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    class RateHelper : public ObjHandler::Object {
      public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(rateHelper_);
        }
        double setQuote(double quote);
        QuantLib::Handle<QuantLib::Quote> quoteHandle() const {
            return quoteHandle_;
        }
        const QuantLib::RateHelper& getObject() const {
            return *rateHelper_;
        }
      protected:
        boost::shared_ptr<QuantLib::SimpleQuote> quote_;
        boost::shared_ptr<QuantLib::RateHelper> rateHelper_;
        QuantLib::Handle<QuantLib::Quote> quoteHandle_;
    };
    
    class DepositRateHelper : public RateHelper {
      public:
        DepositRateHelper(
            const double &quote,
            const long &maturity,
            const QuantLib::TimeUnit &timeUnits,
            const long &fixingDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::BusinessDayConvention &convention,
            const QuantLib::DayCounter &dayCounter);
    };

    class FuturesRateHelper : public RateHelper {
      public:
        FuturesRateHelper(
            const double &price,
            const std::string &immDateID,
            const QuantLib::Integer &months,
            const QuantLib::Calendar& calendar,
            const QuantLib::BusinessDayConvention &bDayConvention,
            const QuantLib::DayCounter &dayCounter);
    };

    class SwapRateHelper : public RateHelper {
      public:
        SwapRateHelper(
            const double &quote,
            const long &maturity,
            const QuantLib::TimeUnit &timeUnits,
            const long &fixingDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::Frequency &fixedFrequency,
            const QuantLib::BusinessDayConvention &fixedConvention,
            const QuantLib::DayCounter &fixedDayCounter,
            const QuantLib::Frequency &floatingFrequency,
            const QuantLib::BusinessDayConvention &floatingConvention,
            const QuantLib::DayCounter &floatingDayCounter);
    };

    class YieldTermStructure : public ObjHandler::Object {
      public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(termStructure_);
        }
        const QuantLib::YieldTermStructure& getObject() const {return *termStructure_;}
      protected:
        boost::shared_ptr<QuantLib::YieldTermStructure> termStructure_;
    };


    class PiecewiseYieldCurve : public YieldTermStructure {
      public:
        PiecewiseYieldCurve(
            const long &nDays, 
            const QuantLib::Calendar &calendar,
            const std::vector<std::string> &handlesRateHelper,
            const QuantLib::DayCounter &dayCounter);
    };
    
    class DiscountCurve : public YieldTermStructure {
      public:
        DiscountCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector<double> &dfs,
            const QuantLib::DayCounter &dayCounter);
    };

    class ZeroCurve : public YieldTermStructure {
      public:
        ZeroCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &zeroRates,
            const QuantLib::DayCounter &dayCounter);
    };

    class ForwardCurve : public YieldTermStructure {
      public:
        ForwardCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &forwardRates,
            const QuantLib::DayCounter &dayCounter);
    };

    class ForwardSpreadedTermStructure : public YieldTermStructure {
      public:
        ForwardSpreadedTermStructure(
            const std::string &baseTermStructure,
            const double &spread);
    };

    std::vector<std::string> qlRateHelperSelection(
        const std::vector<std::string>& instrumentHandles,
        const std::vector<bool>& includeFlag,
        const long& nFutures);

}

#endif

