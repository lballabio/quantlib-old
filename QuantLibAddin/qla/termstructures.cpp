
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

#ifdef HAVE_CONFIG_H
#include <qla/config.hpp>
#endif
#include <oh/objhandlerdefines.hpp>
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>
#include <qla/generalutils.hpp>

#include <ql/date.hpp>
#include <ql/Math/cubicspline.hpp>
#include <ql/TermStructures/discountcurve.hpp>
#include <ql/TermStructures/forwardcurve.hpp>

namespace QuantLibAddin {

    double RateHelper::setQuote(double quote) {
        double diff = quote - quote_->value();
        quote_->setValue(quote);
        return diff;
    }


    DepositRateHelper::DepositRateHelper(
            const double &quote,
            const long &maturity,
            const QuantLib::TimeUnit &timeUnits,
            const long &fixingDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::BusinessDayConvention &convention,
            const QuantLib::DayCounter &dayCounter) {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(quote));
        quoteHandle_.linkTo(quote_);

        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::DepositRateHelper(quoteHandle_,
                                            maturity,
                                            timeUnits,
                                            fixingDays,
                                            calendar,
                                            convention,
                                            dayCounter));
    }

    FuturesRateHelper::FuturesRateHelper(
            const double &price,
            const std::string &immDateID,
            const QuantLib::Integer &months,
            const QuantLib::Calendar& calendar,
            const QuantLib::BusinessDayConvention &bDayConvention,
            const QuantLib::DayCounter &dayCounter) {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(price));
        quoteHandle_.linkTo(quote_);

        QuantLib::Date expiry = QuantLib::Date::IMMdate(immDateID);
        
        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::FuturesRateHelper(
                quoteHandle_,
                expiry,
                months,
                calendar,
                bDayConvention,
                dayCounter));
    }
    
    SwapRateHelper::SwapRateHelper(
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
            const QuantLib::DayCounter &floatingDayCounter) {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(quote));
        quoteHandle_.linkTo(quote_);

        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::SwapRateHelper(quoteHandle_,
                                         maturity,
                                         timeUnits,
                                         fixingDays,
                                         calendar,
                                         fixedFrequency,
                                         fixedConvention,
                                         fixedDayCounter,
                                         floatingFrequency,
                                         floatingConvention,
                                         floatingDayCounter));
    }


    PiecewiseYieldCurve::PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            const std::vector<std::string> &handlesRateHelper,
            const QuantLib::DayCounter &dayCounter) {

        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i != handlesRateHelper.end() ; i++) {
            OH_GET_REFERENCE(rateHelper, *i, RateHelper, QuantLib::RateHelper)
            rateHelpersQL.push_back(rateHelper);
        }

        QuantLib::Cubic cubic0(
            QuantLib::CubicSpline::SecondDerivative, 0.0,
            QuantLib::CubicSpline::SecondDerivative, 0.0,
            false);

        QuantLib::Cubic cubic1(
            QuantLib::CubicSpline::SecondDerivative, 0.0,
            QuantLib::CubicSpline::FirstDerivative, 0.0,
            false);

        QuantLib::Cubic monotoneCubic(
            QuantLib::CubicSpline::SecondDerivative, 0.0,
            QuantLib::CubicSpline::FirstDerivative, 0.0,
            true);

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
            QuantLib::LogLinear>(
                nDays, calendar,
                rateHelpersQL,
                dayCounter,
                1.0e-6));

        /*
        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,QuantLib::Cubic>(
                nDays, calendar,
                rateHelpersQL,
                dayCounter,
                1e-12,
                monotoneCubic));
        */

    }

    DiscountCurve::DiscountCurve(
        const std::vector<QuantLib::Date> &dates,
        const std::vector <double> &dfs,
        const QuantLib::DayCounter &dayCounter) {

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::DiscountCurve(dates, dfs, dayCounter));
    }

    ZeroCurve::ZeroCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &zeroRates,
            const QuantLib::DayCounter &dayCounter) {

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::ZeroCurve(dates, zeroRates, dayCounter));
    }

    ForwardCurve::ForwardCurve(
            const std::vector<QuantLib::Date> &dates,
            const std::vector <double> &forwardRates,
            const QuantLib::DayCounter &dayCounter) {

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::ForwardCurve(dates, forwardRates, dayCounter));
    }


    ForwardSpreadedTermStructure::ForwardSpreadedTermStructure(
            const std::string &baseTermStructure,
            const double &spread) {

        QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;

        if(!baseTermStructure.empty()) {
            OH_GET_REFERENCE(discYC, baseTermStructure, 
                YieldTermStructure, QuantLib::YieldTermStructure)
            discountingTermStructure.linkTo(discYC);
        }

        QuantLib::Handle<QuantLib::Quote> spreadQuote(
            boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(spread)));

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::ForwardSpreadedTermStructure(discountingTermStructure, spreadQuote));
    }

    // helper class
    namespace detail {

        class RateHelperPrioritySorter {
          public:
            // does h1 come before h2?
            bool operator()(const std::pair<boost::shared_ptr<RateHelper>, long>& h1,
                            const std::pair<boost::shared_ptr<RateHelper>, long>& h2) const {

                if (h1.first->getObject().latestDate() > h2.first->getObject().latestDate())
                    return false;

                if (h1.first->getObject().latestDate() == h2.first->getObject().latestDate()) {
                    if (h1.second > h2.second) {
                        return false;
                    }
                }

                return true;

            }
        };
    }

    std::vector<std::string> qlRateHelperSelection(
        const std::vector<std::string>& instrumentHandles,
        const std::vector<bool>& includeFlag,
        const std::vector<long>& priority,
        const long& nFutures) {

        QL_REQUIRE(!instrumentHandles.empty(), "no instrument given");

        QuantLib::Size nInstruments = instrumentHandles.size();
        QL_REQUIRE(includeFlag.size()==nInstruments,
            "includeFlag / instruments mismatch");
        QL_REQUIRE(priority.size()==nInstruments,
            "priority / instruments mismatch");

        std::vector<boost::shared_ptr<RateHelper> > instruments;
        for (std::vector<std::string>::const_iterator it = instrumentHandles.begin();
            it != instrumentHandles.end(); it++) {
                OH_GET_OBJECT(objectPointer, *it, RateHelper)
                instruments.push_back(objectPointer);
        }

        // purge input rate helpers according to their includeFlag,
        // their expiration, and maximum number of allowed futures
        std::vector<std::pair<boost::shared_ptr<RateHelper>, long> > rhs;
        QuantLib::Size i;
        long futuresCounter = 0;
        QuantLib::Date earliestDate, evalDate = QuantLib::Settings::instance().evaluationDate();
        for (i=0; i<nInstruments; i++) {
            earliestDate = instruments[i]->getObject().earliestDate();
            if (includeFlag[i]) {
                if (!boost::dynamic_pointer_cast<FuturesRateHelper>(instruments[i]) && (earliestDate >= evalDate)) {
                    rhs.push_back(std::make_pair(instruments[i], priority[i]));
                } else if (futuresCounter<nFutures && (earliestDate-2 >= evalDate)) {
                    futuresCounter++;
                    rhs.push_back(std::make_pair(instruments[i], priority[i]));
                }
            }
        }

        std::vector<std::string> instanceNameStubs;

        // zero or one rate helper left
        if (rhs.size()<2) {
            std::vector<std::pair<boost::shared_ptr<RateHelper>, long> >::const_iterator i;
            for (i = rhs.begin(); i != rhs.end(); i++)
                instanceNameStubs.push_back((*i).first->getStubName());
            return instanceNameStubs;
        }

        // sort rate helpers according to their latest date and priority
        std::sort(rhs.begin(), rhs.end(), detail::RateHelperPrioritySorter());

        for (i=0; i<rhs.size()-1; i++) {
            if (rhs[i].first->getObject().latestDate() < rhs[i+1].first->getObject().latestDate()) 
                instanceNameStubs.push_back(rhs[i].first->getStubName());
        }
        // add the last one in any case
        instanceNameStubs.push_back(rhs[i].first->getStubName());

        return instanceNameStubs;
    }

}
