
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
            const std::string &timeUnitsID,
            const long &fixingDays,
            const std::string &calendarID,
            const std::string &conventionID,
            const std::string &dayCounterID) {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(quote));
        quoteHandle_.linkTo(quote_);

        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::BusinessDayConvention convention = 
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

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
            const std::string &dayCounterID,
            const std::string &bDayConventionID,
            const std::string &calendarID,
            const QuantLib::Integer &decade) {

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        QuantLib::BusinessDayConvention bDayConvention = 
            Create<QuantLib::BusinessDayConvention>()(bDayConventionID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::Date expiry = FutIDtoExpiryDate(immDateID, calendar, bDayConvention, decade);

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(price));
        quoteHandle_.linkTo(quote_);
        
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
            const std::string &timeUnitsID,
            const long &fixingDays,
            const std::string &calendarID,
            const std::string &fixedFrequencyID,
            const std::string &fixedConventionID,
            const std::string &fixedDayCounterID,
            const std::string &floatingFrequencyID,
            const std::string &floatingConventionID,
            const std::string &floatingDayCounterID) {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(quote));
        quoteHandle_.linkTo(quote_);
        
        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::Frequency fixedFrequency =
            Create<QuantLib::Frequency>()(fixedFrequencyID);
        QuantLib::BusinessDayConvention fixedConvention = 
            Create<QuantLib::BusinessDayConvention>()(fixedConventionID);
        QuantLib::DayCounter fixedDayCounter =
            Create<QuantLib::DayCounter>()(fixedDayCounterID);
        QuantLib::Frequency floatingFrequency =
            Create<QuantLib::Frequency>()(floatingFrequencyID);
        QuantLib::BusinessDayConvention floatingConvention = 
            Create<QuantLib::BusinessDayConvention>()(floatingConventionID);
        QuantLib::DayCounter floatingDayCounter =
            Create<QuantLib::DayCounter>()(floatingDayCounterID);

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


            
            
            
    PiecewiseFlatForward::PiecewiseFlatForward(
            const long &settlement,
            const std::vector<std::string> &handlesRateHelper,
            const std::string &dayCounterID) {

        QuantLib::Date settlementDate(settlement);

        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i != handlesRateHelper.end() ; i++) {
            OH_GET_REFERENCE(rateHelper, *i, RateHelper, QuantLib::RateHelper)
            rateHelpersQL.push_back(rateHelper);
        }

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseFlatForward(settlementDate,
                                               rateHelpersQL,
                                               dayCounter));
    }

    PiecewiseYieldCurve::PiecewiseYieldCurve(
            const long &nDays,
            const QuantLib::Calendar &calendar,
            //const long &settlement,
            const std::vector<std::string> &handlesRateHelper,
            const std::string &dayCounterID) {

        //QuantLib::Calendar calendar =Create<QuantLib::Calendar>()(calendarID);
        //QuantLib::Date settlementDate(settlement);

        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i != handlesRateHelper.end() ; i++) {
            OH_GET_REFERENCE(rateHelper, *i, RateHelper, QuantLib::RateHelper)
            rateHelpersQL.push_back(rateHelper);
        }

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

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
            new QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
            QuantLib::Linear>(
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
            const std::vector < long > &dates,
            const std::vector < double > &dfs,
            const std::string &dayCounterID) {

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        const std::vector<QuantLib::Date> qlDates = 
            longVectorToDateVector(dates);

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::DiscountCurve(qlDates, dfs, dayCounter));
    }

    ZeroCurve::ZeroCurve(
            const std::vector < long > &dates,
            const std::vector < double > &zeroRates,
            const std::string &dayCounterID) {

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        const std::vector<QuantLib::Date> qlDates = 
            longVectorToDateVector(dates);

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::ZeroCurve(qlDates, zeroRates, dayCounter));
    }

    ForwardCurve::ForwardCurve(
            const std::vector < long > &dates,
            const std::vector < double > &forwardRates,
            const std::string &dayCounterID) {

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        const std::vector<QuantLib::Date> qlDates = 
            longVectorToDateVector(dates);

        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::ForwardCurve(qlDates, forwardRates, dayCounter));
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
            bool operator()(const boost::shared_ptr<RateHelper>& h1,
                            const boost::shared_ptr<RateHelper>& h2) const {

                if (h1->getObject().latestDate() > h2->getObject().latestDate())
                    return false;

                if (h1->getObject().latestDate() == h2->getObject().latestDate()) {
                    if (boost::dynamic_pointer_cast<FuturesRateHelper>(h1)) {
                        return false;
                    } else if (!boost::dynamic_pointer_cast<FuturesRateHelper>(h2)) {
                        if (boost::dynamic_pointer_cast<SwapRateHelper>(h1)) {
                            return false;
                        //} else if (!boost::dynamic_pointer_cast<SwapRateHelper>(h2)) {
                        //    if (boost::dynamic_pointer_cast<FixedCouponBondHelper>(h1)) {
                        //        return false;
                        //    }
                        }
                    }
                }

                return true;

            }
        };

    }

    std::vector<std::string> qlRateHelperSelection(
        const std::vector<std::string>& instrumentHandles,
        const std::vector<bool>& includeFlag,
        const long& nFutures) {

        QL_REQUIRE(!instrumentHandles.empty(), "no instrument given");

        QuantLib::Size nInstruments = instrumentHandles.size();
        QL_REQUIRE(includeFlag.size()==nInstruments,
            "includeFlag / instruments mismatch");

        std::vector<boost::shared_ptr<RateHelper>> instruments;
        for (std::vector<std::string>::const_iterator it = instrumentHandles.begin();
            it != instrumentHandles.end(); it++) {
                OH_GET_OBJECT(objectPointer, *it, RateHelper)
                instruments.push_back(objectPointer);
        }

        // purge input rate helpers according to their includeFlag,
        // their expiration, and maximum number of allowed futures
        std::vector<boost::shared_ptr<RateHelper> > rhs;
        QuantLib::Size i;
        long futuresCounter = 0;
        for (i=0; i<nInstruments; i++) {
            if (includeFlag[i] && (instruments[i]->getObject().earliestDate() <
                                   QuantLib::Settings::instance().evaluationDate())) {
                if (!boost::dynamic_pointer_cast<FuturesRateHelper>(instruments[i])) {
                    rhs.push_back(instruments[i]);
                } else if (futuresCounter<nFutures) {
                    futuresCounter++;
                    rhs.push_back(instruments[i]);
                }
            }
        }

        std::vector<std::string> instanceNameStubs;

        // zero or one rate helper left
        if (rhs.size()<2) {
            for (std::vector<boost::shared_ptr<RateHelper> >::const_iterator i = rhs.begin();
                i != rhs.end(); i++)
                instanceNameStubs.push_back((*i)->getStubName());
            return instanceNameStubs;
        }

        // sort rate helpers
        std::sort(rhs.begin(),rhs.end(), detail::RateHelperPrioritySorter());

        for (i=0; i<rhs.size()-1; i++) {
            if (rhs[i]->getObject().latestDate() < rhs[i+1]->getObject().latestDate()) 
                instanceNameStubs.push_back(rhs[i]->getStubName());
        }
        instanceNameStubs.push_back(rhs[i]->getStubName());

        return instanceNameStubs;
    }

}
