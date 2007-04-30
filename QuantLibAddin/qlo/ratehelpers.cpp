
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Marco Bianchetti
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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
#include <qlo/config.hpp>
#endif
#include <oh/objhandlerdefines.hpp>
#include <qlo/ratehelpers.hpp>

#include <ql/time/imm.hpp>

namespace QuantLibAddin {

    DepositRateHelper::DepositRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            const QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            const QuantLib::DayCounter& dayCounter)
    {
        quoteHandle_ = quote;
        libraryObject_ = boost::shared_ptr<QuantLib::RateHelper>(new
            QuantLib::DepositRateHelper(quoteHandle_,
                                        p,
                                        fixingDays,
                                        calendar,
                                        convention,
                                        false,      // FIXME
                                        fixingDays, // FIXME
                                        dayCounter));
    }

    FuturesRateHelper::FuturesRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& price,
            const std::string& immDateID,
            const QuantLib::Integer months,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention bDayConvention,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Handle<QuantLib::Quote>& convAdj)
    {
        quoteHandle_ = price;
        convAdjHandle_= convAdj;
        QuantLib::Date expiry = QuantLib::IMM::date(immDateID);
        libraryObject_ = boost::shared_ptr<QuantLib::RateHelper>(new
            QuantLib::FuturesRateHelper(quoteHandle_,
                                        expiry,
                                        months,
                                        calendar,
                                        bDayConvention,
                                        dayCounter,
                                        convAdjHandle_));
    }


    SwapRateHelper::SwapRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            const QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::Frequency& fixedFrequency,
            QuantLib::BusinessDayConvention fixedConvention,
            const QuantLib::DayCounter& fixedDayCounter,
            const boost::shared_ptr<QuantLib::IborIndex>& index)
    {
        quoteHandle_ = quote;
        libraryObject_ = boost::shared_ptr<QuantLib::RateHelper>(new
            QuantLib::SwapRateHelper(quoteHandle_,
                                     p,
                                     fixingDays,
                                     calendar,
                                     fixedFrequency,
                                     fixedConvention,
                                     fixedDayCounter,
                                     index));
    }


    // helper class
    namespace detail {

        struct RateHelperItem {
            bool isFutures;
            bool isDepo;
            std::string objectID;
            long priority;
            QuantLib::Date earliestDate;
            QuantLib::Date latestDate;
            RateHelperItem(bool isFutures,
                           bool isDepo,
                           const std::string& objectID,
                           const long& priority,
                           const QuantLib::Date& earliestDate,
                           const QuantLib::Date& latestDate)
            : isFutures(isFutures), isDepo(isDepo), objectID(objectID),
              priority(priority),
              earliestDate(earliestDate), latestDate(latestDate) {}
        };

        class RateHelperPrioritySorter {
          public:
            // does h1 come before h2?
            bool operator()(const RateHelperItem& h1,
                            const RateHelperItem& h2) const {
                if (h1.latestDate > h2.latestDate)
                    return false;
                if (h1.latestDate == h2.latestDate) {
                    if (h1.priority > h2.priority) {
                        return false;
                    } else if (h1.priority == h2.priority) {
                        return h1.objectID > h2.objectID;
                    }
                }
                return true;
            }
        };
    }

    std::vector<std::string> qlRateHelperSelection(
        const std::vector<std::string>& instrumentIDs,
        const std::vector<QuantLib::Size>& priority,
        const QuantLib::Natural nFutures,
        const QuantLib::Natural frontFuturesRollingDays,
        RateHelper::DepoInclusionCriteria depoInclusionCriteria)
    {
        // Checks
        QL_REQUIRE(!instrumentIDs.empty(), "no instrument given");
        QuantLib::Size nInstruments = instrumentIDs.size();
        QL_REQUIRE(priority.size()==nInstruments,
                   "priority / instruments mismatch");

        // RateHelperItem
        boost::shared_ptr<QuantLib::RateHelper> qlrh;
        std::vector<detail::RateHelperItem> rhsAll;
        rhsAll.reserve(nInstruments);
        for (QuantLib::Size i=0; i<nInstruments; ++i) {
            OH_GET_OBJECT(qlarh, instrumentIDs[i], RateHelper);
            bool isFutures, isDepo;
            if (boost::dynamic_pointer_cast<FuturesRateHelper>(qlarh))
                isFutures = true;
            else
                isFutures = false;
            if (boost::dynamic_pointer_cast<DepositRateHelper>(qlarh))
                isDepo = true;
            else
                isDepo = false;
            qlarh->getLibraryObject(qlrh);
            rhsAll.push_back(detail::RateHelperItem(isFutures,
                                                    isDepo,
                                                    instrumentIDs[i],
                                                    priority[i],
                                                    qlrh->earliestDate(),
                                                    qlrh->latestDate()));
        }

        // Preliminary sort of RateHelperItems according to
        // their latest date and priority
        std::sort(rhsAll.begin(), rhsAll.end(),
            detail::RateHelperPrioritySorter());

        // Select input rate helpers according to:
        // expiration, maximum number of allowed Futures, Depo/Futures priorities
        QuantLib::Natural futuresCounter = 0;
        QuantLib::Date evalDate = QuantLib::Settings::instance().evaluationDate();
        std::vector<detail::RateHelperItem> rhs, rhsDepo;
        long actualFrontFuturesRollingDays = 2+frontFuturesRollingDays;
        // Look for the front Futures, if any
        QuantLib::Date frontFuturesEarliestDate, frontFuturesLatestDate;
        bool thereAreFutures = false;
        QuantLib::Size j=0;
        while (j<nInstruments) {
            if (rhsAll[j].isFutures &&
                    (rhsAll[j].earliestDate-actualFrontFuturesRollingDays >= evalDate)) {
                thereAreFutures = true;
                frontFuturesEarliestDate = rhsAll[j].earliestDate;
                frontFuturesLatestDate = rhsAll[j].latestDate;
                break;
            }
            ++j;
        }
        // If there are NOT Futures, include all Depos
        if (!thereAreFutures)
            depoInclusionCriteria = RateHelper::AllDepos;
        // Start selection
        bool depoAfterFrontFuturesAlreadyIncluded = false;
        for (QuantLib::Size i=0; i<nInstruments; ++i) {
            if (rhsAll[i].earliestDate >= evalDate) {
                if (rhsAll[i].isDepo) {                 // Check Depo conditions
                    //if (!depoAfterFrontFuturesStartDateExcludeFlag &&
                    //    !depoAfterFrontFuturesExpiryDateExcludeFlag) {
                    //    rhs.push_back(rhsAll[i]);       // Include all depos
                    //} else if (depoAfterFrontFuturesStartDateExcludeFlag) {
                    //    // Include only depos with maturity date before the front Futures start date, +1
                    //    if (rhsAll[i].latestDate < frontFuturesEarliestDate) {
                    //        rhs.push_back(rhsAll[i]);
                    //    } else {
                    //        if (depoAfterFrontFuturesAlreadyIncluded == false) {
                    //            rhs.push_back(rhsAll[i]);
                    //            depoAfterFrontFuturesAlreadyIncluded = true;
                    //        }
                    //    }
                    //} else if (depoAfterFrontFuturesExpiryDateExcludeFlag) {
                    //    // Include only depos with maturity date before the front Futures expiry date
                    //    if (rhsAll[i].latestDate < frontFuturesLatestDate) {
                    //        rhs.push_back(rhsAll[i]);
                    //    }
                    //}
                    switch (depoInclusionCriteria) {
                        case RateHelper::AllDepos:
                       // Include all depos
                            rhs.push_back(rhsAll[i]);
                            break;
                        case RateHelper::DeposBeforeFirstFuturesStartDate:
                        // Include only depos with maturity date before the front Futures start date
                            if (rhsAll[i].latestDate < frontFuturesEarliestDate)
                                rhs.push_back(rhsAll[i]);
                            break;
                        case RateHelper::DeposBeforeFirstFuturesStartDatePlusOne:
                        // Include only depos with maturity date before the front Futures start date, +1
                            if (rhsAll[i].latestDate < frontFuturesEarliestDate) {
                                rhs.push_back(rhsAll[i]);
                            } else {
                                if (depoAfterFrontFuturesAlreadyIncluded == false) {
                                    rhs.push_back(rhsAll[i]);
                                    depoAfterFrontFuturesAlreadyIncluded = true;
                                }
                            }
                            break;
                        case RateHelper::DeposBeforeFirstFuturesExpiryDate:
                        // Include only depos with maturity date before the front Futures expiry date
                            if (rhsAll[i].latestDate < frontFuturesLatestDate)
                                rhs.push_back(rhsAll[i]);
                            break;
                        default:
                            QL_FAIL("unknown/illegal DepoInclusionCriteria");
                    }
                } else if (rhsAll[i].isFutures) {       // Check Futures conditions
                    if (futuresCounter<nFutures &&
                           (rhsAll[i].earliestDate-actualFrontFuturesRollingDays >= evalDate)) {
                        futuresCounter++;
                        rhs.push_back(rhsAll[i]);
                    }
                } else {                                // No conditions for other instruments
                    rhs.push_back(rhsAll[i]);
                }
            }
        }
        std::vector<std::string> result;

        // Zero or one rate helper left
        if (rhs.size()<2) {
            std::vector<detail::RateHelperItem>::const_iterator i;
            for (i = rhs.begin(); i != rhs.end(); ++i)
                result.push_back(i->objectID);
            return result;
        }

        // Sort rate helpers according to their latest date and priority
        std::sort(rhs.begin(), rhs.end(), detail::RateHelperPrioritySorter());

        // remove RateHelpers with duplicate latestDate
        for (QuantLib::Size i=0; i<rhs.size()-1; ++i) {
            if (rhs[i].latestDate < rhs[i+1].latestDate)
                result.push_back(rhs[i].objectID);
        }
        // Add the last one in any case
        result.push_back(rhs[rhs.size()-1].objectID);

        return result;
    }

}
