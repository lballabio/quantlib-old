/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif

#include <qlo/ratehelpers.hpp>

#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yield/ratehelpers.hpp>
#include <ql/termstructures/yield/bondhelpers.hpp>
#include <ql/experimental/overnightswap/eoniaswaphelper.hpp>
#include <ql/time/imm.hpp>

#include <oh/repository.hpp>

using ObjectHandler::ValueObject;
using ObjectHandler::convert2;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using std::string;

namespace QuantLibAddin {

    DepositRateHelper::DepositRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            const shared_ptr<QuantLib::IborIndex>& iborIndex,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::DepositRateHelper(rate, iborIndex));
    }

    DepositRateHelper::DepositRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::DepositRateHelper(quote,
                                        p,
                                        fixingDays,
                                        calendar,
                                        convention,
                                        endOfMonth,
                                        dayCounter));
    }

    FuturesRateHelper::FuturesRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& price,
            const QuantLib::Date& immDate,
            const shared_ptr<QuantLib::IborIndex>& iborIndex,
            const QuantLib::Handle<QuantLib::Quote>& convAdj,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::FuturesRateHelper(price, immDate, iborIndex, convAdj));
    }

    FuturesRateHelper::FuturesRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& price,
            const QuantLib::Date& immDate,
            QuantLib::Natural lengthInMonths,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Handle<QuantLib::Quote>& convAdj,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::FuturesRateHelper(price,
                                        immDate,
                                        lengthInMonths,
                                        calendar,
                                        convention,
                                        endOfMonth,
                                        dayCounter,
                                        convAdj));
    }

    SwapRateHelper::SwapRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            const shared_ptr<QuantLib::SwapIndex>& swapIndex,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            const QuantLib::Period& forwardStart,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discount,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::SwapRateHelper(rate,
                                     swapIndex,
                                     spread, forwardStart, discount));
    }

    SwapRateHelper::SwapRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            const QuantLib::Period& p,
            const QuantLib::Calendar& cal,
            const QuantLib::Frequency& fixFreq,
            QuantLib::BusinessDayConvention fixConv,
            const QuantLib::DayCounter& fixDC,
            const shared_ptr<QuantLib::IborIndex>& ibor,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            const QuantLib::Period& forwardStart,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discount,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::SwapRateHelper(rate,
                                     p, cal, fixFreq, fixConv, fixDC, ibor,
                                     spread, forwardStart, discount));
    }

    FraRateHelper::FraRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            QuantLib::Period periodToStart,
            const shared_ptr<QuantLib::IborIndex>& iborIndex,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::FraRateHelper(rate, periodToStart, iborIndex));
    }

    FraRateHelper::FraRateHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            QuantLib::Period periodToStart,
            QuantLib::Natural lengthInMonths,
            QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::RateHelper>(new
            QuantLib::FraRateHelper(rate,
                                    periodToStart,
                                    lengthInMonths,
                                    fixingDays,
                                    calendar,
                                    convention,
                                    endOfMonth,
                                    dayCounter));
    }

    OISRateHelper::OISRateHelper(
                        const shared_ptr<ValueObject>& properties,
                        const QuantLib::Handle<QuantLib::Quote>& fixedRate,
                        const QuantLib::Period& tenor,
                        QuantLib::Natural settlementDays,
                        const QuantLib::Calendar& calendar,
                        const QuantLib::Period& overnightPeriod,
                        QuantLib::BusinessDayConvention overnightConvention,
                        const shared_ptr<QuantLib::OvernightIndex>& overnightIndex,
                        const QuantLib::Period& fixedPeriod,
                        QuantLib::BusinessDayConvention fixedConvention,
                        const QuantLib::DayCounter& fixedDayCount,
                        bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::OISRateHelper>(new
            QuantLib::OISRateHelper(fixedRate, tenor, settlementDays,
                calendar, overnightPeriod, overnightConvention, overnightIndex,
                fixedPeriod, fixedConvention, fixedDayCount));
    }

    BondHelper::BondHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& cleanPrice,
            const shared_ptr<QuantLib::Bond>& bond,
            bool permanent)
    : RateHelper(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::BondHelper>(new
            QuantLib::BondHelper(cleanPrice, bond));
    }

    FixedRateBondHelper::FixedRateBondHelper(
            const shared_ptr<ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& cleanPrice,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& paymentDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent)
    : BondHelper(properties, cleanPrice, shared_ptr<QuantLib::Bond>(), permanent) {
        libraryObject_ = shared_ptr<QuantLib::FixedRateBondHelper>(new
            QuantLib::FixedRateBondHelper(cleanPrice,
                                          settlementDays,
                                          faceAmount,
                                          *schedule,
                                          coupons,
                                          paymentDayCounter,
                                          paymentConvention,
                                          redemption,
                                          issueDate));
    }

    // helper class
    namespace {

        struct RateHelperItem {
            bool isImmFutures;
            bool isSerialFutures;
            bool isDepo;
            string objectID;
            long priority;
            QuantLib::Date earliestDate;
            QuantLib::Date latestDate;
            QuantLib::Natural minDist;
            RateHelperItem(bool isImmFutures_inp,
                           bool isSerialFutures_inp,
                           bool isDepo_inp,
                           const string& objectID_inp,
                           long priority_inp,
                           const QuantLib::Date& earliestDate_inp,
                           const QuantLib::Date& latestDate_inp,
                           QuantLib::Natural minDist_inp)
            : isImmFutures(isImmFutures_inp), isSerialFutures(isSerialFutures_inp),
              isDepo(isDepo_inp), objectID(objectID_inp),
              priority(priority_inp),
              earliestDate(earliestDate_inp), latestDate(latestDate_inp),
              minDist(minDist_inp)
            {
                QL_REQUIRE(minDist>0, "zero minimum distance not allowed");
            }
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

    std::vector<string> qlRateHelperSelection(
        const std::vector<shared_ptr<QuantLibAddin::RateHelper> >& qlarhs,
        const std::vector<QuantLib::Natural>& priority,
        QuantLib::Natural nImmFutures,
        QuantLib::Natural nSerialFutures,
        QuantLib::Natural frontFuturesRollingDays,
        RateHelper::DepoInclusionCriteria depoInclusionCriteria,
        const std::vector<QuantLib::Natural>& minDist)
    {
        // Checks
        QL_REQUIRE(!qlarhs.empty(), "no instrument given");
        QuantLib::Size nInstruments = qlarhs.size();
        QL_REQUIRE(priority.size()==nInstruments,
                   "priority (" << priority.size() <<
                   ") / instruments (" << nInstruments << ") size mismatch");
        QL_REQUIRE(minDist.size()==nInstruments || minDist.size()==1,
                   "minDist (" << minDist.size() <<
                   ") / instruments (" << nInstruments << ") mismatch");

        // RateHelperItem
        shared_ptr<QuantLibAddin::RateHelper> qlarh;
        shared_ptr<QuantLib::RateHelper> qlrh;
        std::vector<RateHelperItem> rhsAll;
        rhsAll.reserve(nInstruments);
        for (QuantLib::Size i=0; i<nInstruments; ++i) {
            qlarh = qlarhs[i];
            qlarh->getLibraryObject(qlrh);
            string qlarh_id = convert2<string>(
                qlarh->propertyValue("OBJECTID"));
            bool isFutures = dynamic_pointer_cast<FuturesRateHelper>(qlarh);
            bool isImmFutures = false, isSerialFutures = false;
            if (isFutures) {
                isImmFutures = QuantLib::IMM::isIMMdate(qlrh->earliestDate());
                isSerialFutures = !isImmFutures;
            }
            bool isDepo = dynamic_pointer_cast<DepositRateHelper>(qlarh);
            rhsAll.push_back(RateHelperItem(isImmFutures,
                                            isSerialFutures,
                                            isDepo,
                                            qlarh_id,
                                            priority[i],
                                            qlrh->earliestDate(),
                                            qlrh->latestDate(),
                                            minDist.size()==1 ? minDist[0] : minDist[i]));
        }

        // Preliminary sort of RateHelperItems according to
        // their latest date and priority
        std::sort(rhsAll.begin(), rhsAll.end(), RateHelperPrioritySorter());

        // Select input rate helpers according to:
        // expiration, maximum number of allowed Imm and Serial Futures, Depo/Futures priorities
        QuantLib::Natural immFuturesCounter = 0;
        QuantLib::Natural serialFuturesCounter = 0;
        QuantLib::Date evalDate = QuantLib::Settings::instance().evaluationDate();
        std::vector<RateHelperItem> rhs, rhsDepo;

        // FIXME: the number 2 must not be hard coded, but asked to the index
        long actualFrontFuturesRollingDays = 2+frontFuturesRollingDays;

        // Look for the front Futures, if any
        QuantLib::Date frontFuturesEarliestDate, frontFuturesLatestDate;
        bool thereAreFutures = false;
        QuantLib::Size j=0;
        while (j<nInstruments) {
            if ((rhsAll[j].isImmFutures || rhsAll[j].isSerialFutures) &&
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
                    switch (depoInclusionCriteria) {
                        case RateHelper::AllDepos:
                       // Include all depos
                            rhs.push_back(rhsAll[i]);
                            break;
                        case RateHelper::DeposBeforeFirstFuturesStartDate:
                        // Include only depos with maturity date before
                        // the front Futures start date
                            if (rhsAll[i].latestDate < frontFuturesEarliestDate)
                                rhs.push_back(rhsAll[i]);
                            break;
                        case RateHelper::DeposBeforeFirstFuturesStartDatePlusOne:
                        // Include only depos with maturity date before
                        // the front Futures start date + 1 more Futures
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
                        // Include only depos with maturity date before
                        // the front Futures expiry date
                            if (rhsAll[i].latestDate < frontFuturesLatestDate)
                                rhs.push_back(rhsAll[i]);
                            break;
                        default:
                            QL_FAIL("unknown/illegal DepoInclusionCriteria");
                    }
                } else if (rhsAll[i].isSerialFutures) {       // Check Serial Futures conditions
                    if (serialFuturesCounter<nSerialFutures &&
                           (rhsAll[i].earliestDate-actualFrontFuturesRollingDays >= evalDate)) {
                        ++serialFuturesCounter;
                        rhs.push_back(rhsAll[i]);
                    }
                } else if (rhsAll[i].isImmFutures) {       // Check IMM Futures conditions
                    if (immFuturesCounter<nImmFutures &&
                           (rhsAll[i].earliestDate-actualFrontFuturesRollingDays >= evalDate)) {
                        ++immFuturesCounter;
                        rhs.push_back(rhsAll[i]);
                    }
                } else {                                // No conditions for other instruments
                    rhs.push_back(rhsAll[i]);
                }
            }
        }

        std::vector<RateHelperItem>::iterator k;

        if (rhs.size()>1) {
            // Sort rate helpers according to their latest date and priority
            std::sort(rhs.begin(), rhs.end(), RateHelperPrioritySorter());

            // remove RateHelpers with near latestDate
            k = rhs.begin();
            while (k != rhs.end()-1) {
                QuantLib::Natural distance = static_cast<QuantLib::Natural>((k+1)->latestDate - k->latestDate);
                QuantLib::Natural minDistance = std::max(k->minDist, (k+1)->minDist);
                if ( distance < minDistance) {
                    if (k->priority <= (k+1)->priority)
                        k = rhs.erase(k);
                    else
                        rhs.erase(k+1);
                } else ++k;
            }
        }

        std::vector<string> result;
        for (k = rhs.begin(); k != rhs.end(); ++k)
            result.push_back(k->objectID);
        return result;
    }

    namespace {

        class RateInspector
            : public QuantLib::AcyclicVisitor,
              public QuantLib::Visitor<QuantLib::DepositRateHelper>,
              public QuantLib::Visitor<QuantLib::FraRateHelper>,
              public QuantLib::Visitor<QuantLib::FuturesRateHelper>,
              public QuantLib::Visitor<QuantLib::SwapRateHelper>,
              public QuantLib::Visitor<QuantLib::OISRateHelper>,
              public QuantLib::Visitor<QuantLib::BMASwapRateHelper>,
              public QuantLib::Visitor<QuantLib::FixedRateBondHelper> {
            QuantLib::Rate rate_;
          public:
            QuantLib::Rate rate() const { return rate_; }
            void visit(QuantLib::DepositRateHelper& h) {
                rate_ = h.quote()->value();
            }
            void visit(QuantLib::FraRateHelper& h) {
                rate_ = h.quote()->value();
            }
            void visit(QuantLib::FuturesRateHelper& h) {
                QuantLib::Rate futureRate = 1.0 - h.quote()->value()/100.0;
                QuantLib::Rate convAdj = h.convexityAdjustment();
                QL_ENSURE(convAdj >= 0.0,
                          "Negative (" << convAdj <<
                          ") futures convexity adjustment");
                rate_ = futureRate - convAdj;
            }
            void visit(QuantLib::SwapRateHelper& h) {
                rate_ = h.quote()->value();
            }
            void visit(QuantLib::OISRateHelper& h) {
                rate_ = h.quote()->value();
            }
            void visit(QuantLib::BMASwapRateHelper& h) {
                rate_ = h.quote()->value();
            }
            void visit(QuantLib::FixedRateBondHelper& h) {
                QL_FAIL("not implemented yet");
            }
        };

    }

    QuantLib::Real qlRateHelperRate(
        const shared_ptr<QuantLibAddin::RateHelper>& qlarh) {

        shared_ptr<QuantLib::RateHelper> qlrh;
        qlarh->getLibraryObject(qlrh);

        RateInspector v;
        qlrh->accept(v);
        return v.rate();
    }

}
