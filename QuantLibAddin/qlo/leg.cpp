/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/leg.hpp>
#include <qlo/flowanalysis.hpp>
#include <qlo/enumerations/factories/iborcouponpricersfactory.hpp>
#include <qlo/couponvectors.hpp>

#include <ql/cashflow.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/cashflows/couponpricer.hpp>

using QuantLib::earlier_than;
using QuantLib::CashFlow;
using QuantLib::Date;
using QuantLib::YieldTermStructure;
using std::vector;
using boost::shared_ptr;
using QuantLib::FloatingRateCouponPricer;

namespace QuantLibAddin {

    QuantLib::Date Leg::startDate() const {
        return QuantLib::CashFlows::startDate(leg_);
    }

    QuantLib::Date Leg::maturityDate() const {
        return QuantLib::CashFlows::maturityDate(leg_);
    }

    bool Leg::isExpired(Date refDate) const {
        return QuantLib::CashFlows::isExpired(leg_, refDate);
    }





    QuantLib::Date Leg::previousCashFlowDate(const Date& refDate) const {
        return QuantLib::CashFlows::previousCashFlowDate(leg_, refDate);
    }

    QuantLib::Date Leg::nextCashFlowDate(const Date& refDate) const {
        return QuantLib::CashFlows::nextCashFlowDate(leg_, refDate);
    }

    QuantLib::Real Leg::previousCashFlowAmount(const Date& refDate) const {
        return QuantLib::CashFlows::previousCashFlowAmount(leg_, refDate);
    }

    QuantLib::Real Leg::nextCashFlowAmount(const Date& refDate) const {
        return QuantLib::CashFlows::nextCashFlowAmount(leg_, refDate);
    }






    QuantLib::Rate Leg::previousCouponRate(const Date& settlementDate) const {
        return QuantLib::CashFlows::previousCouponRate(leg_, settlementDate);
    }

    QuantLib::Rate Leg::nextCouponRate(const Date& settlementDate) const {
        return QuantLib::CashFlows::nextCouponRate(leg_, settlementDate);
    }

    QuantLib::Real Leg::accruedAmount(const Date& settlementDate) const {
        return QuantLib::CashFlows::accruedAmount(leg_, settlementDate);
    }





    QuantLib::Real Leg::npv(const YieldTermStructure& hYTS,
                            QuantLib::Date settlementDate,
                            const QuantLib::Date& npvDate,
                            QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::npv(leg_, hYTS,
                                        settlementDate, npvDate,
                                        exDividendDays);
    }

    QuantLib::Real Leg::bps(const YieldTermStructure& hYTS,
                            QuantLib::Date settlementDate,
                            const QuantLib::Date& npvDate,
                            QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::bps(leg_, hYTS,
                                        settlementDate, npvDate,
                                        exDividendDays);
    }

    QuantLib::Rate Leg::atmRate(const YieldTermStructure& hYTS,
                                QuantLib::Date settlementDate,
                                const QuantLib::Date& npvDate,
                                QuantLib::Natural exDividendDays,
                                QuantLib::Real npv) const {
        return QuantLib::CashFlows::atmRate(leg_, hYTS,
                                            settlementDate, npvDate,
                                            exDividendDays,
                                            npv);
    }






    QuantLib::Real Leg::npv(QuantLib::Rate y,
                            const QuantLib::DayCounter& dayCounter,
                            QuantLib::Compounding compounding,
                            QuantLib::Frequency frequency,
                            QuantLib::Date settlementDate,
                            QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::npv(leg_,
                                        y, dayCounter, compounding, frequency,
                                        settlementDate, exDividendDays);
    }

    QuantLib::Real Leg::bps(QuantLib::Rate y,
                            const QuantLib::DayCounter& dayCounter,
                            QuantLib::Compounding compounding,
                            QuantLib::Frequency frequency,
                            QuantLib::Date settlementDate,
                            QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::bps(leg_,
                                        y, dayCounter, compounding, frequency,
                                        settlementDate, exDividendDays);
    }

    QuantLib::Rate Leg::yield(QuantLib::Real npv,
                              const QuantLib::DayCounter& dayCounter,
                              QuantLib::Compounding compounding,
                              QuantLib::Frequency frequency,
                              QuantLib::Date settlementDate,
                              QuantLib::Natural exDividendDays,
                              QuantLib::Real accuracy,
                              QuantLib::Size maxIterations,
                              QuantLib::Rate guess) const {
        return QuantLib::CashFlows::yield(leg_, npv,
                                          dayCounter, compounding, frequency,
                                          settlementDate,
                                          exDividendDays,
                                          accuracy, maxIterations, guess);
    }

    QuantLib::Time Leg::duration(QuantLib::Rate y,
                                 const QuantLib::DayCounter& dayCounter,
                                 QuantLib::Compounding compounding,
                                 QuantLib::Frequency frequency,
                                 QuantLib::Duration::Type type,
                                 QuantLib::Date settlementDate,
                                 QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::duration(leg_,
                                             y, dayCounter, compounding, frequency,
                                             type,
                                             settlementDate, exDividendDays);
    }

    QuantLib::Time Leg::convexity(QuantLib::Rate y,
                                  const QuantLib::DayCounter& dayCounter,
                                  QuantLib::Compounding compounding,
                                  QuantLib::Frequency frequency,
                                  QuantLib::Date settlementDate,
                                  QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::convexity(leg_,
                                             y, dayCounter, compounding, frequency,
                                             settlementDate, exDividendDays);
    }

    QuantLib::Time Leg::basisPointValue(QuantLib::Rate y,
                                        const QuantLib::DayCounter& dayCounter,
                                        QuantLib::Compounding compounding,
                                        QuantLib::Frequency frequency,
                                        QuantLib::Date settlementDate,
                                        QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::basisPointValue(leg_,
                                             y, dayCounter, compounding, frequency,
                                             settlementDate, exDividendDays);
    }

    QuantLib::Time Leg::yieldValueBasisPoint(QuantLib::Rate y,
                                             const QuantLib::DayCounter& dayCounter,
                                             QuantLib::Compounding compounding,
                                             QuantLib::Frequency frequency,
                                             QuantLib::Date settlementDate,
                                             QuantLib::Natural exDividendDays) const {
        return QuantLib::CashFlows::yieldValueBasisPoint(leg_,
                                             y, dayCounter, compounding, frequency,
                                             settlementDate, exDividendDays);
    }







        QuantLib::Real Leg::npv(const boost::shared_ptr<QuantLib::YieldTermStructure>& d,
                           QuantLib::Spread zSpread,
                           const QuantLib::DayCounter& dayCounter,
                           QuantLib::Compounding compounding,
                           QuantLib::Frequency frequency,
                           QuantLib::Date settlementDate,
                           const QuantLib::Date& npvDate,
                           QuantLib::Natural exDividendDays) const {
            return  QuantLib::CashFlows::npv(leg_, d,
                                             zSpread, dayCounter, compounding, frequency,
                                             settlementDate, npvDate,
                                             exDividendDays);
        }

        QuantLib::Spread Leg::zSpread(QuantLib::Real npv,
                                 const boost::shared_ptr<QuantLib::YieldTermStructure>& d,
                                 const QuantLib::DayCounter& dayCounter,
                                 QuantLib::Compounding compounding,
                                 QuantLib::Frequency frequency,
                                 QuantLib::Date settlementDate,
                                 const QuantLib::Date& npvDate,
                                 QuantLib::Natural exDividendDays,
                                 QuantLib::Real accuracy,
                                 QuantLib::Size maxIterations,
                                 QuantLib::Rate guess) const {
            return  QuantLib::CashFlows::zSpread(leg_, npv, d,
                                                 dayCounter, compounding, frequency,
                                                 settlementDate, npvDate,
                                                 exDividendDays,
                                                 accuracy, maxIterations, guess);
        }











    std::vector<std::vector<ObjectHandler::property_t> > Leg::analysis() const {
        return flowAnalysis(leg_);
    }

    void Leg::setCouponPricers(
                const vector<shared_ptr<QuantLibAddin::FloatingRateCouponPricer> >& pricers) {
        std::vector<std::string> ids;
        std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> > ql_pricers;
        std::vector<boost::shared_ptr<QuantLibAddin::FloatingRateCouponPricer> >::const_iterator i;
        for (i = pricers.begin(); i != pricers.end(); ++i) {
            ids.push_back((*i)->properties()->objectId());
            boost::shared_ptr<QuantLib::FloatingRateCouponPricer> p;
            (*i)->getLibraryObject(p);
            ql_pricers.push_back(p);
        }
        QuantLib::setCouponPricers(leg_, ql_pricers);

        boost::shared_ptr<ObjectHandler::ValueObject> inst_properties = properties();
        inst_properties->setProperty("UserLegIDs", ids);
    }

    const QuantLib::Leg& Leg::getQuantLibLeg() {
        return leg_;
    }

    MultiPhaseLeg::MultiPhaseLeg(
                    const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    const std::vector<boost::shared_ptr<Leg> >& streams,
                    bool toBeSorted,
                    bool permanent) : Leg(properties, permanent) {
        for (QuantLib::Size i=0; i<streams.size(); ++i) {
            const QuantLib::Leg& leg = streams[i]->getQuantLibLeg();
            leg_.insert(leg_.end(), leg.begin(), leg.end());
        }
        if (toBeSorted)
            std::stable_sort(leg_.begin(), leg_.end(),
                             earlier_than<boost::shared_ptr<CashFlow> >());
    };

    SimpleCashFlowVector::SimpleCashFlowVector(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                               const std::vector<QuantLib::Real>& amounts,
                                               const std::vector<QuantLib::Date>& dates,
                                               bool permanent) : Leg(properties, permanent)
    {
        QL_REQUIRE(!amounts.empty(), "Amounts vector must have at least one element");
        QL_REQUIRE(amounts.size() == dates.size(), "Dates and amounts vector must have the same size");
        for (QuantLib::Size i=0; i < amounts.size(); ++i) {
            leg_.push_back(boost::shared_ptr<CashFlow>(new
                QuantLib::SimpleCashFlow(amounts[i], dates[i])));
        }
    }

    InterestRate::InterestRate(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Rate r,
            const QuantLib::DayCounter& dc,
            QuantLib::Compounding comp,
            QuantLib::Frequency freq,
            bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::InterestRate>(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::InterestRate>(new
            QuantLib::InterestRate(r, dc, comp, freq));
    }

}
