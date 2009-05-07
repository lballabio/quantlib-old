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

using ObjectHandler::ValueObject;
using ObjectHandler::LibraryObject;

using QuantLib::earlier_than;
using QuantLib::CashFlow;
using QuantLib::CashFlows;
using QuantLib::Date;
using QuantLib::Real;
using QuantLib::Time;
using QuantLib::Rate;
using QuantLib::Spread;
using QuantLib::YieldTermStructure;

using std::vector;

using boost::shared_ptr;

namespace QuantLibAddin {

    Date Leg::startDate() const {
        return CashFlows::startDate(leg_);
    }

    Date Leg::maturityDate() const {
        return CashFlows::maturityDate(leg_);
    }

    bool Leg::isExpired(bool includeSettlementDateFlows,
                        const Date& refDate) const {
        return CashFlows::isExpired(leg_,
                                    includeSettlementDateFlows,
                                    refDate);
    }


    Date Leg::previousCashFlowDate(bool includeSettlementDateFlows,
                                   const Date& refDate) const {
        return CashFlows::previousCashFlowDate(leg_,
                                               includeSettlementDateFlows,
                                               refDate);
    }

    Date Leg::nextCashFlowDate(bool includeSettlementDateFlows,
                               const Date& refDate) const {
        return CashFlows::nextCashFlowDate(leg_,
                                           includeSettlementDateFlows,
                                           refDate);
    }

    Real Leg::previousCashFlowAmount(bool includeSettlementDateFlows,
                                     const Date& refDate) const {
        return CashFlows::previousCashFlowAmount(leg_,
                                                 includeSettlementDateFlows,
                                                 refDate);
    }

    Real Leg::nextCashFlowAmount(bool includeSettlementDateFlows,
                                 const Date& refDate) const {
        return CashFlows::nextCashFlowAmount(leg_,
                                             includeSettlementDateFlows,
                                             refDate);
    }



    Rate Leg::previousCouponRate(bool includeSettlementDateFlows,
                                 const Date& settlementDate) const {
        return CashFlows::previousCouponRate(leg_,
                                             includeSettlementDateFlows,
                                             settlementDate);
    }

    Rate Leg::nextCouponRate(bool includeSettlementDateFlows,
                             const Date& settlementDate) const {
        return CashFlows::nextCouponRate(leg_,
                                         includeSettlementDateFlows,
                                         settlementDate);
    }

    Real Leg::accruedAmount(bool includeSettlementDateFlows,
                            const Date& settlementDate) const {
        return CashFlows::accruedAmount(leg_,
                                        includeSettlementDateFlows,
                                        settlementDate);
    }



    Real Leg::npv(const YieldTermStructure& hYTS,
                  bool includeSettlementDateFlows,
                  Date settlementDate,
                  const Date& npvDate) const {
        return CashFlows::npv(leg_, hYTS,
                              includeSettlementDateFlows,
                              settlementDate, npvDate);
    }

    Real Leg::bps(const YieldTermStructure& hYTS,
                  bool includeSettlementDateFlows,
                  Date settlementDate,
                  const Date& npvDate) const {
        return CashFlows::bps(leg_, hYTS, 
                              includeSettlementDateFlows,
                              settlementDate, npvDate);
    }

    Rate Leg::atmRate(const YieldTermStructure& hYTS,
                      bool includeSettlementDateFlows,
                      Date settlementDate,
                      const Date& npvDate,
                      Real npv) const {
        return CashFlows::atmRate(leg_, hYTS,
                                  includeSettlementDateFlows,
                                  settlementDate, npvDate,
                                  npv);
    }


    Real Leg::npv(QuantLib::Rate y,
                  const QuantLib::DayCounter& dayCounter,
                  QuantLib::Compounding compounding,
                  QuantLib::Frequency frequency,
                  bool includeSettlementDateFlows,
                  Date settlementDate,
                  const Date& npvDate) const {
        return CashFlows::npv(leg_,
                              y, dayCounter, compounding, frequency,
                              includeSettlementDateFlows,
                              settlementDate, npvDate);
    }

    Real Leg::bps(QuantLib::Rate y,
                  const QuantLib::DayCounter& dayCounter,
                  QuantLib::Compounding compounding,
                  QuantLib::Frequency frequency,
                  bool includeSettlementDateFlows,
                  Date settlementDate,
                  const Date& npvDate) const {
        return CashFlows::bps(leg_,
                              y, dayCounter, compounding, frequency,
                              includeSettlementDateFlows,
                              settlementDate, npvDate);
    }

    QuantLib::Rate Leg::yield(Real npv,
                              const QuantLib::DayCounter& dayCounter,
                              QuantLib::Compounding compounding,
                              QuantLib::Frequency frequency,
                              bool includeSettlementDateFlows,
                              Date settlementDate,
                              const Date& npvDate,
                              Real accuracy,
                              QuantLib::Size maxIterations,
                              QuantLib::Rate guess) const {
        return CashFlows::yield(leg_, npv,
                                dayCounter, compounding, frequency,
                                includeSettlementDateFlows,
                                settlementDate, npvDate,
                                accuracy, maxIterations, guess);
    }

    QuantLib::Time Leg::duration(QuantLib::Rate y,
                                 const QuantLib::DayCounter& dayCounter,
                                 QuantLib::Compounding compounding,
                                 QuantLib::Frequency frequency,
                                 QuantLib::Duration::Type type,
                                 bool includeSettlementDateFlows,
                                 Date settlementDate,
                                 const Date& npvDate) const {
        return CashFlows::duration(leg_,
                                   y, dayCounter, compounding, frequency,
                                   type, includeSettlementDateFlows,
                                   settlementDate, npvDate);
    }

    QuantLib::Time Leg::convexity(QuantLib::Rate y,
                                  const QuantLib::DayCounter& dayCounter,
                                  QuantLib::Compounding compounding,
                                  QuantLib::Frequency frequency,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate,
                                  const Date& npvDate) const {
        return CashFlows::convexity(leg_,
                                    y, dayCounter, compounding, frequency,
                                    includeSettlementDateFlows,
                                    settlementDate, npvDate);
    }

    QuantLib::Time Leg::basisPointValue(QuantLib::Rate y,
                                        const QuantLib::DayCounter& dayCounter,
                                        QuantLib::Compounding compounding,
                                        QuantLib::Frequency frequency,
                                        bool includeSettlementDateFlows,
                                        Date settlementDate,
                                        const Date& npvDate) const {
        return CashFlows::basisPointValue(leg_,
                                          y, dayCounter, compounding, frequency,
                                          includeSettlementDateFlows,
                                          settlementDate, npvDate);
    }

    Time Leg::yieldValueBasisPoint(QuantLib::Rate y,
                                   const QuantLib::DayCounter& dc,
                                   QuantLib::Compounding comp,
                                   QuantLib::Frequency freq,
                                   bool includeSettlementDateFlows,
                                   Date settlementDate,
                                   const Date& npvDate) const {
        return CashFlows::yieldValueBasisPoint(leg_,
                                               y, dc, comp, freq,
                                               includeSettlementDateFlows,
                                               settlementDate, npvDate);
    }



    Real Leg::npv(const shared_ptr<QuantLib::YieldTermStructure>& discountC,
                  QuantLib::Spread zSpread,
                  const QuantLib::DayCounter& dayCounter,
                  QuantLib::Compounding compounding,
                  QuantLib::Frequency frequency,
                  bool includeSettlementDateFlows,
                  Date settlementDate,
                  const Date& npvDate) const {
        return  CashFlows::npv(leg_, discountC,
                               zSpread, dayCounter, compounding, frequency,
                               includeSettlementDateFlows,
                               settlementDate, npvDate);
    }

    Spread Leg::zSpread(Real npv,
                        const shared_ptr<QuantLib::YieldTermStructure>& disc,
                        const QuantLib::DayCounter& dayCounter,
                        QuantLib::Compounding compounding,
                        QuantLib::Frequency frequency,
                        bool includeSettlementDateFlows,
                        Date settlementDate,
                        const Date& npvDate,
                        Real accuracy,
                        QuantLib::Size maxIterations,
                        QuantLib::Rate guess) const {
        return  CashFlows::zSpread(leg_, disc, npv,
                                   dayCounter, compounding, frequency,
                                   includeSettlementDateFlows,
                                   settlementDate, npvDate,
                                   accuracy, maxIterations, guess);
    }

    vector<vector<ObjectHandler::property_t> > Leg::analysis() const {
        return flowAnalysis(leg_);
    }

    void Leg::setCouponPricers(
                const vector<shared_ptr<FloatingRateCouponPricer> >& pricers) {
        vector<std::string> ids;
        vector<shared_ptr<QuantLib::FloatingRateCouponPricer> > ql_pricers;
        vector<shared_ptr<FloatingRateCouponPricer> >::const_iterator i;
        for (i = pricers.begin(); i != pricers.end(); ++i) {
            ids.push_back((*i)->properties()->objectId());
            shared_ptr<QuantLib::FloatingRateCouponPricer> p;
            (*i)->getLibraryObject(p);
            ql_pricers.push_back(p);
        }
        QuantLib::setCouponPricers(leg_, ql_pricers);

        shared_ptr<ObjectHandler::ValueObject> inst_properties = properties();
        inst_properties->setProperty("UserLegIDs", ids);
    }

    const QuantLib::Leg& Leg::getQuantLibLeg() {
        return leg_;
    }

    MultiPhaseLeg::MultiPhaseLeg(const shared_ptr<ValueObject>& p,
                                 const vector<shared_ptr<Leg> >& streams,
                                 bool toBeSorted,
                                 bool permanent)
    : Leg(p, permanent) {
        for (QuantLib::Size i=0; i<streams.size(); ++i) {
            const QuantLib::Leg& leg = streams[i]->getQuantLibLeg();
            leg_.insert(leg_.end(), leg.begin(), leg.end());
        }
        if (toBeSorted)
            std::stable_sort(leg_.begin(), leg_.end(),
                             earlier_than<shared_ptr<CashFlow> >());
    };

    SimpleCashFlowVector::SimpleCashFlowVector(
                                            const shared_ptr<ValueObject>& p,
                                            const vector<Real>& amounts,
                                            const vector<Date>& dates,
                                            bool permanent)
    : Leg(p, permanent) {
        QL_REQUIRE(!amounts.empty(),
                   "Amounts vector must have at least one element");
        QL_REQUIRE(amounts.size() == dates.size(),
                   "Dates and amounts vector must have the same size");
        for (QuantLib::Size i=0; i < amounts.size(); ++i) {
            leg_.push_back(shared_ptr<CashFlow>(new
                QuantLib::SimpleCashFlow(amounts[i], dates[i])));
        }
    }

    InterestRate::InterestRate(const shared_ptr<ValueObject>& properties,
                               QuantLib::Rate r,
                               const QuantLib::DayCounter& dc,
                               QuantLib::Compounding comp,
                               QuantLib::Frequency freq,
                               bool permanent)
    : LibraryObject<QuantLib::InterestRate>(properties, permanent) {
        libraryObject_ = shared_ptr<QuantLib::InterestRate>(new
            QuantLib::InterestRate(r, dc, comp, freq));
    }

}
