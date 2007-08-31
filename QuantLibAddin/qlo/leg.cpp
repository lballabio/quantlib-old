
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
//#include <qlo/termstructures.hpp>
#include <qlo/Enumerations/Factories/iborcouponpricersfactory.hpp>

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

    QuantLib::Rate Leg::previousCouponRate(const Date& refDate) const {
        return QuantLib::CashFlows::previousCouponRate(leg_, refDate);
    }

    QuantLib::Rate Leg::currentCouponRate(const Date& refDate) const {
        return QuantLib::CashFlows::currentCouponRate(leg_, refDate);
    }

    QuantLib::Date Leg::startDate() const {
        return QuantLib::CashFlows::startDate(leg_);
    }

    QuantLib::Date Leg::maturityDate() const {
        return QuantLib::CashFlows::maturityDate(leg_);
    }

    QuantLib::Real Leg::npv(const YieldTermStructure& hYTS) const {
        return QuantLib::CashFlows::npv(leg_, hYTS);
    }

    QuantLib::Real Leg::bps(const YieldTermStructure& hYTS) const {
        return QuantLib::CashFlows::bps(leg_, hYTS);
    }

    QuantLib::Rate Leg::atmRate(const YieldTermStructure& hYTS) const {
        return QuantLib::CashFlows::atmRate(leg_, hYTS);
    }
    QuantLib::Rate Leg::irr(QuantLib::Real marketPrice,
                            const QuantLib::DayCounter& dayCounter,
                            QuantLib::Compounding compounding,
                            QuantLib::Frequency frequency,
                            QuantLib::Date settlementDate,
                            QuantLib::Real tolerance,
                            QuantLib::Size maxIterations,
                            QuantLib::Rate guess) const {
        return QuantLib::CashFlows::irr(leg_, marketPrice, dayCounter,
                                        compounding, frequency,
                                        settlementDate, tolerance,
                                        maxIterations, guess);
    }
    QuantLib::Time Leg::duration(const QuantLib::InterestRate& y,
                        QuantLib::Duration::Type type,
                        QuantLib::Date settlementDate) const {
        return QuantLib::CashFlows::duration(leg_, y, type, settlementDate);
    }
    QuantLib::Real Leg::convexity(const QuantLib::InterestRate& y,
                                QuantLib::Date settlementDate) const { 
        return QuantLib::CashFlows::convexity(leg_, y, settlementDate);
    }
    std::vector<std::vector<boost::any> > Leg::analysis() const {
        return flowAnalysis(leg_);
    }

    void Leg::setCouponPricers(
                const vector<shared_ptr<FloatingRateCouponPricer> >& pricers) {
        return QuantLib::setCouponPricers(leg_, pricers);
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
          
    InterestRate::InterestRate(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
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

