
/*
 Copyright (C) 2006, 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Walter Penschke

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/bonds.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/swaptionvolstructure.hpp>
#include <qlo/capletvolstructure.hpp>
#include <ql/instruments/fixedratebond.hpp>
#include <ql/instruments/zerocouponbond.hpp>
#include <ql/instruments/cmsratebond.hpp>
#include <ql/instruments/floatingratebond.hpp>
#include <ql/cashflows/analysis.hpp>

namespace QuantLibAddin {

    std::vector<std::vector<boost::any> > Bond::flowAnalysis()
    {
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();

        return QuantLibAddin::flowAnalysis(cashflows);
    }

    void Bond::setPricer(const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer){
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::CashFlows::setPricer(cashflows, pricer);
    }

    void Bond::setPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers){
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::CashFlows::setPricers(cashflows, pricers);
    } 

    ZeroCouponBond::ZeroCouponBond(
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const QuantLib::Calendar& calendar,
            const QuantLib::Date& maturityDate,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des, cur) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::ZeroCouponBond(settlementDays, faceAmount,
                                     calendar, maturityDate,
                                     dayCounter, paymentConvention,
                                     redemption, issueDate, hYTS));

    }

    FixedRateBond::FixedRateBond(
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention payConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& sch,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& payDayCounter,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des, cur) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::FixedRateBond(settlementDays, faceAmount, *sch,
                                    coupons, payDayCounter, payConvention,
                                    redemption, issueDate, hYTS));
    }

     FloatingRateBond::FloatingRateBond( 
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des, cur) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::FloatingRateBond(settlementDays, faceAmount, *schedule,
                                       index, paymentDayCounter,
                                       paymentConvention, fixingDays,
                                       gearings, spreads,
                                       caps, floors,
                                       inArrears,
                                       redemption, issueDate, hYTS));
        }

    CmsRateBond::CmsRateBond( 
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des, cur) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::CmsRateBond(settlementDays, faceAmount, *schedule,
                                  index, paymentDayCounter,
                                  paymentConvention, fixingDays,
                                  gearings, spreads,
                                  caps, floors,
                                  inArrears,
                                  redemption, issueDate, hYTS));
    }

}
