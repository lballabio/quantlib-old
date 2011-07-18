/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Chiara Fornarola
 Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Walter Penschke
 Copyright (C) 2009 Piter Dias

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/bonds.hpp>
#include <qlo/flowanalysis.hpp>

#include <ql/instruments/bonds/fixedratebond.hpp>
#include <ql/instruments/bonds/zerocouponbond.hpp>
#include <ql/instruments/bonds/cmsratebond.hpp>
#include <ql/instruments/bonds/floatingratebond.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/pricingengines/bond/discountingbondengine.hpp>
#include <ql/currency.hpp>
#include <ql/interestrate.hpp>

using std::vector;
using boost::shared_ptr;
using ObjectHandler::property_t;

namespace QuantLibAddin {

    vector<vector<property_t> > Bond::flowAnalysis(const QuantLib::Date& d)
    {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();

        return QuantLibAddin::flowAnalysis(cashflows, d);
    }

    QuantLib::Real Bond::redemptionAmount() {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        return temp->redemption()->amount();
    }

    QuantLib::Date Bond::redemptionDate() {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        return temp->redemption()->date();
    }

    std::string Bond::description() {
        return boost::get<std::string>(propertyValue("DESCRIPTION"));
    }

    //QuantLib::Currency Bond::currency() {
    //    return boost::get<QuantLib::Currency>(propertyValue("CURRENCY"));
    //}

    std::string Bond::currency() {
        return boost::get<std::string>(propertyValue("CURRENCY"));
    }

    void Bond::setCouponPricer(const shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer) {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricer(cashflows, pricer);
    }

    void Bond::setCouponPricers(const vector<shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers) {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricers(cashflows, pricers);
    }

    Bond::Bond(const shared_ptr<ObjectHandler::ValueObject>& properties,
               const std::string&,
               const QuantLib::Currency&,
               QuantLib::Natural settlementDays,
               const QuantLib::Calendar& calendar,
               QuantLib::Real faceAmount,
               const QuantLib::Date& maturityDate,
               const QuantLib::Date& issueDate,
               const QuantLib::Leg& leg,
               bool permanent) : Instrument(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::Bond(settlementDays,
                           calendar,
                           faceAmount,
                           maturityDate,
                           issueDate,
                           leg));
    }

    ZeroCouponBond::ZeroCouponBond(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const QuantLib::Date& maturityDate,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::ZeroCouponBond(settlementDays,
                                     calendar,
                                     faceAmount,
                                     maturityDate,
                                     paymentConvention,
                                     redemption, issueDate));
    }

    FixedRateBond::FixedRateBond(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Calendar& paymentCalendar,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::FixedRateBond(settlementDays, faceAmount,
                                    *schedule,
                                    coupons, accrualDayCounter,
                                    paymentConvention,
                                    redemption, issueDate,
                                    paymentCalendar));
    }

    FixedRateBond::FixedRateBond(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<boost::shared_ptr<QuantLib::InterestRate> >& coupons,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Calendar& paymentCalendar,
            bool permanent)
    : Bond(properties, permanent)
    {
		vector<QuantLib::InterestRate> couponRate(coupons.size());

		for (QuantLib::Size i=0; i<coupons.size(); ++i)
			couponRate[i] = *coupons[i];

        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::FixedRateBond(settlementDays, faceAmount,
                                    *schedule,
                                    couponRate,
                                    paymentConvention,
									redemption,
                                    issueDate,
                                    paymentCalendar));
    }

    FloatingRateBond::FloatingRateBond(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const vector<QuantLib::Rate>& floors,
            const vector<QuantLib::Real>& gearings,
            const shared_ptr<QuantLib::IborIndex>& index,
            const vector<QuantLib::Spread>& spreads,
            const vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent)
    : Bond(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::FloatingRateBond(settlementDays, faceAmount, *schedule,
                                       index, paymentDayCounter,
                                       paymentConvention, fixingDays,
                                       gearings, spreads,
                                       caps, floors,
                                       inArrears,
                                       redemption, issueDate));
    }

    CmsRateBond::CmsRateBond(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const vector<QuantLib::Rate>& floors,
            const vector<QuantLib::Real>& gearings,
            const shared_ptr<QuantLib::SwapIndex>& index,
            const vector<QuantLib::Spread>& spreads,
            const vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Instrument>(new
            QuantLib::CmsRateBond(settlementDays, faceAmount, *schedule,
                                  index, paymentDayCounter,
                                  paymentConvention, fixingDays,
                                  gearings, spreads,
                                  caps, floors,
                                  inArrears,
                                  redemption, issueDate));
    }

}

