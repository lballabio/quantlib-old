
/*
 Copyright (C) 2006, 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Walter Penschke

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

namespace QuantLibAddin {

    std::vector<std::vector<boost::any> > Bond::flowAnalysis()
    {
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();

        return QuantLibAddin::flowAnalysis(cashflows);
    }

    std::string Bond::description() {
        return boost::any_cast<std::string>(propertyValue("DESCRIPTION"));
    }

    //QuantLib::Currency Bond::currency() {
    //    return boost::any_cast<QuantLib::Currency>(propertyValue("Currency"));
    //}

    std::string Bond::currency() {
        return boost::any_cast<std::string>(propertyValue("CURRENCY"));
    }

    void Bond::setCouponPricer(const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer){
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricer(cashflows, pricer);
    }

    void Bond::setCouponPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers){
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricers(cashflows, pricers);
    }

    GenericBond::GenericBond(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                             const std::string&,
                             const QuantLib::Currency&,
                             QuantLib::Natural settlementDays,
                             const QuantLib::Calendar& calendar,
                             QuantLib::Real faceAmount,
                             const QuantLib::Date& maturityDate,
                             const QuantLib::Date& issueDate,
                             const boost::shared_ptr<Leg>& leg,
                             bool permanent) : Bond(properties, permanent)

    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::Bond(settlementDays,
                           calendar,
                           faceAmount,
                           maturityDate,
                           issueDate,
                           leg->getQuantLibLeg()));
    }

    ZeroCouponBond::ZeroCouponBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const QuantLib::Date& maturityDate,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::ZeroCouponBond(settlementDays,
                                     calendar,
                                     faceAmount,
                                     maturityDate,
                                     paymentConvention,
                                     redemption, issueDate));
    }

    FixedRateBond::FixedRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::FixedRateBond(settlementDays, faceAmount,
                                    *schedule,
                                    coupons, accrualDayCounter,
                                    paymentConvention,
                                    redemption, issueDate));
    }

    FixedRateBond::FixedRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const QuantLib::Date& startDate,
            const QuantLib::Date& maturityDate,
            const QuantLib::Period& tenor,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention accrualConvention,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& stubDate,
            bool fromEnd,
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::FixedRateBond(settlementDays, calendar, faceAmount,
                                    startDate, maturityDate, tenor,
                                    coupons, accrualDayCounter,
                                    accrualConvention, paymentConvention,
                                    redemption,
                                    issueDate, stubDate,
                                    fromEnd));
    }


    FloatingRateBond::FloatingRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
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
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::FloatingRateBond(settlementDays, faceAmount, *schedule,
                                       index, paymentDayCounter,
                                       paymentConvention, fixingDays,
                                       gearings, spreads,
                                       caps, floors,
                                       inArrears,
                                       redemption, issueDate));
    }

    CmsRateBond::CmsRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string&,
            const QuantLib::Currency&,
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
            bool permanent) : Bond(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::CmsRateBond(settlementDays, faceAmount, *schedule,
                                  index, paymentDayCounter,
                                  paymentConvention, fixingDays,
                                  gearings, spreads,
                                  caps, floors,
                                  inArrears,
                                  redemption, issueDate));
    }

}

