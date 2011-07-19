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

#include <oh/repository.hpp>

using std::vector;
using std::string;
using boost::shared_ptr;
using ObjectHandler::property_t;
using ObjectHandler::convert2;
using QuantLib::Size;
using QuantLib::Date;

namespace QuantLibAddin {

    vector<vector<property_t> > Bond::flowAnalysis(const Date& d)
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

    Date Bond::redemptionDate() {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        return temp->redemption()->date();
    }

    string Bond::description() {
        return boost::get<string>(propertyValue("DESCRIPTION"));
    }

    //QuantLib::Currency Bond::currency() {
    //    return boost::get<QuantLib::Currency>(propertyValue("CURRENCY"));
    //}

    string Bond::currency() {
        return boost::get<string>(propertyValue("CURRENCY"));
    }

    void Bond::setCouponPricer(
                const shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer) {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricer(cashflows, pricer);
    }

    void Bond::setCouponPricers(
            const vector<shared_ptr<QuantLib::FloatingRateCouponPricer> >& p) {
        shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const QuantLib::Leg& cashflows = temp->cashflows();
        QuantLib::setCouponPricers(cashflows, p);
    }

    Bond::Bond(const shared_ptr<ObjectHandler::ValueObject>& properties,
               const string&,
               const QuantLib::Currency&,
               QuantLib::Natural settlementDays,
               const QuantLib::Calendar& calendar,
               QuantLib::Real faceAmount,
               const Date& maturityDate,
               const Date& issueDate,
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
            const string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const Date& maturityDate,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const Date& issueDate,
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
            const string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const Date& issueDate,
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
            const string&,
            const QuantLib::Currency&,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const shared_ptr<QuantLib::Schedule>& schedule,
            const vector<shared_ptr<QuantLib::InterestRate> >& coupons,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const Date& issueDate,
            const QuantLib::Calendar& paymentCalendar,
            bool permanent)
    : Bond(properties, permanent)
    {
		vector<QuantLib::InterestRate> couponRate(coupons.size());

		for (Size i=0; i<coupons.size(); ++i)
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
            const string&,
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
            const Date& issueDate,
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
            const string&,
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
            const Date& issueDate,
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

    vector<string> qlBondAlive(const vector<shared_ptr<Bond> >& bonds,
                               Date& refDate)
    {
        if (bonds.empty())
            return vector<string>();

        if (refDate==Date())
            refDate = QuantLib::Settings::instance().evaluationDate();

        vector<string> result;
        Size n = bonds.size();
        result.reserve(n);

        shared_ptr<QuantLib::Bond> qlBond;
        Date settlement, maturity;
        string id;
        for (Size i=0; i<n; ++i) {
            bonds[i]->getLibraryObject(qlBond);
            settlement = qlBond->settlementDate(refDate);
            maturity = qlBond->maturityDate();
            if (settlement<maturity) {
                id = convert2<string>(bonds[i]->propertyValue("OBJECTID"));
                result.push_back(id);
            }
        }
        return result;
    }

    string qlBondMaturityLookup(const vector<shared_ptr<Bond> >& bonds,
                                const Date& maturity)
    {
        shared_ptr<QuantLib::Bond> qlBond;
        for (Size i=0; i<bonds.size(); ++i) {
            bonds[i]->getLibraryObject(qlBond);
            if (maturity==qlBond->maturityDate())
                return convert2<string>(bonds[i]->propertyValue("OBJECTID"));
        }
        // how to return NA() ?
        return string();
    }

    namespace {

        struct BondItem {
            string objectID;
            Date maturityDate;
            BondItem(const string& _objectID,
                     const Date& _maturityDate)
            : objectID(_objectID),
              maturityDate(_maturityDate) {}
            BondItem() {}
        };

        class BondItemSorter {
          public:
            bool operator()(const BondItem& item1,
                            const BondItem& item2) const
            {
                if (item1.maturityDate>item2.maturityDate)
                    return false;
                return true;
            }
        };
    }

    vector<string> qlBondMaturitySort(const vector<shared_ptr<Bond> >& bonds)
    {
        if (bonds.empty())
            return vector<string>();
        Size n = bonds.size();

        // create BondItems
        std::vector<BondItem> bondItems(n);
        string id;
        Date maturity;
        shared_ptr<QuantLib::Bond> qlBond;
        for (Size i=0; i<n; ++i) {
            bonds[i]->getLibraryObject(qlBond);
            maturity = qlBond->maturityDate() ;
            id = convert2<string>(bonds[i]->propertyValue("OBJECTID"));
            bondItems[i] = BondItem(id, maturity);
        }

        // sort BondItems
        std::sort(bondItems.begin(), bondItems.end(), BondItemSorter());

        // fill result
        std::vector<string> result(n);
        for (Size i=0; i<n; ++i)
            result[i] = bondItems[i].objectID;
        return result;
    }

}
