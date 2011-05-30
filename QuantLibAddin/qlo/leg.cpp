/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2010 Ferdinando Ametrano
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

#include <ql/instruments/capfloor.hpp>
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
using QuantLib::Null;
using QuantLib::YieldTermStructure;

using std::vector;

using boost::shared_ptr;

namespace QuantLibAddin {

    Leg::Leg(const shared_ptr<ValueObject>& p,
             const vector<Real>& amounts,
             const vector<Date>& dates,
             bool toBeSorted,
             bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::Leg>(p, permanent)
    {
        QL_REQUIRE(amounts.size() == dates.size(),
                   "Dates (" << dates.size() << ") and amounts (" <<
                   amounts.size() << ") must have the same size");

        libraryObject_ = shared_ptr<QuantLib::Leg>(new QuantLib::Leg());

        for (QuantLib::Size i=0; i<amounts.size(); ++i) {
            if (dates[i]!=Date())
                libraryObject_->push_back(shared_ptr<CashFlow>(new
                    QuantLib::SimpleCashFlow(amounts[i], dates[i])));
            else
                QL_REQUIRE(amounts[i]==0 || amounts[i]==Null<Real>(),
                           "non-null amount (" << amounts[i] << ") on null date");
        }

        if (toBeSorted)
            std::stable_sort(libraryObject_->begin(), libraryObject_->end(),
                             earlier_than<shared_ptr<CashFlow> >());
    }

    Leg::Leg(const shared_ptr<ValueObject>& p,
             const boost::shared_ptr<QuantLib::CapFloor>& capFloor,
             bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::Leg>(p, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Leg>(new QuantLib::Leg());
        *libraryObject_ = capFloor->floatingLeg();
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
        QuantLib::setCouponPricers(*libraryObject_, ql_pricers);

        shared_ptr<ObjectHandler::ValueObject> inst_properties = properties();
        inst_properties->setProperty("UserLegIDs", ids);
    }

    vector<vector<ObjectHandler::property_t> > Leg::flowAnalysis() const {
        return QuantLibAddin::flowAnalysis(*libraryObject_);
    }

    MultiPhaseLeg::MultiPhaseLeg(const shared_ptr<ValueObject>& p,
                                 const vector<shared_ptr<Leg> >& legs,
                                 bool toBeSorted,
                                 bool permanent)
    : Leg(p, permanent) {

        libraryObject_ = shared_ptr<QuantLib::Leg>(new QuantLib::Leg());

        shared_ptr<QuantLib::Leg> leg;
        for (QuantLib::Size i=0; i<legs.size(); ++i) {
            legs[i]->getLibraryObject(leg);
            libraryObject_->insert(libraryObject_->end(),
                                   leg->begin(), leg->end());
        }
        if (toBeSorted)
            std::stable_sort(libraryObject_->begin(), libraryObject_->end(),
                             earlier_than<shared_ptr<CashFlow> >());
    };

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
