
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

#ifndef qla_bonds_hpp
#define qla_bonds_hpp

#include <qlo/baseinstruments.hpp>
#include <qlo/index.hpp>
#include <oh/objhandler.hpp>
#include <qlo/schedule.hpp>
#include <qlo/analysis.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/swaptionvolstructure.hpp>

#include <ql/CashFlows/conundrumpricer.hpp>
#include <ql/Volatilities/all.hpp>
#include <ql/Indexes/iborindex.hpp>
#include <ql/Indexes/swapindex.hpp>
//#include <ql/currency.hpp>
#include <string>

namespace QuantLib {
    class Currency;
}

namespace QuantLibAddin {


    class Bond : public Instrument {
      public:
        Bond(const std::string& des,
             const QuantLib::Currency& cur)
        : description_(des), currency_(cur) {}
       
        virtual ~Bond() {}
        std::vector<std::vector<boost::any> > flowAnalysis();
        const std::string& description() { return description_; }
        const QuantLib::Currency& currency() { return currency_; }
        void setPricer(const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer);
        void setPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers);

      private:
        std::string description_;
        QuantLib::Currency currency_;
    };

    class ZeroCouponBond : public Bond {
      public:
        ZeroCouponBond(
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
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

    class FixedRateBond : public Bond {
      public:
        FixedRateBond(
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& paymentDayCounter,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

    class FloatingRateBond : public Bond {
      public:
        FloatingRateBond(
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
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
        };

    class CmsRateBond : public Bond {
      public:
        CmsRateBond(
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
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

}

#endif
