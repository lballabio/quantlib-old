
/*
 Copyright (C) 2006, 2007 Chiara Fornarola
 Copyright (C) 2006 Ferdinando Ametrano
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

namespace QuantLibAddin {

    class Bond : public Instrument {
      public:
        Bond(const std::string& des)
        : description_(des) {}
        virtual ~Bond() {}
        std::vector<std::vector<boost::any> > flowAnalysis();
        const std::string& description() { return description_; }
      private:
        std::string description_;
    };

    class ZeroCouponBond : public Bond {
      public:
        ZeroCouponBond(
            const std::string& des,
            QuantLib::Real faceAmount,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Integer settlementDays,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            QuantLib::Real redemption,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

    class FixedCouponBond : public Bond {
      public:
        FixedCouponBond(
            const std::string& des,
            QuantLib::Real faceAmount,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& datedDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Integer settlementDays,
            const std::vector<QuantLib::Rate>& coupons,
            QuantLib::Real redemption,
            const QuantLib::Frequency& frequency,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::BusinessDayConvention accrualConvention,
            QuantLib::BusinessDayConvention paymentConvention,
            const QuantLib::Calendar& calendar,
            bool startFromEnd,
            const QuantLib::Date& stub,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

    class FloatingCouponBond : public Bond {
      public:
        FloatingCouponBond(
            const std::string& des,
            QuantLib::Real faceAmount,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& datedDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Integer settlementDays,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            QuantLib::Integer fixingDays,
            const std::vector<QuantLib::Real>& gearings,
            const std::vector<QuantLib::Spread>& spreads,
            QuantLib::Frequency couponFrequency,
            const QuantLib::Calendar& calendar,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::BusinessDayConvention accrualConvention,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Date& stub,
            bool fromEnd);
      };

 

    class CmsCouponBond : public Bond {
      public:
           CmsCouponBond(
             const std::string& des,
             QuantLib::Real faceAmount,
             const QuantLib::Date& issueDate,
             const QuantLib::Date& datedDate,
             const QuantLib::Date& maturityDate,
             QuantLib::Integer settlementDays,
             const boost::shared_ptr<QuantLib::SwapIndex>& index,
             QuantLib::Integer fixingDays,
             const std::vector<QuantLib::Real>& gearings,
             const std::vector<QuantLib::Spread>& spreads,
             QuantLib::Frequency couponFrequency,
             const QuantLib::Calendar& calendar,
             const QuantLib::DayCounter& dayCounter,
             const boost::shared_ptr<QuantLib::VanillaCMSCouponPricer>& pricer,
             const std::vector<QuantLib::Rate>& caps,
             const std::vector<QuantLib::Rate>& floors,
             QuantLib::BusinessDayConvention accrualConvention,
             QuantLib::BusinessDayConvention paymentConvention,
             QuantLib::Real redemption,
             const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
             const QuantLib::Date& stub,
             bool fromEnd);
    };

    class CappedFlooredCouponBond : public Bond {
          public:
               CappedFlooredCouponBond(
                 const std::string& des,
                 QuantLib::Real faceAmount,
                 const QuantLib::Date& issueDate,
                 const QuantLib::Date& datedDate,
                 const QuantLib::Date& maturityDate,
                 QuantLib::Integer settlementDays,
                 const boost::shared_ptr<QuantLib::IborIndex>& index,
                 QuantLib::Integer fixingDays,
                 const std::vector<QuantLib::Real>& gearings,
                 const std::vector<QuantLib::Spread>& spreads,
                 QuantLib::Frequency couponFrequency,
                 const QuantLib::Calendar& calendar,
                 const QuantLib::DayCounter& dayCounter,
                 const std::vector<QuantLib::Rate>& caps,
                 const std::vector<QuantLib::Rate>& floors,
                 QuantLib::BusinessDayConvention accrualConvention,
                 QuantLib::BusinessDayConvention paymentConvention,
                 QuantLib::Real redemption,
                 const QuantLib::Handle<QuantLib::CapletVolatilityStructure>&,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                 const QuantLib::Date& stub,
                 bool fromEnd);
        };

}

#endif
