
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/bonds.hpp>
#include <qlo/couponvectors.hpp>
#include <qlo/termstructures.hpp>
#include <qlo/typefactory.hpp>
#include <qlo/swaptionvolstructure.hpp>
#include <qlo/capletvolstructure.hpp>
#include <ql/Instruments/fixedcouponbond.hpp>
#include <ql/Instruments/zerocouponbond.hpp>
#include <ql/Instruments/cmscouponbond.hpp>
#include <ql/Instruments/cappedflooredcouponbond.hpp>

namespace QuantLibAddin {

    std::vector<std::vector<boost::any> > Bond::flowAnalysis()
    {
        boost::shared_ptr<QuantLib::Bond> temp;
        getLibraryObject(temp);
        const std::vector<boost::shared_ptr<QuantLib::CashFlow> >& cashflows = 
            temp->cashflows();

        return QuantLibAddin::flowAnalysis(cashflows);
    }

    ZeroCouponBond::ZeroCouponBond(
            const std::string& des,
            QuantLib::Real faceAmount,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Integer settlementDays,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            QuantLib::Real redemption,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::ZeroCouponBond(faceAmount,
                                         issueDate,
                                         maturityDate,
                                         settlementDays,
                                         dayCounter,
                                         calendar,
                                         convention,
                                         redemption,
                                         hYTS));

    }

    FixedCouponBond::FixedCouponBond(
            const std::string& des,
            QuantLib::Real faceAmount,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& datedDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Integer settlementDays,
            const std::vector<double>& coupons,
            QuantLib::Real redemption,
            const QuantLib::Frequency& frequency,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::BusinessDayConvention accrualConvention,
            QuantLib::BusinessDayConvention paymentConvention,
            const QuantLib::Calendar& calendar,
            bool startFromEnd,
            const QuantLib::Date& stub,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    : Bond(des) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::FixedCouponBond(faceAmount,
                                          issueDate,
                                          datedDate,
                                          maturityDate,
                                          settlementDays,
                                          coupons,
                                          frequency,
                                          calendar,
                                          dayCounter,
                                          accrualConvention,
                                          paymentConvention,
                                          redemption,
                                          hYTS,
                                          stub,
                                          startFromEnd));
    }


    CmsCouponBond::CmsCouponBond( 
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
                            const std::vector<QuantLib::Rate>& caps,
                            const std::vector<QuantLib::Rate>& floors,
                            QuantLib::BusinessDayConvention accrualConvention,
                            QuantLib::BusinessDayConvention paymentConvention,
                            QuantLib::Real redemption,
                            const boost::shared_ptr<QuantLib::CmsCouponPricer>& pricer,
                            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                            const QuantLib::Date& stub,
                            bool fromEnd)
        : Bond(des) {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
            QuantLib::CmsCouponBond(
                                        settlementDays,
                                        issueDate,
                                        calendar,
                                        datedDate,
                                        couponFrequency,
                                        maturityDate,
                                        accrualConvention,

                                        faceAmount,

                                        index,
                                        dayCounter,

                                        pricer,

                                        fixingDays,
                                        paymentConvention,

                                        gearings,
                                        spreads,
                                        caps,
                                        floors,

                                        hYTS,
                                        redemption,
                                        stub, 
                                        fromEnd));
}

     CappedFlooredCouponBond::CappedFlooredCouponBond( 
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
                            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& volatility,
                            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                            const QuantLib::Date& stub,
                            bool fromEnd)
        : Bond(des) {
            libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(new
                QuantLib::CappedFlooredCouponBond(
                                        settlementDays,
                                        issueDate,
                                        calendar,
                                        datedDate,
                                        couponFrequency,
                                        maturityDate,
                                        accrualConvention,

                                        faceAmount,

                                        index,
                                        dayCounter,

                                        volatility,

                                        fixingDays,
                                        paymentConvention,

                                        gearings,
                                        spreads,
                                        caps,
                                        floors,

                                        hYTS,
                                        redemption,
                                        stub, 
                                        fromEnd));
        }
}
