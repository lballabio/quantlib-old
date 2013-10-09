/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
	Copyright (C) 2013 Mehdi Bouassab

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

#include <ql/pricingengines/swap/discountingxccyswapengine.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/utilities/dataformatters.hpp>

namespace QuantLib {

    DiscountingXccySwapEngine::DiscountingXccySwapEngine(
							const Handle<YieldTermStructure>& floatDiscountcurve,
							const Handle<YieldTermStructure>& fixedDiscountcurve,
							const Handle<Quote>& FxSpot,
                            boost::optional<bool> includeSettlementDateFlows,
                            Date settlementDate,
                            Date npvDate)
    : floatDiscountcurve_(floatDiscountcurve),
	  fixedDiscountcurve_(fixedDiscountcurve),
	  FxSpot_(FxSpot),
      includeSettlementDateFlows_(includeSettlementDateFlows),
      settlementDate_(settlementDate), npvDate_(npvDate) {
        registerWith(floatDiscountcurve_);
		registerWith(fixedDiscountcurve_);
    }

    void DiscountingXccySwapEngine::calculate() const {
        QL_REQUIRE(!floatDiscountcurve_.empty(),
                   "float leg discounting term structure handle is empty");
		QL_REQUIRE(!fixedDiscountcurve_.empty(),
			       "fixed leg discounting term structure handle is empty");
		QL_REQUIRE(!(FxSpot_->value()==0),"FxSpot should not be null");

        results_.value = 0.0;
        results_.errorEstimate = Null<Real>();

		//checker que c fait avec max
		Date float_refDate=floatDiscountcurve_->referenceDate();
		Date fixed_refDate=fixedDiscountcurve_->referenceDate();

		Date refDate = std::max<Date>(float_refDate,fixed_refDate);

        Date settlementDate = settlementDate_;

        if (settlementDate_==Date()) {
            settlementDate = refDate;
        } else {
            QL_REQUIRE(settlementDate>=refDate,
                       "settlement date (" << settlementDate << ") before "
                       "discount curve reference date (" << refDate << ")");
        }

        results_.valuationDate = npvDate_;
        if (npvDate_==Date()) {
            results_.valuationDate = refDate;
        } else {
            QL_REQUIRE(npvDate_>=refDate,
                       "npv date (" << npvDate_  << ") before "
                       "discount curve reference date (" << refDate << ")");
        }
       
		//npv of the receiver leg
		if(arguments_.type) {
			results_.npvDateDiscount = fixedDiscountcurve_->discount(results_.valuationDate);
		}
		else{
			results_.npvDateDiscount = floatDiscountcurve_->discount(results_.valuationDate);
		}

		//n=2 for our case
        Size n = arguments_.legs.size();
        results_.legNPV.resize(n);
        results_.legBPS.resize(n);
        results_.startDiscounts.resize(n);
        results_.endDiscounts.resize(n);

        bool includeRefDateFlows =
            includeSettlementDateFlows_ ?
            *includeSettlementDateFlows_ :
            Settings::instance().includeReferenceDateEvents();

        for (Size i=0; i<n; ++i) {
			//need exchange rates
			Real exchange_rate= (i%2) ? 1/FxSpot_->value() : 1;
            try {

				const YieldTermStructure& discount_ref= (i%2) ? **floatDiscountcurve_ : **fixedDiscountcurve_;

                CashFlows::npvbps(arguments_.legs[i],
                                  discount_ref,
                                  includeRefDateFlows,
                                  settlementDate,
                                  results_.valuationDate,
                                  results_.legNPV[i],
                                  results_.legBPS[i]);
                results_.legNPV[i] *= arguments_.payer[i];
                results_.legBPS[i] *= arguments_.payer[i];

				//all the cashfows are expressed in the fixed leg currency
				results_.legNPV[i] *= exchange_rate;
				results_.legBPS[i] *= exchange_rate;

                Date d1 = CashFlows::startDate(arguments_.legs[i]);
                if (d1>=refDate)
                   results_.startDiscounts[i] = discount_ref.discount(d1);
                else
                   results_.startDiscounts[i] = Null<DiscountFactor>();

                Date d2 = CashFlows::maturityDate(arguments_.legs[i]);
                if (d2>=refDate)
                   results_.endDiscounts[i] =   discount_ref.discount(d2);
                else
                   results_.endDiscounts[i] = Null<DiscountFactor>();

            } catch (std::exception &e) {
                QL_FAIL(io::ordinal(i+1) << " leg: " << e.what());
            }

			
            results_.value += results_.legNPV[i];
        }
    }

}
