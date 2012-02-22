
/*
 Copyright (C) 2006, 2007 StatPro Italia srl

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

#ifndef quantlib_convertible_bonds_i
#define quantlib_convertible_bonds_i

%include bonds.i
%include callability.i
%include dividends.i
%include options.i

%{
using QuantLib::ConvertibleZeroCouponBond;
using QuantLib::ConvertibleFixedCouponBond;
using QuantLib::ConvertibleFloatingRateBond;
using QuantLib::BinomialConvertibleEngine;
typedef boost::shared_ptr<Instrument> ConvertibleZeroCouponBondPtr;
typedef boost::shared_ptr<Instrument> ConvertibleFixedCouponBondPtr;
typedef boost::shared_ptr<Instrument> ConvertibleFloatingRateBondPtr;
typedef boost::shared_ptr<PricingEngine> BinomialConvertibleEnginePtr;
%}

%rename(ConvertibleZeroCouponBond) ConvertibleZeroCouponBondPtr;
class ConvertibleZeroCouponBondPtr : public BondPtr {
  public:
    %extend {
        ConvertibleZeroCouponBondPtr(
              const boost::shared_ptr<Exercise>& exercise,
              Real conversionRatio,
              const std::vector<boost::shared_ptr<Dividend> >& dividends,
              const std::vector<boost::shared_ptr<Callability> >& callability,
              const Handle<Quote>& creditSpread,
              const Date& issueDate,
              Integer settlementDays,
              const DayCounter& dayCounter,
              const Schedule& schedule,
              Real redemption = 100.0) {
            return new ConvertibleZeroCouponBondPtr(
                     new ConvertibleZeroCouponBond(exercise, conversionRatio,
                                                   dividends, callability,
                                                   creditSpread,
                                                   issueDate, settlementDays,
                                                   dayCounter, schedule,
                                                   redemption));
        }
    }
};


%rename(ConvertibleFixedCouponBond) ConvertibleFixedCouponBondPtr;
class ConvertibleFixedCouponBondPtr : public BondPtr {
  public:
    %extend {
        ConvertibleFixedCouponBondPtr(
              const boost::shared_ptr<Exercise>& exercise,
              Real conversionRatio,
              const std::vector<boost::shared_ptr<Dividend> >& dividends,
              const std::vector<boost::shared_ptr<Callability> >& callability,
              const Handle<Quote>& creditSpread,
              const Date& issueDate,
              Integer settlementDays,
              const std::vector<Rate>& coupons,
              const DayCounter& dayCounter,
              const Schedule& schedule,
              Real redemption = 100.0) {
            return new ConvertibleFixedCouponBondPtr(
                    new ConvertibleFixedCouponBond(exercise, conversionRatio,
                                                   dividends, callability,
                                                   creditSpread,
                                                   issueDate, settlementDays,
                                                   coupons, dayCounter,
                                                   schedule, redemption));
        }
    }
};


%rename(ConvertibleFloatingRateBond) ConvertibleFloatingRateBondPtr;
class ConvertibleFloatingRateBondPtr : public BondPtr {
  public:
    %extend {
        ConvertibleFloatingRateBondPtr(
              const boost::shared_ptr<Exercise>& exercise,
              Real conversionRatio,
              const std::vector<boost::shared_ptr<Dividend> >& dividends,
              const std::vector<boost::shared_ptr<Callability> >& callability,
              const Handle<Quote>& creditSpread,
              const Date& issueDate,
              Integer settlementDays,
              const IborIndexPtr& index,
              Integer fixingDays,
              const std::vector<Spread>& spreads,
              const DayCounter& dayCounter,
              const Schedule& schedule,
              Real redemption = 100.0) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new ConvertibleFloatingRateBondPtr(
                   new ConvertibleFloatingRateBond(exercise, conversionRatio,
                                                   dividends, callability,
                                                   creditSpread,
                                                   issueDate, settlementDays,
                                                   libor, fixingDays, spreads,
                                                   dayCounter, schedule,
                                                   redemption));
        }
    }
};



%rename(BinomialConvertibleEngine) BinomialConvertibleEnginePtr;
class BinomialConvertibleEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        BinomialConvertibleEnginePtr(
                             const GeneralizedBlackScholesProcessPtr& process,
                             const std::string& type,
                             Size steps) {
            boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess =
                 boost::dynamic_pointer_cast<GeneralizedBlackScholesProcess>(
                                                                     process);
            QL_REQUIRE(bsProcess, "Black-Scholes process required");
            std::string s = boost::algorithm::to_lower_copy(type);
            if (s == "crr" || s == "coxrossrubinstein")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<CoxRossRubinstein>(
                                                            bsProcess,steps));
            else if (s == "jr" || s == "jarrowrudd")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<JarrowRudd>(
                                                            bsProcess,steps));
            else if (s == "eqp")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<AdditiveEQPBinomialTree>(
                                                            bsProcess,steps));
            else if (s == "trigeorgis")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<Trigeorgis>(
                                                            bsProcess,steps));
            else if (s == "tian")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<Tian>(bsProcess,steps));
            else if (s == "lr" || s == "leisenreimer")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<LeisenReimer>(
                                                            bsProcess,steps));
            else if (s == "j4" || s == "joshi4")
                return new BinomialConvertibleEnginePtr(
                    new BinomialConvertibleEngine<Joshi4>(bsProcess,steps));
            else
                QL_FAIL("unknown binomial engine type: "+s);
        }
    }
};


#endif
