/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2010, 2011 Lluis Pujol Bajador

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


#ifndef quantlib_cash_flows_i
#define quantlib_cash_flows_i

%include date.i
%include types.i
%include calendars.i
%include daycounters.i
%include indexes.i
%include termstructures.i
%include scheduler.i
%include vectors.i
%include volatilities.i

%{
using QuantLib::CashFlow;
%}

%ignore CashFlow;
class CashFlow {
  public:
    Real amount() const;
    Date date() const;
};

%template(CashFlow) boost::shared_ptr<CashFlow>;
IsObservable(boost::shared_ptr<CashFlow>);

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<CashFlow> )
#endif
%template(Leg) std::vector<boost::shared_ptr<CashFlow> >;
typedef std::vector<boost::shared_ptr<CashFlow> > Leg;


// implementations

%{
using QuantLib::SimpleCashFlow;
using QuantLib::Redemption;
using QuantLib::AmortizingPayment;
using QuantLib::Coupon;
using QuantLib::FixedRateCoupon;
using QuantLib::IborCoupon;
using QuantLib::Leg;
using QuantLib::FloatingRateCoupon;

typedef boost::shared_ptr<CashFlow> SimpleCashFlowPtr;
typedef boost::shared_ptr<CashFlow> RedemptionPtr;
typedef boost::shared_ptr<CashFlow> AmortizingPaymentPtr;
typedef boost::shared_ptr<CashFlow> CouponPtr;
typedef boost::shared_ptr<CashFlow> IborCouponPtr;
typedef boost::shared_ptr<CashFlow> FixedRateCouponPtr;
typedef boost::shared_ptr<CashFlow> FloatingRateCouponPtr;
%}

%rename(SimpleCashFlow) SimpleCashFlowPtr;
class SimpleCashFlowPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        SimpleCashFlowPtr(Real amount, const Date& date) {
            return new SimpleCashFlowPtr(new SimpleCashFlow(amount,date));
        }
    }
};

%rename(Redemption) RedemptionPtr;
class RedemptionPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        RedemptionPtr(Real amount, const Date& date) {
            return new RedemptionPtr(new Redemption(amount,date));
        }
    }
};

%rename(AmortizingPayment) AmortizingPaymentPtr;
class AmortizingPaymentPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        AmortizingPaymentPtr(Real amount, const Date& date) {
            return new AmortizingPaymentPtr(new AmortizingPayment(amount,date));
        }
    }
};

%rename(Coupon) CouponPtr;
class CouponPtr : public boost::shared_ptr<CashFlow> {
  private:
    CouponPtr();
  public:
    %extend {
        Real nominal() {
            return boost::dynamic_pointer_cast<Coupon>(*self)->nominal();
        }
        Date accrualStartDate() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->accrualStartDate();
        }
        Date accrualEndDate() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->accrualEndDate();
        }
        Date referencePeriodStart() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->referencePeriodStart();
        }
        Date referencePeriodEnd() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->referencePeriodEnd();
        }
        Date exCouponDate() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->exCouponDate();
        }
        Real rate() {
            return boost::dynamic_pointer_cast<Coupon>(*self)->rate();
        }
        Time accrualPeriod() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->accrualPeriod();
        }
        BigInteger accrualDays() {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->accrualDays();
        }
        DayCounter dayCounter() const {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->dayCounter();
        }
        Real accruedAmount(const Date& date) {
            return boost::dynamic_pointer_cast<Coupon>(*self)
                ->accruedAmount(date);
        }
    }
};

%inline %{
    CouponPtr as_coupon(const boost::shared_ptr<CashFlow>& cf) {
        return boost::dynamic_pointer_cast<Coupon>(cf);
    }
%}


%rename(FixedRateCoupon) FixedRateCouponPtr;
class FixedRateCouponPtr : public CouponPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") FixedRateCouponPtr;
    #endif
  public:
    %extend {
        FixedRateCouponPtr(const Date& paymentDate, Real nominal,
                           Rate rate, const DayCounter& dayCounter,
                           const Date& startDate, const Date& endDate,
                           const Date& refPeriodStart = Date(),
                           const Date& refPeriodEnd = Date(),
                           const Date& exCouponDate = Date()) {
            return new FixedRateCouponPtr(
                new FixedRateCoupon(paymentDate, nominal, rate,
                                    dayCounter, startDate, endDate,
                                    refPeriodStart, refPeriodEnd,
                                    exCouponDate));
        }
        InterestRate interestRate() {
            return boost::dynamic_pointer_cast<FixedRateCoupon>(*self)
                ->interestRate();
        }
    }
};

%inline %{
    FixedRateCouponPtr as_fixed_rate_coupon(
                                      const boost::shared_ptr<CashFlow>& cf) {
        return boost::dynamic_pointer_cast<FixedRateCoupon>(cf);
    }
%}


%{
using QuantLib::FloatingRateCouponPricer;
%}

%ignore FloatingRateCouponPricer;
class FloatingRateCouponPricer {};

%template(FloatingRateCouponPricer) boost::shared_ptr<FloatingRateCouponPricer>;

void setCouponPricer(const Leg&,
                     const boost::shared_ptr<FloatingRateCouponPricer>&);

%rename(FloatingRateCoupon) FloatingRateCouponPtr;
class FloatingRateCouponPtr : public CouponPtr {
  private:
    FloatingRateCouponPtr();
  public:
    %extend {
        Date fixingDate() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->fixingDate();
        }
        Integer fixingDays() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->fixingDays();
        }
        bool isInArrears() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->isInArrears();
        }
        Real gearing() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->gearing();
        }
        Rate spread() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->spread();
        }
        Rate indexFixing() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->indexFixing();
        }
        Rate adjustedFixing() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->adjustedFixing();
        }
        Rate convexityAdjustment() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->convexityAdjustment();
        }
        Real price(const Handle<YieldTermStructure>& discountCurve) {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->price(discountCurve);
        }
        InterestRateIndexPtr index() const {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->index();
        }
        void setPricer(const boost::shared_ptr<FloatingRateCouponPricer>& p) {
            boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->setPricer(p);
        }
    }
};

%inline %{
    FloatingRateCouponPtr as_floating_rate_coupon(
                                      const boost::shared_ptr<CashFlow>& cf) {
        return boost::dynamic_pointer_cast<FloatingRateCoupon>(cf);
    }
%}


%{
using QuantLib::CappedFlooredCoupon;
typedef boost::shared_ptr<CashFlow> CappedFlooredCouponPtr;
%}

%rename(CappedFlooredCoupon) CappedFlooredCouponPtr;
class CappedFlooredCouponPtr : public FloatingRateCouponPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") CappedFlooredCouponPtr;
    #endif
  public:
    %extend {
        CappedFlooredCouponPtr(const FloatingRateCouponPtr& underlying,
                               Rate cap = Null<Rate>(),
                               Rate floor = Null<Rate>()) {
            boost::shared_ptr<FloatingRateCoupon> u =
                boost::dynamic_pointer_cast<FloatingRateCoupon>(underlying);
            return new CappedFlooredCouponPtr(
                new CappedFlooredCoupon(u,cap,floor));
        }
        Rate cap() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->cap();
        }
        Rate floor() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->floor();
        }
        Rate effectiveCap() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->effectiveCap();
        }
        Rate effectiveFloor() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->effectiveFloor();
        }
        bool isCapped() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->isCapped();
        }
        bool isFloored() {
           return boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->isFloored();
        }
        void setPricer(const boost::shared_ptr<FloatingRateCouponPricer>& p) {
            boost::dynamic_pointer_cast<CappedFlooredCoupon>(*self)
                ->setPricer(p);
        }
    }
};


// specialized floating-rate coupons

%rename(IborCoupon) IborCouponPtr;
class IborCouponPtr : public FloatingRateCouponPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") IborCouponPtr;
    #endif
  public:
    %extend {
        IborCouponPtr(const Date& paymentDate, Real nominal,
                      const Date& startDate, const Date& endDate,
                      Integer fixingDays, InterestRateIndexPtr& index,
                      Real gearing = 1.0, Spread spread = 0.0,
                      const Date& refPeriodStart = Date(),
                      const Date& refPeriodEnd = Date(),
                      const DayCounter& dayCounter = DayCounter()) {
            const boost::shared_ptr<IborIndex> iri =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new IborCouponPtr(
                new IborCoupon(paymentDate, nominal, startDate, endDate,
                               fixingDays, iri, gearing, spread,
                               refPeriodStart, refPeriodEnd, dayCounter));
        }
    }
};


%{
using QuantLib::IborCouponPricer;
using QuantLib::BlackIborCouponPricer;
typedef boost::shared_ptr<FloatingRateCouponPricer> IborCouponPricerPtr;
typedef boost::shared_ptr<FloatingRateCouponPricer> BlackIborCouponPricerPtr;
%}

%rename(IborCouponPricer) IborCouponPricerPtr;
class IborCouponPricerPtr : public boost::shared_ptr<FloatingRateCouponPricer> {
  private:
    IborCouponPricerPtr();
  public:
    %extend {
        Handle<OptionletVolatilityStructure> capletVolatility() {
            return boost::dynamic_pointer_cast<IborCouponPricer>(*self)
                ->capletVolatility();
        }
        void setCapletVolatility(const Handle<OptionletVolatilityStructure>& v =
                                     Handle<OptionletVolatilityStructure>()) {
            boost::dynamic_pointer_cast<IborCouponPricer>(*self)
                ->setCapletVolatility(v);
        }
    }
};

%rename(BlackIborCouponPricer) BlackIborCouponPricerPtr;
class BlackIborCouponPricerPtr : public IborCouponPricerPtr {
  public:
    %extend {
        BlackIborCouponPricerPtr(const Handle<OptionletVolatilityStructure>& v =
                                     Handle<OptionletVolatilityStructure>()) {
            return new BlackIborCouponPricerPtr(new BlackIborCouponPricer(v));
        }
    }
};

%{
using QuantLib::CmsCoupon;
using QuantLib::CappedFlooredCmsCoupon;
typedef boost::shared_ptr<CashFlow> CmsCouponPtr;
typedef boost::shared_ptr<CashFlow> CappedFlooredCmsCouponPtr;
%}

%rename(CmsCoupon) CmsCouponPtr;
class CmsCouponPtr : public FloatingRateCouponPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") CmsCouponPtr;
    #endif
  public:
    %extend {
        CmsCouponPtr(const Date& paymentDate, Real nominal,
                     const Date& startDate, const Date& endDate,
                     Integer fixingDays, const SwapIndexPtr& index,
                     Real gearing = 1.0, Spread spread = 0.0,
                     const Date& refPeriodStart = Date(),
                     const Date& refPeriodEnd = Date(),
                     const DayCounter& dayCounter = DayCounter(),
                     bool isInArrears = false) {
            const boost::shared_ptr<SwapIndex> swi =
                boost::dynamic_pointer_cast<SwapIndex>(index);
            return new CmsCouponPtr(
                new CmsCoupon(paymentDate,nominal,startDate,endDate,
                              fixingDays,swi,gearing,spread,
                              refPeriodStart,refPeriodEnd,
                              dayCounter,isInArrears));
        }
    }
};

%{
using QuantLib::CmsCouponPricer;
using QuantLib::AnalyticHaganPricer;
using QuantLib::NumericHaganPricer;
using QuantLib::GFunctionFactory;
typedef boost::shared_ptr<FloatingRateCouponPricer> CmsCouponPricerPtr;
typedef boost::shared_ptr<FloatingRateCouponPricer> AnalyticHaganPricerPtr;
typedef boost::shared_ptr<FloatingRateCouponPricer> NumericHaganPricerPtr;
%}

%rename(CmsCouponPricer) CmsCouponPricerPtr;
class CmsCouponPricerPtr : public boost::shared_ptr<FloatingRateCouponPricer> {
  private:
    CmsCouponPricerPtr();
  public:
    %extend {
        Handle<SwaptionVolatilityStructure> swaptionVolatility() {
            return boost::dynamic_pointer_cast<CmsCouponPricer>(*self)
                ->swaptionVolatility();
        }
        void setSwaptionVolatility(
                                const Handle<SwaptionVolatilityStructure>& v =
                                      Handle<SwaptionVolatilityStructure>()) {
            boost::dynamic_pointer_cast<CmsCouponPricer>(*self)
                ->setSwaptionVolatility(v);
        }
    }
};

class GFunctionFactory {
  private:
    GFunctionFactory();
  public:
    enum YieldCurveModel { Standard,
                           ExactYield,
                           ParallelShifts,
                           NonParallelShifts };
};

%rename(AnalyticHaganPricer) AnalyticHaganPricerPtr;
class AnalyticHaganPricerPtr : public CmsCouponPricerPtr {
  public:
    %extend {
        AnalyticHaganPricerPtr(const Handle<SwaptionVolatilityStructure>& v,
                               GFunctionFactory::YieldCurveModel model,
                               const Handle<Quote>& meanReversion) {
            return new AnalyticHaganPricerPtr(
                            new AnalyticHaganPricer(v, model, meanReversion));
        }
    }
};

%rename(NumericHaganPricer) NumericHaganPricerPtr;
class NumericHaganPricerPtr : public CmsCouponPricerPtr {
  public:
    %extend {
        NumericHaganPricerPtr(const Handle<SwaptionVolatilityStructure>& v,
                              GFunctionFactory::YieldCurveModel model,
                              const Handle<Quote>& meanReversion,
                              Rate lowerLimit = 0.0,
                              Rate upperLimit = 1.0,
                              Real precision = 1.0e-6) {
             return new NumericHaganPricerPtr(
                 new NumericHaganPricer(v, model, meanReversion,
                                        lowerLimit, upperLimit, precision));
        }
    }
};

%rename(CappedFlooredCmsCoupon) CappedFlooredCmsCouponPtr;
class CappedFlooredCmsCouponPtr: public CappedFlooredCouponPtr {
    #if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
    %feature("kwargs") CappedFlooredCouponPtr;
    #endif
  public:
    %extend {
        CappedFlooredCmsCouponPtr(
                  const Date& paymentDate, Real nominal,
                  const Date& startDate, const Date& endDate,
                  Natural fixingDays, const SwapIndexPtr& index,
                  Real gearing = 1.0, Spread spread = 0.0,
                  const Rate cap = Null<Rate>(),
                  const Rate floor = Null<Rate>(),
                  const Date& refPeriodStart = Date(),
                  const Date& refPeriodEnd = Date(),
                  const DayCounter& dayCounter = DayCounter(),
                  bool isInArrears = false) {
            const boost::shared_ptr<SwapIndex> swi =
                boost::dynamic_pointer_cast<SwapIndex>(index);
            return new CappedFlooredCmsCouponPtr(
                new CappedFlooredCmsCoupon(
                      paymentDate, nominal, startDate, endDate, fixingDays,
                      swi, gearing, spread, cap, floor, refPeriodStart,
                      refPeriodEnd, dayCounter, isInArrears));
        }
    }
};


// cash flow vector builders

%{
Leg _FixedRateLeg(const Schedule& schedule,
                  const DayCounter& dayCount,
                  const std::vector<Real>& nominals,
                  const std::vector<Rate>& couponRates,
                  BusinessDayConvention paymentAdjustment = Following,
                  const DayCounter& firstPeriodDayCount = DayCounter()) {
    return QuantLib::FixedRateLeg(schedule)
        .withNotionals(nominals)
        .withCouponRates(couponRates,dayCount)
        .withPaymentAdjustment(paymentAdjustment)
        .withFirstPeriodDayCounter(firstPeriodDayCount);
}
%}
#if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
%feature("kwargs") _FixedRateLeg;
#endif
%rename(FixedRateLeg) _FixedRateLeg;
Leg _FixedRateLeg(const Schedule& schedule,
                  const DayCounter& dayCount,
                  const std::vector<Real>& nominals,
                  const std::vector<Rate>& couponRates,
                  BusinessDayConvention paymentAdjustment = Following,
                  const DayCounter& firstPeriodDayCount = DayCounter());

%{
Leg _IborLeg(const std::vector<Real>& nominals,
             const Schedule& schedule,
             const boost::shared_ptr<Index>& index,
             const DayCounter& paymentDayCounter = DayCounter(),
             const BusinessDayConvention paymentConvention = Following,
             const std::vector<Natural>& fixingDays = std::vector<Natural>(),
             const std::vector<Real>& gearings = std::vector<Real>(),
             const std::vector<Spread>& spreads = std::vector<Spread>(),
             const std::vector<Rate>& caps = std::vector<Rate>(),
             const std::vector<Rate>& floors = std::vector<Rate>(),
             bool isInArrears = false) {
    boost::shared_ptr<IborIndex> libor =
        boost::dynamic_pointer_cast<IborIndex>(index);
    return QuantLib::IborLeg(schedule, libor)
        .withNotionals(nominals)
        .withPaymentDayCounter(paymentDayCounter)
        .withPaymentAdjustment(paymentConvention)
        .withFixingDays(fixingDays)
        .withGearings(gearings)
        .withSpreads(spreads)
        .withCaps(caps)
        .withFloors(floors)
        .inArrears(isInArrears);
}
%}
#if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
%feature("kwargs") _IborLeg;
#endif
%rename(IborLeg) _IborLeg;
Leg _IborLeg(const std::vector<Real>& nominals,
             const Schedule& schedule,
             const IborIndexPtr& index,
             const DayCounter& paymentDayCounter = DayCounter(),
             const BusinessDayConvention paymentConvention = Following,
             const std::vector<Natural>& fixingDays = std::vector<Natural>(),
             const std::vector<Real>& gearings = std::vector<Real>(),
             const std::vector<Spread>& spreads = std::vector<Spread>(),
             const std::vector<Rate>& caps = std::vector<Rate>(),
             const std::vector<Rate>& floors = std::vector<Rate>(),
             bool isInArrears = false);

%{
Leg _CmsLeg(const std::vector<Real>& nominals,
            const Schedule& schedule,
            const boost::shared_ptr<Index>& index,
            const DayCounter& paymentDayCounter = DayCounter(),
            const BusinessDayConvention paymentConvention = Following,
            const std::vector<Natural>& fixingDays = std::vector<Natural>(),
            const std::vector<Real>& gearings = std::vector<Real>(),
            const std::vector<Spread>& spreads = std::vector<Spread>(),
            const std::vector<Rate>& caps = std::vector<Rate>(),
            const std::vector<Rate>& floors = std::vector<Rate>(),
            bool isInArrears = false) {
    boost::shared_ptr<SwapIndex> swapIndex =
        boost::dynamic_pointer_cast<SwapIndex>(index);
    return QuantLib::CmsLeg(schedule, swapIndex)
        .withNotionals(nominals)
        .withPaymentDayCounter(paymentDayCounter)
        .withPaymentAdjustment(paymentConvention)
        .withFixingDays(fixingDays)
        .withGearings(gearings)
        .withSpreads(spreads)
        .withCaps(caps)
        .withFloors(floors)
        .inArrears(isInArrears);
}
%}
#if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
%feature("kwargs") _CmsLeg;
#endif
%rename(CmsLeg) _CmsLeg;
Leg _CmsLeg(const std::vector<Real>& nominals,
            const Schedule& schedule,
            const SwapIndexPtr& index,
            const DayCounter& paymentDayCounter = DayCounter(),
            const BusinessDayConvention paymentConvention = Following,
            const std::vector<Natural>& fixingDays = std::vector<Natural>(),
            const std::vector<Real>& gearings = std::vector<Real>(),
            const std::vector<Spread>& spreads = std::vector<Spread>(),
            const std::vector<Rate>& caps = std::vector<Rate>(),
            const std::vector<Rate>& floors = std::vector<Rate>(),
            bool isInArrears = false);

%{
Leg _CmsZeroLeg(const std::vector<Real>& nominals,
                const Schedule& schedule,
                const boost::shared_ptr<Index>& index,
                const DayCounter& paymentDayCounter = DayCounter(),
                const BusinessDayConvention paymentConvention = Following,
                const std::vector<Natural>& fixingDays = std::vector<Natural>(),
                const std::vector<Real>& gearings = std::vector<Real>(),
                const std::vector<Spread>& spreads = std::vector<Spread>(),
                const std::vector<Rate>& caps = std::vector<Rate>(),
                const std::vector<Rate>& floors = std::vector<Rate>()) {
    boost::shared_ptr<SwapIndex> swapIndex =
        boost::dynamic_pointer_cast<SwapIndex>(index);
    return QuantLib::CmsLeg(schedule, swapIndex)
        .withNotionals(nominals)
        .withPaymentDayCounter(paymentDayCounter)
        .withPaymentAdjustment(paymentConvention)
        .withFixingDays(fixingDays)
        .withGearings(gearings)
        .withSpreads(spreads)
        .withCaps(caps)
        .withFloors(floors)
        .withZeroPayments();
}
%}
#if !defined(SWIGJAVA) && !defined(SWIGCSHARP)
%feature("kwargs") _CmsZeroLeg;
#endif
%rename(CmsZeroLeg) _CmsZeroLeg;
Leg _CmsZeroLeg(const std::vector<Real>& nominals,
                const Schedule& schedule,
                const SwapIndexPtr& index,
                const DayCounter& paymentDayCounter = DayCounter(),
                const BusinessDayConvention paymentConvention = Following,
                const std::vector<Natural>& fixingDays = std::vector<Natural>(),
                const std::vector<Real>& gearings = std::vector<Real>(),
                const std::vector<Spread>& spreads = std::vector<Spread>(),
                const std::vector<Rate>& caps = std::vector<Rate>(),
                const std::vector<Rate>& floors = std::vector<Rate>());


// cash-flow analysis

%{
using QuantLib::CashFlows;
using QuantLib::Duration;
%}

struct Duration {
    enum Type { Simple, Macaulay, Modified };
};

class CashFlows {
    #if defined(SWIGPYTHON)
    %rename("yieldRate")   yield;
    #endif
  private:
    CashFlows();
    CashFlows(const CashFlows&);
  public:
    static Date startDate(const Leg &);
    static Date maturityDate(const Leg &);
    %extend {
        static Real npv(
                   const Leg& leg,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   Spread zSpread,
                   const DayCounter &dayCounter,
                   Compounding compounding,
                   Frequency frequency,
                   bool includeSettlementDateFlows,
                   const Date& settlementDate = Date(),
                   const Date& npvDate = Date()) {
            return QuantLib::CashFlows::npv(leg, discountCurve,
                                            zSpread,
                                            dayCounter,
                                            compounding,
                                            frequency,
                                            includeSettlementDateFlows,
                                            settlementDate,
                                            npvDate);
        }
        static Real npv(
                   const Leg& leg,
                   const Handle<YieldTermStructure>& discountCurve,
                   bool includeSettlementDateFlows,
                   const Date& settlementDate = Date(),
                   const Date& npvDate = Date()) {
            return QuantLib::CashFlows::npv(leg, **discountCurve,
                                            includeSettlementDateFlows,
                                            settlementDate, npvDate);
        }
    }
    static Real npv(const Leg&,
                    const InterestRate&,
                    bool includeSettlementDateFlows,
                    Date settlementDate = Date(),
                    Date npvDate = Date());

    static Real npv(const Leg&,
                    Rate yield,
                    const DayCounter&dayCounter,
                    Compounding compounding,
                    Frequency frequency,
                    bool includeSettlementDateFlows,
                    Date settlementDate = Date(),
                    Date npvDate = Date());
    %extend {
        static Real bps(
                   const Leg& leg,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   bool includeSettlementDateFlows,
                   const Date& settlementDate = Date(),
                   const Date& npvDate = Date()) {
            return QuantLib::CashFlows::bps(leg, *discountCurve,
                                            includeSettlementDateFlows,
                                            settlementDate, npvDate);
        }
        static Real bps(
                   const Leg& leg,
                   const Handle<YieldTermStructure>& discountCurve,
                   bool includeSettlementDateFlows,
                   const Date& settlementDate = Date(),
                   const Date& npvDate = Date()) {
            return QuantLib::CashFlows::bps(leg, **discountCurve,
                                            includeSettlementDateFlows,
                                            settlementDate, npvDate);
        }
    }
    static Real bps(const Leg&,
                    const InterestRate &,
                    bool includeSettlementDateFlows,
                    Date settlementDate = Date(),
                    Date npvDate = Date());
    static Real bps(const Leg&,
                    Rate yield,
                    const DayCounter&dayCounter,
                    Compounding compounding,
                    Frequency frequency,
                    bool includeSettlementDateFlows,
                    Date settlementDate = Date(),
                    Date npvDate = Date());

    %extend {
        static Rate atmRate(
                   const Leg& leg,
                   const boost::shared_ptr<YieldTermStructure>& discountCurve,
                   bool includeSettlementDateFlows,
                   const Date& settlementDate = Date(),
                   const Date& npvDate = Date(),
                   Real npv = Null<Real>()) {
            return QuantLib::CashFlows::atmRate(leg, *discountCurve,
                                                includeSettlementDateFlows,
                                                settlementDate, npvDate,
                                                npv);
        }
    }
    static Rate yield(const Leg&,
                      Real npv,
                      const DayCounter& dayCounter,
                      Compounding compounding,
                      Frequency frequency,
                      bool includeSettlementDateFlows,
                      Date settlementDate = Date(),
                      Date npvDate = Date(),
                      Real accuracy = 1.0e-10,
                      Size maxIterations = 10000,
                      Rate guess = 0.05);
    static Time duration(const Leg&,
                         const InterestRate&,
                         Duration::Type type,
                         bool includeSettlementDateFlows,
                         Date settlementDate = Date());

    static Time duration(const Leg&,
             Rate yield,
             const DayCounter& dayCounter,
             Compounding compounding,
             Frequency frequency,
             Duration::Type type,
             bool includeSettlementDateFlows,
             Date settlementDate = Date(),
             Date npvDate = Date());

    static Real convexity(const Leg&,
                          const InterestRate&,
                          bool includeSettlementDateFlows,
                          Date settlementDate = Date(),
                          Date npvDate = Date());

    static Real convexity(const Leg&,
             Rate yield,
             const DayCounter& dayCounter,
             Compounding compounding,
             Frequency frequency,
             bool includeSettlementDateFlows,
             Date settlementDate = Date(),
             Date npvDate = Date());

    static Real basisPointValue(const Leg& leg,
             const InterestRate& yield,
             bool includeSettlementDateFlows,
             Date settlementDate = Date(),
             Date npvDate = Date());

    static Real basisPointValue(const Leg& leg,
             Rate yield,
             const DayCounter& dayCounter,
             Compounding compounding,
             Frequency frequency,
             bool includeSettlementDateFlows,
             Date settlementDate = Date(),
             Date npvDate = Date());

    static Spread zSpread(const Leg& leg,
             Real npv,
             const boost::shared_ptr<YieldTermStructure>&,
             const DayCounter& dayCounter,
             Compounding compounding,
             Frequency frequency,
             bool includeSettlementDateFlows,
             Date settlementDate = Date(),
             Date npvDate = Date(),
             Real accuracy = 1.0e-10,
             Size maxIterations = 100,
             Rate guess = 0.0);

};


#endif
