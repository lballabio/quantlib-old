
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier

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


// implementations

%{
using QuantLib::SimpleCashFlow;
using QuantLib::FixedRateCoupon;
using QuantLib::IborCoupon;
using QuantLib::Leg;
using QuantLib::FloatingRateCoupon;

typedef boost::shared_ptr<CashFlow> SimpleCashFlowPtr;
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

%rename(FixedRateCoupon) FixedRateCouponPtr;
class FixedRateCouponPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        FixedRateCouponPtr(Real nominal, const Date& paymentDate,
                           Rate rate, const DayCounter& dayCounter,
                           const Date& startDate, const Date& endDate,
                           const Date& refPeriodStart = Date(),
                           const Date& refPeriodEnd = Date()) {
            return new FixedRateCouponPtr(
                new FixedRateCoupon(nominal, paymentDate, rate,
                                    dayCounter, startDate, endDate,
                                    refPeriodStart, refPeriodEnd));
        }
    }
};

%rename(IborCoupon) IborCouponPtr;
class IborCouponPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        IborCouponPtr(const Date& paymentDate, Real nominal,
                     const Date& startDate, const Date& endDate,
                     Integer fixingDays, InterestRateIndexPtr& index,
                     Real gearing = 1.0, Spread spread = 0.0,
                     const Date& refPeriodStart = Date(),
                     const Date& refPeriodEnd = Date(),
                     const DayCounter& dayCounter = DayCounter()) {
            const boost::shared_ptr<InterestRateIndex> iri =
                boost::dynamic_pointer_cast<InterestRateIndex>(index);
            return new IborCouponPtr(
                new IborCoupon(paymentDate, nominal, startDate, endDate,
                              fixingDays, iri, gearing, spread,
                              refPeriodStart, refPeriodEnd, dayCounter));
        }
        Rate rate() {
            return boost::dynamic_pointer_cast<IborCoupon>(*self)->rate();
        }
        Rate indexFixing() {
            return boost::dynamic_pointer_cast<IborCoupon>(*self)
                ->indexFixing();
        }
        Real nominal() {
            return boost::dynamic_pointer_cast<IborCoupon>(*self)->nominal();
        }
    }
};

%rename(FloatingRateCoupon) FloatingRateCouponPtr;
class FloatingRateCouponPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        Rate rate() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->rate();
        }
        Integer fixingDays() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->fixingDays();
        }
        Rate spread() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->spread();
        }
        Rate indexFixing() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->indexFixing();
        }
        Date fixingDate() {
            return boost::dynamic_pointer_cast<FloatingRateCoupon>(*self)
                ->fixingDate();
        }
    }
};


#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_SPECIALIZE( CashFlow, boost::shared_ptr<CashFlow> )
#endif
%template(Leg) std::vector<boost::shared_ptr<CashFlow> >;
typedef std::vector<boost::shared_ptr<CashFlow> > Leg;


// cash flow vector builders

%{
using QuantLib::FixedRateLeg;
%}

Leg FixedRateLeg(const std::vector<Real>& nominals,
                 const Schedule& schedule,
                 const std::vector<Rate>& couponRates,
                 const DayCounter& dayCount,
                 BusinessDayConvention paymentAdjustment = Following,
                 const DayCounter& firstPeriodDayCount = DayCounter());

%{
Leg IborLeg(const std::vector<Real>& nominals,
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
    return QuantLib::IborLeg(nominals, schedule, libor, paymentDayCounter,
                             paymentConvention, fixingDays, gearings,
                             spreads, caps, floors, isInArrears);
}
%}
%feature("kwargs") IborLeg;
Leg IborLeg(const std::vector<Real>& nominals,
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
Leg CmsLeg(const std::vector<Real>& nominals,
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
    return QuantLib::CmsLeg(nominals, schedule, swapIndex, paymentDayCounter,
                            paymentConvention, fixingDays, gearings,
                            spreads, caps, floors, isInArrears);
}
%}
%feature("kwargs") CmsLeg;
Leg CmsLeg(const std::vector<Real>& nominals,
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
Leg CmsZeroLeg(const std::vector<Real>& nominals,
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
    return QuantLib::CmsZeroLeg(nominals, schedule, swapIndex,
                                paymentDayCounter, paymentConvention,
                                fixingDays, gearings, spreads, caps, floors);
}
%}
%feature("kwargs") CmsZeroLeg;
Leg CmsZeroLeg(const std::vector<Real>& nominals,
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
  private:
    CashFlows();
    CashFlows(const CashFlows&);
  public:
    static Date startDate(const Leg &);
    static Date maturityDate(const Leg &);
    static Real npv(const Leg&,
                    const YieldTermStructure & discountCurve,
		    const Date& settlementDate = Date(),
                    const Date& npvDate = Date(),
		    Integer exDividendDays = 0);
    static Real npv(const Leg&,
                    const InterestRate&,
                    Date settlementDate = Date());
    static Real bps(const Leg&,
                    const YieldTermStructure & discountCurve,
		    const Date& settlementDate = Date(),
                    const Date& npvDate = Date(),
		    Integer exDividendDays = 0);
    static Real bps(const Leg&,
                    const InterestRate &,
                    Date settlementDate = Date());
    static Rate atmRate(const Leg&,
                        const YieldTermStructure &,
                        const Date& settlementDate = Date(),
                        const Date& npvDate = Date(),
                        Integer exDividendDays = 0,
                        Real npv = Null<Real>());
    static Rate irr(const Leg&,
                    Real marketPrice,
                    const DayCounter& dayCounter,
                    Compounding compounding,
                    Frequency frequency = NoFrequency,
                    Date settlementDate = Date(),
                    Real tolerance = 1.0e-10,
                    Size maxIterations = 10000,
                    Rate guess = 0.05);
    static Time duration(const Leg&,
                         const InterestRate&,
                         Duration::Type type = Duration::Modified,
                         Date settlementDate = Date());
    static Real convexity(const Leg&,
                          const InterestRate&,
                          Date settlementDate = Date());
};


#endif
