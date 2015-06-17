/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006, 2007, 2009 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2015 Riccardo Barone

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

#ifndef qla_yieldtermstructures_hpp
#define qla_yieldtermstructures_hpp

#include <qlo/termstructures.hpp>

#include <ql/time/frequency.hpp>
#include <ql/compounding.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class Calendar;
    class DayCounter;
    class Date;
    class Quote;

    template<class TS>
    class BootstrapHelper;

    typedef BootstrapHelper<YieldTermStructure> RateHelper;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {
     
    class DiscountCurve : public YieldTermStructure {
      public:
        DiscountCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::DiscountFactor>& dfs,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    class ZeroCurve : public YieldTermStructure {
      public:
        ZeroCurve(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  const std::vector<QuantLib::Date>& dates,
                  const std::vector<QuantLib::Rate>& zeroRates,
                  const QuantLib::DayCounter& dayCounter,
                  bool permanent);
    };

    class ForwardCurve : public YieldTermStructure {
      public:
        ForwardCurve(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                     const std::vector<QuantLib::Date>& dates,
                     const std::vector<QuantLib::Rate>& forwardRates,
                     const QuantLib::DayCounter& dayCounter,
                     bool permanent);
    };

    class FlatForward : public YieldTermStructure {
      public:
        FlatForward(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    QuantLib::Natural nDays,
                    const QuantLib::Calendar& calendar,
                    const QuantLib::Handle<QuantLib::Quote>& forward,
                    const QuantLib::DayCounter& dayCounter,
                    QuantLib::Compounding compounding,
                    QuantLib::Frequency frequency,
                    bool permanent);
    };

    class ForwardSpreadedTermStructure : public YieldTermStructure {
      public:
        ForwardSpreadedTermStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            bool permanent);
    };

    class ImpliedTermStructure : public YieldTermStructure {
      public:
        ImpliedTermStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Date& referenceDate,
            bool permanent);
    };

    class InterpolatedYieldCurve : public YieldTermStructure {
      public:
        // placeholders for QuantLib types of the same name.
        enum Traits { Discount,
                      ForwardRate,
                      ZeroYield
        };
        enum Interpolator { BackwardFlat,
                            ForwardFlat,
                            Linear,
                            LogLinear,
                            CubicNaturalSpline,
                            LogCubicNaturalSpline,
                            MonotonicCubicNaturalSpline,
                            MonotonicLogCubicNaturalSpline,
                            KrugerCubic,
                            KrugerLogCubic,
                            FritschButlandCubic,
                            FritschButlandLogCubic,
                            Parabolic,
                            LogParabolic,
                            MonotonicParabolic,
                            MonotonicLogParabolic
        };
        InterpolatedYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Real>& data,
            const QuantLib::Calendar& calendar,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& jumps,
            const std::vector<QuantLib::Date>& jumpDates,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool permanent);
        const std::vector<QuantLib::Time>& times() const;
        const std::vector<QuantLib::Date>& dates() const;
        const std::vector<QuantLib::Real>& data() const;
        const std::vector<QuantLib::Time>& jumpTimes() const;
        const std::vector<QuantLib::Date>& jumpDates() const;
      protected:
        InterpolatedYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool permanent);
        std::string traitsID_, interpolatorID_;
    };

	class CompositeDiscountCurve : public YieldTermStructure {
	public:
		CompositeDiscountCurve(
			const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
			const QuantLib::Handle<QuantLib::YieldTermStructure>& first,
			const QuantLib::Handle<QuantLib::YieldTermStructure>& second,
			const QuantLib::Date& joinDate,
			bool allowExtrapolatedJunction,
			bool allowExtrapolation,
			bool permanent);
	};
	
    // A pair indicating a combination of Traits / Interpolator.
    typedef std::pair<InterpolatedYieldCurve::Traits, InterpolatedYieldCurve::Interpolator> InterpolatedYieldCurvePair;

    // Stream operator to write a InterpolatedYieldCurvePair to a stream - for logging / error handling.
    std::ostream &operator<<(std::ostream &out,
                             InterpolatedYieldCurvePair tokenPair);
}

#endif
