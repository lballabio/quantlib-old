/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2007, 2008, 2009 Ferdinando Ametrano

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif

#include <qlo/piecewiseyieldcurve.hpp>
#include <qlo/enumerations/factories/termstructuresfactory.hpp>

#include <ql/termstructures/yield/piecewiseyieldcurve.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>

namespace QuantLibAddin {

    // Constructor

    PiecewiseYieldCurve::PiecewiseYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& qlrhs,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& jumps,
            const std::vector<QuantLib::Date>& jumpDates,
            QuantLib::Real accuracy,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool permanent)
    : YieldTermStructure(properties, permanent)
    {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::YieldTermStructure> >()(traitsID,
                                              interpolatorID,
                                              nDays,
                                              calendar,
                                              qlrhs,
                                              dayCounter,
                                              jumps,
                                              jumpDates,
                                              accuracy);
    }

    // Before implementing the member functions it is necessary to provide some logic to wrap
    // the underlying QuantLib template class PiecewiseYieldCurve<Traits, Interpolator>.
    // This logic is placed in namespace Call.

    // TODO 1) This code overlaps somewhat with logic in the Enumeration Registry - consolidate?
    //      2) Generalize this functionality to expose any template class to the Addin interface

    namespace Call {

    // A nontemplate abstract base class to hold wrappers for member functions of
    // PiecewiseYieldCurve<Traits, Interpolator>.  A template subclass allows for
    // one concrete instantiation of each combination of Traits / Interpolator.
    
    class CallerBase {
    public:
        virtual const std::vector<QuantLib::Time>& times(const QuantLib::Extrapolator *extrapolator) const = 0;
        virtual const std::vector<QuantLib::Date>& dates(const QuantLib::Extrapolator *extrapolator) const = 0;
        virtual const std::vector<QuantLib::Real>& data(const QuantLib::Extrapolator *extrapolator) const = 0;
        //virtual const std::vector<QuantLib::Real>& improvements(const QuantLib::Extrapolator *extrapolator) const = 0;
        //virtual QuantLib::Size iterations(const QuantLib::Extrapolator *extrapolator) const = 0;

        virtual const std::vector<QuantLib::Time>& jumpTimes(const QuantLib::Extrapolator *extrapolator) const = 0;
        virtual const std::vector<QuantLib::Date>& jumpDates(const QuantLib::Extrapolator *extrapolator) const = 0;

        virtual ~CallerBase() {}
    };

    // Concrete derived class to wrap member functions of PiecewiseYieldCurve<Traits, Interpolator>.
    // Given a pointer to QuantLib::Extrapolator, this class downcasts to
    // PiecewiseYieldCurve<Traits, Interpolator>* and calls the given member function.

    template <class Traits, class Interpolator>
    class Caller : public CallerBase {

        typedef QuantLib::PiecewiseYieldCurve<Traits, Interpolator> CurveClass;

        const CurveClass *get(const QuantLib::Extrapolator *extrapolator) const {

            const CurveClass *ret = dynamic_cast<const CurveClass*>(extrapolator);
            OH_REQUIRE(ret, "Unable to convert from type " << typeid(extrapolator).name()
                << " to type " << typeid(CurveClass).name());
            return ret;
        }

        const std::vector<QuantLib::Time>& times(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->times();
        }

        const std::vector<QuantLib::Date>& dates(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->dates();
        }

        const std::vector<QuantLib::Real>& data(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->data();
        }

        //const std::vector<QuantLib::Real>& improvements(const QuantLib::Extrapolator *extrapolator) const {
        //    return get(extrapolator)->improvements();
        //}

        //QuantLib::Size iterations(const QuantLib::Extrapolator *extrapolator) const {
        //    return get(extrapolator)->iterations();
        //}

        const std::vector<QuantLib::Time>& jumpTimes(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->jumpTimes();
        }

        const std::vector<QuantLib::Date>& jumpDates(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->jumpDates();
        }

    };

    // Class CallerFactory stores a map of pointers to Caller objects

    class CallerFactory {

        // CallerMap - Holds a pointer to Caller for each combination of Traits / Interpolator.
        typedef std::map<InterpolatedYieldCurvePair, CallerBase*> CallerMap;
        CallerMap callerMap_;

        // Add an entry to the caller map.
        template <class Traits, class Interpolator>
        void init(InterpolatedYieldCurvePair tokenPair) {
            callerMap_[tokenPair] = new Caller<Traits, Interpolator>;
        }

    public:

        // Constructor - populate the CallerMap.
        CallerFactory() {

            // Discount
            init<QuantLib::Discount, QuantLib::BackwardFlat>(   InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::BackwardFlat));
            init<QuantLib::Discount, QuantLib::ForwardFlat>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::ForwardFlat));
            init<QuantLib::Discount, QuantLib::Linear>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::Linear));
            init<QuantLib::Discount, QuantLib::LogLinear>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::LogLinear));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::CubicNaturalSpline));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::LogCubicNaturalSpline));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::MonotonicCubicNaturalSpline));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::MonotonicLogCubicNaturalSpline));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::KrugerCubic));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::KrugerLogCubic));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::FritschButlandCubic));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::FritschButlandLogCubic));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::Parabolic));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::LogParabolic));
            init<QuantLib::Discount, QuantLib::Cubic>(          InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::MonotonicParabolic));
            init<QuantLib::Discount, QuantLib::LogCubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::Discount, InterpolatedYieldCurve::MonotonicLogParabolic));

            // ForwardRate
            init<QuantLib::ForwardRate, QuantLib::BackwardFlat>(InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::BackwardFlat));
            init<QuantLib::ForwardRate, QuantLib::ForwardFlat>( InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::ForwardFlat));
            init<QuantLib::ForwardRate, QuantLib::Linear>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::Linear));
            init<QuantLib::ForwardRate, QuantLib::LogLinear>(   InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::LogLinear));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::CubicNaturalSpline));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::LogCubicNaturalSpline));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::MonotonicCubicNaturalSpline));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::MonotonicLogCubicNaturalSpline));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::KrugerCubic));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::KrugerLogCubic));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::FritschButlandCubic));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::FritschButlandLogCubic));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::Parabolic));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::LogParabolic));
            init<QuantLib::ForwardRate, QuantLib::Cubic>(       InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::MonotonicParabolic));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(    InterpolatedYieldCurvePair(InterpolatedYieldCurve::ForwardRate, InterpolatedYieldCurve::MonotonicLogParabolic));

            // ZeroYield
            init<QuantLib::ZeroYield, QuantLib::BackwardFlat>(  InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::BackwardFlat));
            init<QuantLib::ZeroYield, QuantLib::ForwardFlat>(   InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::ForwardFlat));
            init<QuantLib::ZeroYield, QuantLib::Linear>(        InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::Linear));
            init<QuantLib::ZeroYield, QuantLib::LogLinear>(     InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::LogLinear));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::CubicNaturalSpline));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::LogCubicNaturalSpline));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::MonotonicCubicNaturalSpline));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::MonotonicLogCubicNaturalSpline));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::KrugerCubic));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::KrugerLogCubic));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::FritschButlandCubic));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::FritschButlandLogCubic));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::Parabolic));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::LogParabolic));
            init<QuantLib::ZeroYield, QuantLib::Cubic>(         InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::MonotonicParabolic));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(      InterpolatedYieldCurvePair(InterpolatedYieldCurve::ZeroYield, InterpolatedYieldCurve::MonotonicLogParabolic));

        }

        // Destructor - deallocate the CallerMap.
        ~CallerFactory() {
            for (CallerMap::const_iterator i = callerMap_.begin(); i != callerMap_.end(); ++i)
                delete i->second;
        }

        // Retrieve the Caller pointer corresponding to a given InterpolatedYieldCurvePair
        const CallerBase *getCaller(InterpolatedYieldCurvePair tokenPair) const {
            CallerMap::const_iterator i = callerMap_.find(tokenPair);
            OH_REQUIRE(i!=callerMap_.end(), "Unable to retrieve caller for type " << tokenPair);
            return i->second;
        }

    };

    // Basic Singleton behavior for the CallerFactory

    const CallerFactory &callerFactory() {
        static CallerFactory callerFactory_;
        return callerFactory_;
    }

    } // namespace Call

    // QuantLibAddin wrappers for member functions of QuantLib class
    // PiecewiseYieldCurve<Traits, Interpolator>. Invocation of the member function is
    // passed off to the CallerFactory which hides the details of the template class.

#define CALL(FUNC) \
Call::callerFactory().getCaller(InterpolatedYieldCurvePair(traits, interpolator))->FUNC(libraryObject_.get())

    const std::vector<QuantLib::Time>& PiecewiseYieldCurve::times(
        InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
        return CALL(times);
    }

    const std::vector<QuantLib::Date>& PiecewiseYieldCurve::dates(
        InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
        return CALL(dates);
    }

    const std::vector<QuantLib::Real>& PiecewiseYieldCurve::data(
        InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
        return CALL(data);
    }

    //const std::vector<QuantLib::Real>& PiecewiseYieldCurve::improvements(
    //    InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
    //    return CALL(improvements);
    //}

    //QuantLib::Size PiecewiseYieldCurve::iterations(
    //    InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
    //    return CALL(iterations);
    //}

    const std::vector<QuantLib::Time>& PiecewiseYieldCurve::jumpTimes(
        InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
        return CALL(jumpTimes);
    }

    const std::vector<QuantLib::Date>& PiecewiseYieldCurve::jumpDates(
        InterpolatedYieldCurve::Traits traits, InterpolatedYieldCurve::Interpolator interpolator) const {
        return CALL(jumpDates);
    }

}
