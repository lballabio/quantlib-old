/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2007 Ferdinando Ametrano

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
#include <qlo/Enumerations/Factories/termstructuresfactory.hpp>
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
            const QuantLib::Handle<QuantLib::Quote>& turnOfYearEffect,
            QuantLib::Real accuracy,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool permanent) : YieldTermStructure(properties, permanent)
    {
        //QuantLib::CubicSpline naturalCubic(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    false);

        //QuantLib::CubicSpline cubic1(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::FirstDerivative, 0.0,
        //    false);

        //QuantLib::CubicSpline monotoneCubic(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::FirstDerivative, 0.0,
        //    true);

        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::YieldTermStructure> >()(traitsID,
                                              interpolatorID,
                                              nDays,
                                              calendar,
                                              qlrhs,
                                              dayCounter,
                                              turnOfYearEffect,
                                              accuracy);

        //libraryObject_ = boost::shared_ptr<QuantLib::YieldTermStructure>(new
        //    QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
        //                                  QuantLib::CubicSpline>(nDays,
        //                                                   calendar,
        //                                                   rateHelpersQL,
        //                                                   dayCounter,
        //                                                   accuracy,
        //                                                   monotoneCubic));

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

        virtual const std::vector<QuantLib::Time>& times(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->times();
        }

        virtual const std::vector<QuantLib::Date>& dates(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->dates();
        }

        virtual const std::vector<QuantLib::Real>& data(const QuantLib::Extrapolator *extrapolator) const {
            return get(extrapolator)->data();
        }

        //virtual const std::vector<QuantLib::Real>& improvements(const QuantLib::Extrapolator *extrapolator) const {
        //    return get(extrapolator)->improvements();
        //}

        //virtual QuantLib::Size iterations(const QuantLib::Extrapolator *extrapolator) const {
        //    return get(extrapolator)->iterations();
        //}

    };

    // "TokenPair" - A pair of Tokens indicating a combination of Traits / Interpolator.
    typedef std::pair<Token::Traits, Token::Interpolator> TokenPair;

    // Stream operator to write a TokenPair to a stream - for logging / error handling.
    std::ostream &operator<<(std::ostream &out, TokenPair tokenPair) {

        out << "PiecewiseYieldCurve<";

        switch (tokenPair.first) {
            case Token::Discount:
                out << "Discount, ";
                break;
            case Token::ForwardRate:
                out << "ForwardRate, ";
                break;
            case Token::ZeroYield:
                out << "ZeroYield, ";
                break;
            default:
                OH_FAIL("Unknown value for enumeration QuantLibAddin::Token::Traits");
        }

        switch (tokenPair.second) {
            case Token::BackwardFlat:
                out << "BackwardFlat>";
                break;
            case Token::CubicSpline:
                out << "CubicSpline>";
                break;
            case Token::ForwardFlat:
                out << "ForwardFlat>";
                break;
            case Token::Linear:
                out << "Linear>";
                break;
            case Token::LogCubic:
                out << "LogCubic>";
                break;
            case Token::LogLinear:
                out << "LogLinear>";
                break;
            default:
                OH_FAIL("Unknown value for enumeration QuantLibAddin::Token::Interpolator");
        }

        return out;
    }

    // Class CallerFactory stores a map of pointers to Caller objects

    class CallerFactory {

        // CallerMap - Holds a pointer to Caller for each combination of Traits / Interpolator.
        typedef std::map<TokenPair, CallerBase*> CallerMap;
        CallerMap callerMap_;

        // Add an entry to the caller map.
        template <class Traits, class Interpolator>
        void init(TokenPair tokenPair) {
            callerMap_[tokenPair] = new Caller<Traits, Interpolator>;
        }

    public:

        // Constructor - populate the CallerMap.
        CallerFactory() {

            // Discount
            init<QuantLib::Discount, QuantLib::BackwardFlat>(TokenPair(Token::Discount, Token::BackwardFlat));
            init<QuantLib::Discount, QuantLib::CubicSpline>(TokenPair(Token::Discount, Token::CubicSpline));
            init<QuantLib::Discount, QuantLib::ForwardFlat>(TokenPair(Token::Discount, Token::ForwardFlat));
            init<QuantLib::Discount, QuantLib::Linear>(TokenPair(Token::Discount, Token::Linear));
            init<QuantLib::Discount, QuantLib::LogCubic>(TokenPair(Token::Discount, Token::LogCubic));
            init<QuantLib::Discount, QuantLib::LogLinear>(TokenPair(Token::Discount, Token::LogLinear));

            // ForwardRate
            init<QuantLib::ForwardRate, QuantLib::BackwardFlat>(TokenPair(Token::ForwardRate, Token::BackwardFlat));
            init<QuantLib::ForwardRate, QuantLib::CubicSpline>(TokenPair(Token::ForwardRate, Token::CubicSpline));
            init<QuantLib::ForwardRate, QuantLib::ForwardFlat>(TokenPair(Token::ForwardRate, Token::ForwardFlat));
            init<QuantLib::ForwardRate, QuantLib::Linear>(TokenPair(Token::ForwardRate, Token::Linear));
            init<QuantLib::ForwardRate, QuantLib::LogCubic>(TokenPair(Token::ForwardRate, Token::LogCubic));
            init<QuantLib::ForwardRate, QuantLib::LogLinear>(TokenPair(Token::ForwardRate, Token::LogLinear));

            // ZeroYield
            init<QuantLib::ZeroYield, QuantLib::BackwardFlat>(TokenPair(Token::ZeroYield, Token::BackwardFlat));
            init<QuantLib::ZeroYield, QuantLib::CubicSpline>(TokenPair(Token::ZeroYield, Token::CubicSpline));
            init<QuantLib::ZeroYield, QuantLib::ForwardFlat>(TokenPair(Token::ZeroYield, Token::ForwardFlat));
            init<QuantLib::ZeroYield, QuantLib::Linear>(TokenPair(Token::ZeroYield, Token::Linear));
            init<QuantLib::ZeroYield, QuantLib::LogCubic>(TokenPair(Token::ZeroYield, Token::LogCubic));
            init<QuantLib::ZeroYield, QuantLib::LogLinear>(TokenPair(Token::ZeroYield, Token::LogLinear));

        }

        // Destructor - deallocate the CallerMap.
        ~CallerFactory() {
            for (CallerMap::const_iterator i = callerMap_.begin(); i != callerMap_.end(); i++)
                delete i->second;
        }

        // Retrieve the Caller pointer corresponding to a given TokenPair
        const CallerBase *getCaller(TokenPair tokenPair) const {
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
Call::callerFactory().getCaller(Call::TokenPair(traits, interpolator))->FUNC(libraryObject_.get())

    const std::vector<QuantLib::Time>& PiecewiseYieldCurve::times(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return CALL(times);
    }

    const std::vector<QuantLib::Date>& PiecewiseYieldCurve::dates(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return CALL(dates);
    }

    const std::vector<QuantLib::Real>& PiecewiseYieldCurve::data(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return CALL(data);
    }

    //const std::vector<QuantLib::Real>& PiecewiseYieldCurve::improvements(
    //    Token::Traits traits, Token::Interpolator interpolator) const {
    //    return CALL(improvements);
    //}

    //QuantLib::Size PiecewiseYieldCurve::iterations(
    //    Token::Traits traits, Token::Interpolator interpolator) const {
    //    return CALL(iterations);
    //}

}

