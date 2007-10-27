
/*
 Copyright (C) 2007 Eric Ehlers

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
            const std::string& traitsID, 
            const std::string& interpolatorID,
            QuantLib::Real accuracy,
            bool permanent) : YieldTermStructure(properties, permanent)
    {
        //QuantLib::Cubic naturalCubic(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    false);

        //QuantLib::Cubic cubic1(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::FirstDerivative, 0.0,
        //    false);

        //QuantLib::Cubic monotoneCubic(
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
                                              accuracy);

        //libraryObject_ = boost::shared_ptr<QuantLib::YieldTermStructure>(new
        //    QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
        //                                  QuantLib::Cubic>(nDays,
        //                                                   calendar,
        //                                                   rateHelpersQL,
        //                                                   dayCounter,
        //                                                   accuracy,
        //                                                   monotoneCubic));

    }

    // Before implementing the member functions it is necessary to provide some logic to wrap
    // the underlying QuantLib template class PiecewiseYieldCurve<Traits, Interpolator>.
    // This logic is placed in namespace Call.

    // TODO 1) This code overlaps somewhat with logic in the Enumeration Registry, consolidate?
    //      2) Generalize this logic to expose any template class to the Addin interface

    namespace Call {

    typedef const boost::shared_ptr<QuantLib::Extrapolator>& extrapolatorPtr;

    // A nontemplate abstract base class to hold wrappers for member functions of
    // PiecewiseYieldCurve<Traits, Interpolator>.  One concrete subclass will be provided
    // for each combination of Traits / Interpolator.
    
    class CallerBase {
    public:
        virtual const std::vector<QuantLib::Time>& times(extrapolatorPtr extrapolator) const = 0;
        virtual const std::vector<QuantLib::Date>& dates(extrapolatorPtr extrapolator) const = 0;
        virtual const std::vector<QuantLib::Real>& data(extrapolatorPtr extrapolator) const = 0;
        virtual const std::vector<QuantLib::Real>& improvements(extrapolatorPtr extrapolator) const = 0;
        virtual QuantLib::Size iterations(extrapolatorPtr extrapolator) const = 0;
    };

    // Concrete base class to wrap member functions of PiecewiseYieldCurve<Traits, Interpolator>.
    // Given a value of type boost::shared_ptr<QuantLib::Extrapolator>, this class downcasts
    // to PiecewiseYieldCurve<Traits, Interpolator> and calls the given member function.

    template <class Traits, class Interpolator>
    class Caller : public CallerBase {

        typedef QuantLib::PiecewiseYieldCurve<Traits, Interpolator> CurveClass;

        boost::shared_ptr<CurveClass> get(extrapolatorPtr extrapolator) const {

            boost::shared_ptr<CurveClass> ret =
                boost::dynamic_pointer_cast<CurveClass>(extrapolator);
            OH_REQUIRE(ret, "Unable to convert from type " << typeid(extrapolator).name()
                << " to type " << typeid(CurveClass).name());
            return ret;
        }

        virtual const std::vector<QuantLib::Time>& times(extrapolatorPtr extrapolator) const {
            return get(extrapolator)->times();
        }

        virtual const std::vector<QuantLib::Date>& dates(extrapolatorPtr extrapolator) const {
            return get(extrapolator)->dates();
        }

        virtual const std::vector<QuantLib::Real>& data(extrapolatorPtr extrapolator) const {
            return get(extrapolator)->data();
        }

        virtual const std::vector<QuantLib::Real>& improvements(extrapolatorPtr extrapolator) const {
            return get(extrapolator)->improvements();
        }

        virtual QuantLib::Size iterations(extrapolatorPtr extrapolator) const {
            return get(extrapolator)->iterations();
        }

    };

    // "TokenPair" - A pair of Tokens indicating a combination of Traits / Interpolator.
    typedef std::pair<Token::Traits, Token::Interpolator> TokenPair;
    // CallerMap - Holds a pointer to Caller for each combination of Traits / Interpolator.
    typedef std::map<TokenPair, boost::shared_ptr<CallerBase> > CallerMap;

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
            case Token::Cubic:
                out << "Cubic>";
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

    // Class CallerFactory wraps the CallerMap and provides logic around the Caller pointers.

    class CallerFactory {

        CallerMap callerMap_;

        // Retrieve the Caller pointer corresponding to a given TokenPair
        boost::shared_ptr<CallerBase> getCaller(TokenPair tokenPair) const {
            CallerMap::const_iterator i = callerMap_.find(tokenPair);
            OH_REQUIRE(i!=callerMap_.end(), "Unable to retrieve caller for type " << tokenPair);
            return i->second;
        }

    public:

        // Constructor - populate the CallerMap.
        CallerFactory() {

            // Discount
            callerMap_[TokenPair(Token::Discount, Token::BackwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::BackwardFlat>);
            callerMap_[TokenPair(Token::Discount, Token::Cubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::Cubic>);
            callerMap_[TokenPair(Token::Discount, Token::ForwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::ForwardFlat>);
            callerMap_[TokenPair(Token::Discount, Token::Linear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::Linear>);
            callerMap_[TokenPair(Token::Discount, Token::LogCubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::LogCubic>);
            callerMap_[TokenPair(Token::Discount, Token::LogLinear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::Discount, QuantLib::LogLinear>);

            // ForwardRate
            callerMap_[TokenPair(Token::ForwardRate, Token::BackwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::BackwardFlat>);
            callerMap_[TokenPair(Token::ForwardRate, Token::Cubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::Cubic>);
            callerMap_[TokenPair(Token::ForwardRate, Token::ForwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::ForwardFlat>);
            callerMap_[TokenPair(Token::ForwardRate, Token::Linear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::Linear>);
            callerMap_[TokenPair(Token::ForwardRate, Token::LogCubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::LogCubic>);
            callerMap_[TokenPair(Token::ForwardRate, Token::LogLinear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ForwardRate, QuantLib::LogLinear>);

            // ZeroYield
            callerMap_[TokenPair(Token::ZeroYield, Token::BackwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::BackwardFlat>);
            callerMap_[TokenPair(Token::ZeroYield, Token::Cubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::Cubic>);
            callerMap_[TokenPair(Token::ZeroYield, Token::ForwardFlat)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::ForwardFlat>);
            callerMap_[TokenPair(Token::ZeroYield, Token::Linear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::Linear>);
            callerMap_[TokenPair(Token::ZeroYield, Token::LogCubic)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::LogCubic>);
            callerMap_[TokenPair(Token::ZeroYield, Token::LogLinear)]
                = boost::shared_ptr<CallerBase>(new Caller<QuantLib::ZeroYield, QuantLib::LogLinear>);

        }

        // Wrappers for member functions of PiecewiseYieldCurve<Traits, Interpolator>.  These functions
        // accept a TokenPair (Traits / Interpolator) and a reference to a QuantLib::Extrapolator.
        // The functions call getCaller() to retrieve the Caller pointer corresponding to the TokenPair,
        // then pass the QuantLib::Extrapolator reference to the Caller which downcasts to the appropriate
        // instantiation of PiecewiseYieldCurve<Traits, Interpolator> and calls the given member function.

        const std::vector<QuantLib::Time>& times(TokenPair tokenPair, extrapolatorPtr extrapolator) const {
            return getCaller(tokenPair)->times(extrapolator);
        }

        const std::vector<QuantLib::Date>& dates(TokenPair tokenPair, extrapolatorPtr extrapolator) const {
            return getCaller(tokenPair)->dates(extrapolator);
        }

        const std::vector<QuantLib::Real>& data(TokenPair tokenPair, extrapolatorPtr extrapolator) const {
            return getCaller(tokenPair)->data(extrapolator);
        }

        const std::vector<QuantLib::Real>& improvements(TokenPair tokenPair, extrapolatorPtr extrapolator) const {
            return getCaller(tokenPair)->improvements(extrapolator);
        }

        QuantLib::Size iterations(TokenPair tokenPair, extrapolatorPtr extrapolator) const {
            return getCaller(tokenPair)->iterations(extrapolator);
        }

    };

    // Basic Singleton behavior for the CallerFactory

    const CallerFactory &callerFactory() {
        static CallerFactory callerFactory_;
        return callerFactory_;
    }

    } // namespace Call

    // QuantLibAddin wrappers for member functions of QuantLib class PiecewiseYieldCurve<Traits, Interpolator>.
    // Invocation of the member function is passed off to the CallerFactory which hides the details
    // of the template class.

    const std::vector<QuantLib::Time>& PiecewiseYieldCurve::times(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return Call::callerFactory().times(Call::TokenPair(traits, interpolator), libraryObject_);
    }

    const std::vector<QuantLib::Date>& PiecewiseYieldCurve::dates(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return Call::callerFactory().dates(Call::TokenPair(traits, interpolator), libraryObject_);
    }

    const std::vector<QuantLib::Real>& PiecewiseYieldCurve::data(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return Call::callerFactory().data(Call::TokenPair(traits, interpolator), libraryObject_);
    }

    const std::vector<QuantLib::Real>& PiecewiseYieldCurve::improvements(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return Call::callerFactory().improvements(Call::TokenPair(traits, interpolator), libraryObject_);
    }

    QuantLib::Size PiecewiseYieldCurve::iterations(
        Token::Traits traits, Token::Interpolator interpolator) const {
        return Call::callerFactory().iterations(Call::TokenPair(traits, interpolator), libraryObject_);
    }

}
