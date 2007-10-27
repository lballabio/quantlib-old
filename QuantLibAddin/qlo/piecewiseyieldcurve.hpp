
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

#ifndef qla_piecewiseyieldcurve_hpp
#define qla_piecewiseyieldcurve_hpp

#include <qlo/termstructures.hpp>

namespace QuantLibAddin {

    // The struct 'Token' provides values which act as placeholders for
    // QuantLib types of the same name.
    struct Token {
        enum Traits { Discount, ForwardRate, ZeroYield };
        enum Interpolator { BackwardFlat, Cubic, ForwardFlat, Linear, LogCubic, LogLinear };
    };

    // A wrapper for QuantLib template class PiecewiseYieldCurve<Traits, Interpolator>.
    // Calls to constructor/member functions must specify values for Traits and Interpolator
    // because it is not possible to expose a template class directly to client platforms
    // such as Excel.
    class PiecewiseYieldCurve : public YieldTermStructure {
      public:
        PiecewiseYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& qlrhs,
            const QuantLib::DayCounter& dayCounter,
            const std::string& traitsID,
            const std::string& interpolatorID,
            QuantLib::Real accuracy,
            bool permanent);

        const std::vector<QuantLib::Time>& times(
            Token::Traits traits, Token::Interpolator interpolator) const;

        const std::vector<QuantLib::Date>& dates(
            Token::Traits traits, Token::Interpolator interpolator) const;

        const std::vector<QuantLib::Real>& data(
            Token::Traits traits, Token::Interpolator interpolator) const;

        const std::vector<QuantLib::Real>& improvements(
            Token::Traits traits, Token::Interpolator interpolator) const;

        QuantLib::Size iterations(
            Token::Traits traits, Token::Interpolator interpolator) const;

    };

}

#endif
