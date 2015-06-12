/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file spreadedsmilesection.hpp
    \brief multiplicative smile section class
*/

#ifndef quantlib_multiplicative_smile_section_hpp
#define quantlib_multiplicative_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/handle.hpp>

namespace QuantLib {

    class Quote;

    /* the resulting volatility is multiplier * v + constant, where
       constant is the original volatility */

    class MultiplicativeSmileSection : public SmileSection {
      public:
        MultiplicativeSmileSection(const boost::shared_ptr<SmileSection>&,
                                   const Handle<Quote>& multiplier,
                                   const Handle<Quote>& constant);
        //! \name SmileSection interface
        //@{
        Real minStrike () const;
        Real maxStrike () const;
        Real atmLevel() const;
        const Date& exerciseDate() const;
        Time exerciseTime() const;
        const DayCounter& dayCounter() const;
        const Date& referenceDate() const;
        const VolatilityType nature() const;
        const Rate shift() const;
        //@}
        //! \name LazyObject interface
        //@{
        void update() { notifyObservers(); }
        //@}
      protected:
        Volatility volatilityImpl(Rate strike) const;
      private:
        const boost::shared_ptr<SmileSection> underlyingSection_;
        const Handle<Quote> multiplier_, constant_;
    };

    inline Real MultiplicativeSmileSection::minStrike() const {
        return underlyingSection_->minStrike();
    }

    inline Real MultiplicativeSmileSection::maxStrike() const {
        return underlyingSection_->maxStrike();
    }

    inline Real MultiplicativeSmileSection::atmLevel() const {
        return underlyingSection_->atmLevel();
    }

    inline const Date& MultiplicativeSmileSection::exerciseDate() const {
        return underlyingSection_->exerciseDate();
    }

    inline Time MultiplicativeSmileSection::exerciseTime() const {
        return underlyingSection_->exerciseTime();
    }

    inline const DayCounter& MultiplicativeSmileSection::dayCounter() const {
        return underlyingSection_->dayCounter();
    }

    inline const Date& MultiplicativeSmileSection::referenceDate() const {
        return underlyingSection_->referenceDate();
    }

    inline const VolatilityType MultiplicativeSmileSection::nature() const {
        return underlyingSection_->volatilityType();
    }

    inline const Rate MultiplicativeSmileSection::shift() const {
        return underlyingSection_->shift();
    }
}

#endif
