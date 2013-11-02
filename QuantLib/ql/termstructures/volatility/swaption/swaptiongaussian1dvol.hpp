/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file swaptiongaussian1dvol.hpp
    \brief gaussian1d model generated swaption volatility
    \warning the SwaptionVolatilityStructure is initialized always with a fixed reference date
             even if the model is based on a floating term structure
             Furthermore the bdc is hardcoded to Following
*/

#ifndef quantlib_swaption_gaussian1d_vol_hpp
#define quantlib_swaption_gaussian1d_vol_hpp

#include <ql/experimental/models/gaussian1dmodel.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>

namespace QuantLib {

    class Gaussian1dSwaptionVolatility : public SwaptionVolatilityStructure {
    
    public:
        
        // the index might be associated to forwarding and discounting curves
        // otherwise the model's curve will be used for both

        Gaussian1dSwaptionVolatility(boost::shared_ptr<Gaussian1dModel> &model,
                              boost::shared_ptr<SwapIndex> &index);

        //@{
        Date maxDate() const;
        //@}
        //! \name VolatilityTermStructure interface
        //@{
        Real minStrike() const;
        Real maxStrike() const;
        //@}
        //! \name SwaptionVolatilityStructure interface
        //@{
        const Period& maxSwapTenor() const;
        //@}

    protected:
        
        boost::shared_ptr<SmileSection> smileSectionImpl(
                                                const Date& optionDate,
                                                const Period& swapTenor) const;
        boost::shared_ptr<SmileSection> smileSectionImpl(
                                                Time optionTime,
                                                Time swapLength) const = 0;
        Volatility volatilityImpl(const Date& optionDate,
                                  const Period& swapTenor,
                                  Rate strike) const;
        Volatility volatilityImpl(Time optionTime,
                                  Time swapLength,
                                  Rate strike) const;

    private:

        boost::shared_ptr<Gaussian1dModel> model_;
        boost::shared_ptr<SwapIndex> index_;
        Period maxSwapTenor_;

        class DateHelper;
        friend class DateHelper;
        class DateHelper {
         public:
            DateHelper(const TermStructure& ts, const Time t) : ts_(ts), t_(t) {}
            Real operator()(Real date) const {
                Date d1(static_cast<BigInteger>(date));
                Date d2(static_cast<BigInteger>(date)+1);
                Real t1 = ts_.timeFromReference(d1)-t_;
                Real t2 = ts_.timeFromReference(d2)-t_;
                Real h = date - static_cast<BigInteger>(date);
                return h*t2+(1.0-h)*t1;
            }
            Real derivative(Real date) const {
                // use fwd difference to avoid dates before reference date
                return (operator()(date+1E-6)-operator()(date))*1E6; 
            }
            const TermStructure& ts_;
            const Time t_;
        };



    };


    // inline definitions

    inline Date Gaussian1dSwaptionVolatility::maxDate() const {
        return Date::maxDate();
    }

    inline Real Gaussian1dSwaptionVolatility::minStrike() const {
        return 0.0;
    }

    inline Real Gaussian1dSwaptionVolatility::maxStrike() const {
        return QL_MAX_REAL;
    }

    inline const Period& Gaussian1dSwaptionVolatility::maxSwapTenor() const {
        return maxSwapTenor_;

    }
    

}


#endif
