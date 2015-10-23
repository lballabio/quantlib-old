/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

/*! \file swaptionvolmatrix.hpp
    \brief Swaption matrix whith hull white smiles
*/

#ifndef quantlib_swaption_volatility_hullwhite_hpp
#define quantlib_swaption_volatility_hullwhite_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/indexes/SwapIndex.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvoldiscrete.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>
#include <ql/models/shortrate/onefactormodels/hullwhite.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>
#include <ql/math/matrix.hpp>
#include <ql/math/solvers1D/brent.hpp>
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/math/optimization/simplex.hpp>
#include <ql/math/optimization/costfunction.hpp>
#include <ql/math/optimization/endcriteria.hpp>
#include <ql/math/optimization/problem.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/pricingengines/swaption/jamshidianswaptionengine.hpp>
#include <boost/noncopyable.hpp>
#include <vector>

#include <iostream>

#include <hullwhitesmilesection.hpp>

namespace QuantLib {

    class Quote;
    
    class SwaptionVolatilityHullWhite : public SwaptionVolatilityDiscrete,
                                     private boost::noncopyable {
      public:
        //! floating reference date, floating market data
        SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dayCounter);
        //! fixed reference date, floating market data
        SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Date& referenceDate,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dayCounter);
        //! floating reference date, fixed market data
        SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const Matrix& volatilities,
                    const DayCounter& dayCounter);
        //! fixed reference date, fixed market data
        SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Date& referenceDate,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const Matrix& volatilities,
                    const DayCounter& dayCounter);
        // fixed reference date and fixed market data, option dates
        SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
								 const Date& referenceDate,
                                 const std::vector<Date>& optionDates,
                                 const std::vector<Period>& swapTenors,
                                 const Matrix& volatilities,
                                 const DayCounter& dayCounter);
        //! \name LazyObject interface
        //@{
        void performCalculations() const;
        //@}
        //! \name TermStructure interface
        //@{
        Date maxDate() const;
        //@}
        //! \name VolatilityTermStructure interface
        //@{
        Rate minStrike() const;
        Rate maxStrike() const;
        //@}
        //! \name SwaptionVolatilityStructure interface
        //@{
        const Period& maxSwapTenor() const;
        //@}
        //! \name Other inspectors
        //@{
        //! returns the lower indexes of surrounding volatility matrix corners
        std::pair<Size,Size> locate(const Date& optionDate,
                                    const Period& swapTenor) const {
            return locate(timeFromReference(optionDate),
                          swapLength(swapTenor));
        }
        //! returns the lower indexes of surrounding volatility matrix corners
        std::pair<Size,Size> locate(Time optionTime,
                                    Time swapLength) const {
            return std::make_pair(interpolation_.locateY(optionTime),
                                  interpolation_.locateX(swapLength));
        }
        //@}
      protected:
        boost::shared_ptr<SmileSection> smileSectionImpl(const Date&,
                                                         const Period&) const;
        boost::shared_ptr<SmileSection> smileSectionImpl(Time,
                                                         Time) const;
        Volatility volatilityImpl(Time optionTime,
                                  Time swapLength,
                                  Rate strike) const;
		Volatility volatilityImpl(const Date& optionDate, const Period& swapTenor, Rate strike) const;

      private:
        void checkInputs(Size volRows,
                         Size volsColumns) const;
        void registerWithMarketData();
        std::vector<std::vector<Handle<Quote> > > volHandles_;
        mutable Matrix volatilities_;
        mutable Interpolation2D interpolation_;
		mutable Matrix hwsigmas_;
		mutable Interpolation2D interpolationSigma_;
		Real reversion_;
		Handle<YieldTermStructure> yts_;
		boost::shared_ptr<SwapIndex> indexBase_;

		struct calibrationFunction : CostFunction {
			
			calibrationFunction(boost::shared_ptr<CalibratedModel> model, boost::shared_ptr<SwaptionHelper> helper) : model_(model), helper_(helper) {}

			Real value(const Array& params0) const {
				Array params(2);
				params[0]=model_->params()[0];
				params[1]=params0[0]*params0[0];
				model_->setParams(params);
				Real error=helper_->calibrationError();
				return error;	
			}

			Disposable<Array> values(const Array& params) const {
			   Array result(1);
			   result[0] = value(params);
			   return result;
			}

			boost::shared_ptr<CalibratedModel> model_;
			boost::shared_ptr<SwaptionHelper> helper_;
		};
	};
		
		

    // inline definitions

    inline Date SwaptionVolatilityHullWhite::maxDate() const {
        return optionDates_.back();
    }

    inline Rate SwaptionVolatilityHullWhite::minStrike() const {
        return QL_MIN_REAL;
    }

    inline Rate SwaptionVolatilityHullWhite::maxStrike() const {
        return QL_MAX_REAL;
    }

    inline const Period& SwaptionVolatilityHullWhite::maxSwapTenor() const {
        return swapTenors_.back();
    }

    inline Volatility SwaptionVolatilityHullWhite::volatilityImpl(Time optionTime,
                                                               Time swapLength,
                                                               Rate strike) const {
        calculate();
		return smileSection(optionTime,swapLength,true)->volatility(strike);
    }

	inline Volatility SwaptionVolatilityHullWhite::volatilityImpl(const Date& optionDate,
                                                const Period& swapTenor, Rate strike) const {
        calculate();
		return smileSection(optionDate,swapTenor,true)->volatility(strike);
    }

}

#endif
