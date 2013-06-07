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

#include <swaptionvolhullwhite.hpp>
#include <ql/termstructures/volatility/flatsmilesection.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/utilities/dataformatters.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>

namespace QuantLib {

    // floating reference date, floating market data
    SwaptionVolatilityHullWhite::SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Calendar& cal,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionT,
                    const std::vector<Period>& swapT,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dc)
    : reversion_(reversion),yts_(yts),indexBase_(indexBase),SwaptionVolatilityDiscrete(optionT, swapT, 0, cal, bdc, dc),
      volHandles_(vols),
	  hwsigmas_(vols.size(), vols.front().size()),
      volatilities_(vols.size(), vols.front().size()) {
        checkInputs(volatilities_.rows(), volatilities_.columns());
        registerWithMarketData();
        interpolation_ =
            BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  volatilities_);
		interpolationSigma_ =
			BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  hwsigmas_);
   }

    // fixed reference date, floating market data
    SwaptionVolatilityHullWhite::SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Date& refDate,
                    const Calendar& cal,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionT,
                    const std::vector<Period>& swapT,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dc)
    : reversion_(reversion),yts_(yts),indexBase_(indexBase),SwaptionVolatilityDiscrete(optionT, swapT, refDate, cal, bdc, dc),
      volHandles_(vols),
      hwsigmas_(vols.size(), vols.front().size()),
	  volatilities_(vols.size(), vols.front().size()) {
        checkInputs(volatilities_.rows(), volatilities_.columns());
        registerWithMarketData();
        interpolation_ =
            BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  volatilities_);
		interpolationSigma_ =
			BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  hwsigmas_);
    }

    // floating reference date, fixed market data
    SwaptionVolatilityHullWhite::SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                        const Calendar& cal,
                        BusinessDayConvention bdc,
                        const std::vector<Period>& optionT,
                        const std::vector<Period>& swapT,
                        const Matrix& vols,
                        const DayCounter& dc)
    : reversion_(reversion),yts_(yts),indexBase_(indexBase),SwaptionVolatilityDiscrete(optionT, swapT, 0, cal, bdc, dc),
      volHandles_(vols.rows()),
      hwsigmas_(vols.rows(), vols.columns()),
	  volatilities_(vols.rows(), vols.columns()) {

        checkInputs(vols.rows(), vols.columns());

        // fill dummy handles to allow generic handle-based
        // computations later on
        for (Size i=0; i<vols.rows(); ++i) {
            volHandles_[i].resize(vols.columns());
            for (Size j=0; j<vols.columns(); ++j)
                volHandles_[i][j] = Handle<Quote>(boost::shared_ptr<Quote>(new
                    SimpleQuote(vols[i][j])));
        }
        interpolation_ =
            BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  volatilities_);
		interpolationSigma_ =
			BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  hwsigmas_);
    }

    // fixed reference date, fixed market data
    SwaptionVolatilityHullWhite::SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                        const Date& refDate,
                        const Calendar& cal,
                        BusinessDayConvention bdc,
                        const std::vector<Period>& optionT,
                        const std::vector<Period>& swapT,
                        const Matrix& vols,
                        const DayCounter& dc)
    : reversion_(reversion),yts_(yts),indexBase_(indexBase),SwaptionVolatilityDiscrete(optionT, swapT, refDate, cal, bdc, dc),
	  volHandles_(vols.rows()),
      hwsigmas_(vols.rows(), vols.columns()),
      volatilities_(vols.rows(), vols.columns()) {

        checkInputs(vols.rows(), vols.columns());

        // fill dummy handles to allow generic handle-based
        // computations later on
        for (Size i=0; i<vols.rows(); ++i) {
            volHandles_[i].resize(vols.columns());
            for (Size j=0; j<vols.columns(); ++j)
                volHandles_[i][j] = Handle<Quote>(boost::shared_ptr<Quote>(new
                    SimpleQuote(vols[i][j])));
        }
        interpolation_ =
            BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  volatilities_);
		interpolationSigma_ =
			BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  hwsigmas_);
    }

    // fixed reference date and fixed market data, option dates
    SwaptionVolatilityHullWhite::SwaptionVolatilityHullWhite(const Real reversion, const Handle<YieldTermStructure>& yts, const boost::shared_ptr<SwapIndex> indexBase,
                    const Date& today,
                    const std::vector<Date>& optionDates,
                    const std::vector<Period>& swapT,
                    const Matrix& vols,
                    const DayCounter& dc)
    : reversion_(reversion),yts_(yts),indexBase_(indexBase),SwaptionVolatilityDiscrete(optionDates, swapT, today, Calendar(), Following, dc),
      volHandles_(vols.rows()),
      hwsigmas_(vols.rows(), vols.columns()),
	  volatilities_(vols.rows(), vols.columns()) {

        checkInputs(vols.rows(), vols.columns());

        // fill dummy handles to allow generic handle-based
        // computations later on
        for (Size i=0; i<vols.rows(); ++i) {
            volHandles_[i].resize(vols.columns());
            for (Size j=0; j<vols.columns(); ++j)
                volHandles_[i][j] = Handle<Quote>(boost::shared_ptr<Quote>(new
                    SimpleQuote(vols[i][j])));
        }
        interpolation_ =
            BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  volatilities_);
		interpolationSigma_ =
			BilinearInterpolation(swapLengths_.begin(), swapLengths_.end(),
                                  optionTimes_.begin(), optionTimes_.end(),
                                  hwsigmas_);
    }

    void SwaptionVolatilityHullWhite::checkInputs(Size volRows,
                                               Size volsColumns) const {
        QL_REQUIRE(nOptionTenors_==volRows,
                   "mismatch between number of option dates (" <<
                   nOptionTenors_ << ") and number of rows (" << volRows <<
                   ") in the vol matrix");
        QL_REQUIRE(nSwapTenors_==volsColumns,
                   "mismatch between number of swap tenors (" <<
                   nSwapTenors_ << ") and number of rows (" << volsColumns <<
                   ") in the vol matrix");
    }

    void SwaptionVolatilityHullWhite::registerWithMarketData()
    {
        for (Size i=0; i<volHandles_.size(); ++i)
            for (Size j=0; j<volHandles_.front().size(); ++j)
                registerWith(volHandles_[i][j]);
    }

    void SwaptionVolatilityHullWhite::performCalculations() const {

        SwaptionVolatilityDiscrete::performCalculations();

        // we might use iterators here...
        for (Size i=0; i<volatilities_.rows(); ++i)
            for (Size j=0; j<volatilities_.columns(); ++j)
                volatilities_[i][j] = volHandles_[i][j]->value();

		interpolation_.update();

		//LevenbergMarquardt lm;
		Simplex sp(0.001); // we should use another optimizer here...
		EndCriteria ec(1000,500,1E-8,1E-8,1E-8);
		NoConstraint cons;
		Array val(1);
		for(Size i=0; i<optionTenors_.size() ; i++) {
			for(Size j=0; j<swapTenors_.size() ; j++) {
				boost::shared_ptr<SwaptionHelper> sh(new SwaptionHelper(optionTenors_[i],swapTenors_[j],Handle<Quote>(new SimpleQuote(volatilities_[i][j])),indexBase_->iborIndex(),indexBase_->fixedLegTenor(),
					indexBase_->dayCounter(),Actual360(),yts_)); // FIXME float leg day counter hardcoded ?
				boost::shared_ptr<HullWhite> hwTmp(new HullWhite(yts_,reversion_));
				boost::shared_ptr<JamshidianSwaptionEngine> jamshidianEngine(new JamshidianSwaptionEngine(hwTmp,yts_));
				sh->setPricingEngine(jamshidianEngine);
				calibrationFunction cfct(hwTmp,sh);
				val[0]=sqrt(0.01);
				Problem p(cfct,cons,val);
				sp.minimize(p,ec);
				hwsigmas_[i][j] = p.currentValue()[0]*p.currentValue()[0];
				//std::cout << std::setprecision(8) << "calibration: " << optionTenors_[i] << "/" << swapTenors_[j] << " sigma=" << hwsigmas_[i][j] << " model=" << sh->modelValue() << " market=" << sh->marketValue() << std::endl;
			}
		}

		interpolationSigma_.update();

    }

    boost::shared_ptr<SmileSection>
    SwaptionVolatilityHullWhite::smileSectionImpl(const Date& d,
                                               const Period& swapTenor) const {

		 calculate();
		 Time optionTime = timeFromReference(d);
         Time swapLength = SwaptionVolatilityStructure::swapLength(swapTenor);
		
		 return boost::shared_ptr<SmileSection>(new
            HullWhiteSmileSection(d, swapTenor, indexBase_, yts_, reversion_, interpolationSigma_(swapLength,optionTime)));
       
    }

    boost::shared_ptr<SmileSection>
    SwaptionVolatilityHullWhite::smileSectionImpl(Time optionTime,
                                               Time swapLength) const {

		calculate();
		Date optionDate = Date(static_cast<BigInteger>(optionInterpolator_(optionTime)));
        Rounding rounder(0);
        Period swapTenor(static_cast<Integer>(rounder(swapLength*12.0)), Months);
        return smileSection(optionDate, swapTenor);
        
    }

}
