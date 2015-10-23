/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2011 Peter Caspers

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

/*! \file markovfunctional.hpp
    \brief Markov Functional 1 Factor Model
*/

#ifndef quantlib_markovfunctional_hpp
#define quantlib_markovfunctional_hpp

#include <iostream>
#include <iomanip>

#include <boost/timer/timer.hpp>

#include <math.h>
#include <boost/math/special_functions/fpclassify.hpp>

#include <ql/models/model.hpp>
#include <ql/models/parameter.hpp>
#include <ql/math/interpolation.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>
#include <ql/math/integrals/gaussianquadratures.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#include <ql/math/solvers1D/brent.hpp>
#include <ql/math/errorfunction.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/time/date.hpp>
#include <ql/time/period.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/volatility/swaption/swaptionvolstructure.hpp>
#include <ql/termstructures/volatility/optionlet/optionletvolatilitystructure.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/stochasticprocess.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/pricingengines/blackcalculator.hpp>
#include <ql/utilities/null.hpp>

#include <mfstateprocess.hpp>
#include <kahalesmilesection.hpp>


namespace QuantLib {

    //! One factor Markov Functional model class.
    /*! blah blah
		TODO use voltermstructure's timeFromReference() for std dev calculation of market digitals (or blackVariance()), also in swaption and caplet engine
			 however, is this then still consistent with the process x starting at yts's reference date? think about that and make it consistent ...
    */

    class MarkovFunctional : public TermStructureConsistentModel, public CalibratedModel  {

      public:

		struct ModelSettings {

			ModelSettings(const int xGridPoints,const Real xStdDevs,const int gaussHermitePoints,const Real digitalGap,const int arbitrageCheckGridPoints,
				const Real rateCutLeft, const Rate rateCutRight, const Rate upperRateBound, const bool adjustDigitals, const bool forceExactYtsFit, const int digitalInterpolationDegree):

					xGridPoints_(xGridPoints), xStdDevs_(xStdDevs), gaussHermitePoints_(gaussHermitePoints), digitalGap_(digitalGap),
					arbitrageCheckGridPoints_(arbitrageCheckGridPoints), rateCutLeft_(rateCutLeft), rateCutRight_(rateCutRight),
					upperRateBound_(upperRateBound), adjustDigitals_(adjustDigitals), forceExactYtsFit_(forceExactYtsFit), digitalInterpolationDegree_(digitalInterpolationDegree) {}
			
			void validate() const {

				QL_REQUIRE(xGridPoints_>0,"At least one grid point (" << xGridPoints_ << ") for the state process discretization must be given");
				QL_REQUIRE(xStdDevs_>0.0,"Multiple of standard deviations covered by state process discretization (" << xStdDevs_ << ") must be positive");
				QL_REQUIRE(gaussHermitePoints_>0,"Number of gauss hermite integration points (" << gaussHermitePoints_ << ") must be positive");
				QL_REQUIRE(digitalGap_>0.0 && rateCutLeft_-digitalGap_/2.0 >=0.0 &&
					rateCutLeft_+digitalGap_/2.0<=rateCutRight_ && rateCutRight_+digitalGap_/2.0 <=upperRateBound_,
					"Rate cut left (" << rateCutLeft_ << "), digital gap (" << digitalGap_ << "), rate cut right (" << rateCutRight_ << 
					") and upper rate bound (" << upperRateBound_ << ") are not consistent");
				QL_REQUIRE(arbitrageCheckGridPoints_>0,"Number of grid points for arbitrage check (" << arbitrageCheckGridPoints_ << ") must be positive");
				QL_REQUIRE(digitalInterpolationDegree_>=0 && digitalInterpolationDegree_<=4,"Polynomial degree for digital payoff interpolation (" << digitalInterpolationDegree_ <<") must be between 0 and 4");

			}

			const int xGridPoints_;
			const Real xStdDevs_;
			const int gaussHermitePoints_;
			const Real digitalGap_;
			const int arbitrageCheckGridPoints_;
			const Real rateCutLeft_;
			const Real rateCutRight_;
			const Real upperRateBound_;
			const bool adjustDigitals_;
			const bool forceExactYtsFit_;
			const int digitalInterpolationDegree_;

		};

		struct CalibrationPoint {
			bool isCaplet_;
			//Date expiry_;
			Period tenor_;
			std::vector<Date> paymentDates_;
			std::vector<Real> yearFractions_;
			Real atm_;
			Real annuity_;
			Real afLeftBound_, afRightBound_;
			Real alphaL_,betaL_,alphaR_,betaR_;
			boost::shared_ptr<KahaleSmileSection> smileSection_;
		};

		struct ModelOutputs {
			std::vector<Date> expiries_;
			std::vector<Period> tenors_;
			std::vector<Real> afLeftBounds_;
			std::vector<Real> afRightBounds_;
			std::vector<Real> adjustmentFactors_;
			std::vector<Real> digitalsAdjustmentFactors_;
		};

        MarkovFunctional(const Handle<YieldTermStructure>& termStructure,
						const Real reversion,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Handle<SwaptionVolatilityStructure>& swaptionVol,
						const std::vector<Date>& swaptionExpiries,
						const std::vector<Period>& swaptionTenors,
						const boost::shared_ptr<SwapIndex>& swapIndexBase,
						const int xGridPoints=512, const Real xStdDevs=7.0, const int gaussHermitePoints=32,
						const Real digitalGap=1E-8, const int arbitrageCheckGridPoints=200, const Real rateCutLeft=0.0001, const Real rateCutRight=1.9000,
						const Real upperRateBound=2.0, const bool adjustDigitals=false, const bool forceExactYtsFit=false, const int digitalInterpolationDegree=2);

		MarkovFunctional(const Handle<YieldTermStructure>& termStructure,
						const Real reversion,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Handle<OptionletVolatilityStructure>& capletVol,
						const std::vector<Date>& capletExpiries,
						const boost::shared_ptr<IborIndex>& iborIndex,
						const int xGridPoints=512, const Real xStdDevs=7.0, const int gaussHermitePoints=32,
						const Real digitalGap=1E-8, const int arbitrageCheckGridPoints=200, const Real rateCutLeft=0.0001, const Real rateCutRight=1.9000,
						const Real upperRateBound=2.0, const bool adjustDigitals=false, const bool forceExactYtsFit=false, const int digitalInterpolationDegree=2);

		const ModelSettings& modelSettings() const { return modelSettings_; }
		const ModelOutputs& modelOutputs() const { return modelOutputs_; }

		const Date& numeraireDate() const { return numeraireDate_; }
		const Time& numeraireTime() const { return numeraireTime_; }

		const boost::shared_ptr<StochasticProcess1D> stateProcess() const { return stateProcess_; }

		Real numeraire(const Time t, const Real x=0.0) const;
		Real deflatedZerobond(const Time T, const Time t=0.0, const Real x=0.0) const;

		Real zerobond(const Time T, const Time t=0.0, const Real x=0.0) const;
		Real zerobond(const Date& maturity, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;

		Real zerobondOption(const Option::Type& type, const Date& expiry, const Date& maturity, const Rate strike, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;

		Real forwardRate(const Date& fixing, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;
		Real swapRate(const Date& fixing, const Period& tenor, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;
		Real swapAnnuity(const Date& fixing, const Period& tenor, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;

		Real capletPrice(const Option::Type& type, const Date& expiry, const Rate strike, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;
		Real swaptionPrice(const Option::Type& type, const Date& expiry, const Period& tenor, const Rate strike, const Date& referenceDate = Null<Date>(), const Real x=0.0) const;

      protected:
        
		void generateArguments();

      private:

		void initialize();
	    void updateNumeraireTabulation();

		void makeSwaptionCalibrationPoint(const Date& expiry, const Period& tenor);
		void makeCapletCalibrationPoint(const Date& expiry);
		
		void arbitrageFreeStrikeRange(const Date& expiry, CalibrationPoint& p) const;
		Real marketSwapRate(const Date& expiry, const CalibrationPoint& p, const Real digitalPrice) const;
		Real marketDigitalPrice(const Date& expiry, const CalibrationPoint& p, const Option::Type& type, const Real strike) const;
		static Real gaussianPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real x0, const Real x1);

		ModelSettings modelSettings_;
		ModelOutputs modelOutputs_;

		const bool capletCalibrated_;

		boost::shared_ptr<GaussianQuadrature> gaussHermite_;
		const CumulativeNormalDistribution cnd_;

		boost::shared_ptr<IborIndex> ytsLinkedIborIndex_;
		boost::shared_ptr<StochasticProcess1D> stateProcess_;

		std::vector<boost::shared_ptr<std::vector<Real>>> numeraireDiscretization_;
		std::vector<boost::shared_ptr<Interpolation>> numeraire_;

		Parameter reversion_;
		Parameter& sigma_;
		
		std::vector<Date> volstepdates_;
		std::vector<Time> volsteptimes_;
		Array volsteptimesArray_;				// FIXME this is redundant (copy of volsteptimes_)
		std::vector<double> volatilities_;

		Date numeraireDate_;
		Time numeraireTime_;

		Handle<SwaptionVolatilityStructure> swaptionVol_;
		Handle<OptionletVolatilityStructure> capletVol_;

		std::vector<Date> swaptionExpiries_, capletExpiries_;
		std::vector<Period> swaptionTenors_;
		boost::shared_ptr<SwapIndex> swapIndexBase_;
		boost::shared_ptr<IborIndex> iborIndex_;

		//std::set<CalibrationPoint> calibrationPoints_;
		std::map<Date,CalibrationPoint> calibrationPoints_;
		std::vector<double> times_;
		std::vector<double> y_;

		mutable boost::timer::cpu_timer cpuTimer_,cpuTimer2_;

    };

	//bool operator<(const MarkovFunctional::CalibrationPoint& a, const MarkovFunctional::CalibrationPoint& b);

}


#endif

