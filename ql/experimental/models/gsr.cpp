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

#include <ql/experimental/models/gsr.hpp>

namespace QuantLib {

    Gsr::Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T) : 
	  CalibratedModel(1), TermStructureConsistentModel(termStructure),
	  calibrateReversion_(false),
	  reversion_(NullParameter()),  // warning C4413 does not matter, because we do not use this variable if calibrateReversios == false
	  sigma_(arguments_[0]),                   
	  volatilities_(volatilities), reversions_(std::vector<Real>(1,reversion)), volstepdates_(volstepdates) {

		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		initialize(T);

    }

	Gsr::Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T) : 
	  CalibratedModel(1), TermStructureConsistentModel(termStructure),
	  calibrateReversion_(false),
	  reversion_(NullParameter()), // warning C4413 does not matter, because we do not use this variable if calibrateReversios == false
	  sigma_(arguments_[0]), 
	  volatilities_(volatilities), reversions_(reversions), volstepdates_(volstepdates) {
		
		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		initialize(T);

    }

      Gsr::Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T) : 
	  CalibratedModel(2), TermStructureConsistentModel(termStructure),
	  calibrateReversion_(true),
	  reversion_(arguments_[1]),  // warning C4413 does not matter, because we do not use this variable if calibrateReversios == false
	  sigma_(arguments_[0]),                   
	  volatilities_(volatilities), reversions_(std::vector<Real>(1,reversion)), volstepdates_(volstepdates) {

		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		initialize(T);

    }

	Gsr::Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T) : 
	  CalibratedModel(2), TermStructureConsistentModel(termStructure),
	  calibrateReversion_(true),
	  reversion_(arguments_[1]), // warning C4413 does not matter, because we do not use this variable if calibrateReversios == false
	  sigma_(arguments_[0]), 
	  volatilities_(volatilities), reversions_(reversions), volstepdates_(volstepdates) {
		
		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		initialize(T);

    }


	void Gsr::initialize(Real T) {

		volsteptimes_.clear();
		volsteptimesArray_=Array(volstepdates_.size());
		int j=0;
		for(std::vector<Date>::const_iterator i = volstepdates_.begin(); i!= volstepdates_.end(); i++, j++) {
			volsteptimes_.push_back(termStructure()->timeFromReference(*i));
			volsteptimesArray_[j]=volsteptimes_[j];
			if(j==0) QL_REQUIRE(volsteptimes_[0] > 0.0,"volsteptimes must be positive (" << volsteptimes_[0] << ")");
			else QL_REQUIRE(volsteptimes_[j] > volsteptimes_[j-1],"volsteptimes must be strictly increasing (" << volsteptimes_[j-1] << "@" << (j-1) << ", " << volsteptimes_[j] << "@" << j << ")");
		}

		QL_REQUIRE(volatilities_.size() == volsteptimes_.size()+1,"there must be n+1 volatilities (" << volatilities_.size() << ") for n volatility step times (" << volsteptimes_.size() << ")");
		//sigma_ = PiecewiseConstantParameter(volsteptimes_,PositiveConstraint());   
		sigma_ = PiecewiseConstantParameter(volsteptimes_,NoConstraint());   
		for(Size i=0;i<sigma_.size();i++) {
			sigma_.setParam(i,volatilities_[i]);
		}

		QL_REQUIRE(reversions_.size() == 1 || reversions_.size() == volsteptimes_.size()+1,"there must be 1 or n+1 reversions (" << reversions_.size() << ") for n volatility step times (" << volsteptimes_.size() << ")");
		if(reversions_.size() == 1) {
			if(calibrateReversion_)
				reversion_ = ConstantParameter(reversions_[0],NoConstraint());
			else
				reversionNc_ = ConstantParameter(reversions_[0],NoConstraint());
		}
		else {
			if(calibrateReversion_)
				reversion_ = PiecewiseConstantParameter(volsteptimes_,NoConstraint());
			else
				reversionNc_ = PiecewiseConstantParameter(volsteptimes_,NoConstraint());
			for(Size i=0;i<reversions_.size();i++) {
				if(calibrateReversion_)
					reversion_.setParam(i,reversions_[i]);
				else
					reversionNc_.setParam(i,reversions_[i]);
			}
		}
		
		stateProcess_ = boost::shared_ptr<GsrProcess>(new GsrProcess(volsteptimesArray_,sigma_.params(),calibrateReversion_? reversion_.params() : reversionNc_.params(),T));

		LazyObject::registerWith(stateProcess_); // forward measure time may change, the model must be notified then
        LazyObject::registerWith(termStructure());

	}

	const Disposable<Array> Gsr::yGrid(const Real stdDevs, const int gridPoints, const Real T, const Real t, const Real y) const {

		Array result(2*gridPoints+1,0.0);

		Real stdDev_0_t = stateProcess_->stdDeviation(0.0,0.0,t); // we use that the standard deviation is independent of $x$ here
		Real stdDev_0_T = stateProcess_->stdDeviation(0.0,0.0,T);

		Real e_0_t = stateProcess_->expectation(0.0,0.0,t);
		Real e_0_T = stateProcess_->expectation(0.0,0.0,T);
		Real x_t = y*stdDev_0_t + e_0_t;
		Real e_t_T = stateProcess_->expectation(t,x_t,T-t);

		Real stdDev_t_T = stateProcess_->stdDeviation(t,x_t,T-t);

		Real h = stdDevs / ((Real)gridPoints);

		for(int j=-gridPoints;j<=gridPoints;j++) {
			result[j+gridPoints] = stdDev_0_T > QL_EPSILON ? ( (e_t_T + stdDev_t_T*((Real)j)*h) - e_0_T) / stdDev_0_T : 0.0; // second case makes trouble anyway ... maybe look at that again later
		}

		return result;

	}
	
	const Real Gsr::zerobond(Time T, Time t, Real y, boost::shared_ptr<Interpolation> discountSpread) const {

		calculate();

		Real x = y*stateProcess_->stdDeviation(0.0,0.0,t)+stateProcess_->expectation(0.0,0.0,t); 
		Real gtT = stateProcess_->G(t,T,x);

		//std::cout << ";" << x << ";" << gtT << ";";

		return termStructure()->discount(T,true) / termStructure()->discount(t,true) * exp( -x*gtT-0.5*stateProcess_->y(t)*gtT*gtT )
			* ( discountSpread ? std::exp(- discountSpread->operator()(T,true)*T + discountSpread->operator()(t,true)*t ) : 1.0 ); 

	}

	const Real Gsr::zerobond(const Date& maturity, const Date& referenceDate, const Real y, boost::shared_ptr<Interpolation> discountSpread) const {

		calculate();
		return zerobond(termStructure()->timeFromReference(maturity),
			referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate),y,discountSpread);

	}

	const Real Gsr::numeraire(Time t, Real y, boost::shared_ptr<Interpolation> discountSpread) const {

		calculate();
		return zerobond(stateProcess_->getForwardMeasureTime(),t,y,discountSpread);

	}

	const Real Gsr::forwardRate(const Date& fixing, boost::shared_ptr<IborIndex> iborIdx, const Date& referenceDate, const Real y, boost::shared_ptr<Interpolation> forwardSpread) const {

		calculate();

		QL_REQUIRE(iborIdx,"No ibor index given");

		Date valueDate = iborIdx->valueDate(fixing);
		Date endDate = iborIdx->fixingCalendar().advance(valueDate,iborIdx->tenor(),
								iborIdx->businessDayConvention(),iborIdx->endOfMonth()); // FIXME Here we should use the calculation date calendar ?
		Real dcf = iborIdx->dayCounter().yearFraction(valueDate,endDate);

		return ( zerobond(valueDate,referenceDate,y,forwardSpread) - zerobond(endDate,referenceDate,y,forwardSpread) ) / 
							(dcf * zerobond(endDate,referenceDate,y,forwardSpread));

	}

	const Real Gsr::swapRate(const Date& fixing, const Period& tenor, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate, const Real y, boost::shared_ptr<Interpolation> forwardSpread, boost::shared_ptr<Interpolation> discountSpread) const {

		calculate();

		QL_REQUIRE(swapIdx,"No swap index given");

		SwapIndex tmpIdx = SwapIndex(swapIdx->familyName(), tenor, swapIdx->fixingDays(),
                                 swapIdx->currency(), swapIdx->fixingCalendar(), swapIdx->fixedLegTenor(),
                                 swapIdx->fixedLegConvention(), swapIdx->dayCounter(), swapIdx->iborIndex());
		boost::shared_ptr<VanillaSwap> underlying = tmpIdx.underlyingSwap(fixing);
		Schedule sched = underlying->fixedSchedule();
		Real annuity = swapAnnuity(fixing,tenor,swapIdx,referenceDate,y,discountSpread);
		Rate atm;
		if(!forwardSpread && !discountSpread) {
			atm = ( zerobond(sched.dates().front(),referenceDate,y) - zerobond(sched.calendar().adjust(sched.dates().back(), underlying->paymentConvention()), referenceDate, y) ) / annuity;
		}
		else {
			Schedule floatSched = underlying->floatingSchedule();
			Real floatLeg=0.0;
			for(Size i=1; i<floatSched.size(); i++) {
				floatLeg += ( zerobond( floatSched[i-1], referenceDate, y, forwardSpread ) / zerobond ( floatSched[i], referenceDate, y, forwardSpread ) - 1.0 ) *
										zerobond( floatSched.calendar().adjust(floatSched[i], underlying->paymentConvention()), referenceDate, y , discountSpread );
			}
			atm = floatLeg / annuity;
		}
		return atm;

	}

	const Real Gsr::swapAnnuity(const Date& fixing, const Period& tenor, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate, const Real y, boost::shared_ptr<Interpolation> discountSpread) const {

		calculate();

		QL_REQUIRE(swapIdx,"No swap index given");

		SwapIndex tmpIdx = SwapIndex(swapIdx->familyName(), tenor, swapIdx->fixingDays(),
                                 swapIdx->currency(), swapIdx->fixingCalendar(), swapIdx->fixedLegTenor(),
                                 swapIdx->fixedLegConvention(), swapIdx->dayCounter(), swapIdx->iborIndex());
		boost::shared_ptr<VanillaSwap> underlying = tmpIdx.underlyingSwap(fixing);
		Schedule sched = underlying->fixedSchedule();

		Real annuity=0.0;
		for(unsigned int j=1; j<sched.size(); j++) {
			annuity += zerobond(sched.calendar().adjust(sched.date(j),underlying->paymentConvention()),referenceDate,y,discountSpread) * 
				swapIdx->dayCounter().yearFraction( sched.date(j-1) , sched.date(j) );
		}
		return annuity;

	}

	const Real Gsr::zerobondOption(const Option::Type& type, const Date& expiry, const Date& valueDate, const Date& maturity, const Rate strike, const Date& referenceDate, const Real y) {

		calculate();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time valueTime = termStructure()->timeFromReference(valueDate);
		Time maturityTime = termStructure()->timeFromReference(maturity);
		Time referenceTime =  referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		struct VolHelper {

			VolHelper(boost::shared_ptr<GsrProcess> p, Real t1, Real t2) : p_(p), t1_(t1), t2_(t2) {}

			Real operator()(Real u) const {
				Real v = p_->diffusion(u,0.0)*(p_->G(u,t1_,0.0)-p_->G(u,t2_,0.0)); // diffusion and G is independent of x, so putting 0.0 here is safe
				return v*v;
			}

			boost::shared_ptr<GsrProcess> p_;
			Real t1_,t2_;

		} volHelper(stateProcess_,valueTime,maturityTime);

		//SimpsonIntegral s(1E-8,100);
		GaussLobattoIntegral s(10000,1E-8);
		Real v = s(volHelper,referenceTime,fixingTime);
		Real p1 = zerobond(valueDate,referenceDate,y);
		Real p2 = zerobond(maturity,referenceDate,y);
		Real d1 = (std::log(p2/(strike*p1)) + v/2.0) / std::sqrt(v);
		Real d2 = d1 - std::sqrt(v);

		CumulativeNormalDistribution cnd;
		Real call = p2 * cnd(d1) - p1 * strike * cnd(d2);

		if(type == Option::Call) return call;
		else return call - (termStructure()->discount(maturity)/termStructure()->discount(valueTime) - strike) * termStructure()->discount(valueTime);

	}

	/*const Real Gsr::zerobondOption(const Option::Type& type, const Date& expiry, const Date& valueDate, const Date& maturity, const Rate strike, const Date& referenceDate, const Real y) {
		return zerobondOption(type,expiry,maturity,strike,referenceDate,y);
	}*/

	const Real Gsr::zerobondOption(const Option::Type& type, const Date& expiry, const Date& maturity, const Rate strike, const Date& referenceDate, const Real y,
		boost::shared_ptr<Interpolation> discountSpread,
		const Size yGridPoints, const Real yStdDevs, const bool extrapolatePayoff, const bool flatPayoffExtrapolation) const {

		calculate();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Array yg = yGrid(yStdDevs,yGridPoints,fixingTime,referenceTime,y);
		Array z = yGrid(yStdDevs,yGridPoints);

		Array p(yg.size());

		for(Size i=0;i<yg.size();i++) {
			Real discount = zerobond(maturity,expiry,yg[i],discountSpread);
			p[i] = std::max((type == Option::Call ? 1.0 : -1.0) * (discount-strike), 0.0 ) / numeraire(fixingTime,yg[i],discountSpread);
		}

		CubicInterpolation payoff(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);

		Real price = 0.0;
		for(Size i=0;i<z.size()-1;i++) {
			price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[i], payoff.bCoefficients()[i], payoff.aCoefficients()[i], p[i], z[i], z[i], z[i+1] );
		}
		if(extrapolatePayoff) {
			if(flatPayoffExtrapolation) {
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[0], z[0], -100.0 , z[0] );
			}
			else {
				if(type == Option::Call) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[z.size()-2], payoff.bCoefficients()[z.size()-2], payoff.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				if(type == Option::Put) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[0], payoff.bCoefficients()[0], payoff.aCoefficients()[0], p[0], z[0], -100.0 , z[0] );
			}
		}

		return numeraire(referenceTime,y) * price;

	}

	const Real Gsr::swaptionPrice(const Option::Type& type, const Date& expiry, const Period& tenor,  const Rate strike, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate, const Real y,
		const Size yGridPoints, const Real yStdDevs, const bool extrapolatePayoff, const bool flatPayoffExtrapolation) const {
		
		calculate();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Array yg = yGrid(yStdDevs,yGridPoints,fixingTime,referenceTime,y);
		Array z = yGrid(yStdDevs,yGridPoints);
		Array p(yg.size());

		for(Size i=0;i<yg.size();i++) {
			Real annuity=swapAnnuity(expiry,tenor,swapIdx,expiry,yg[i]);
			Rate atm=swapRate(expiry,tenor,swapIdx,expiry,yg[i]);
			p[i] = annuity * std::max((type == Option::Call ? 1.0 : -1.0) * (atm-strike), 0.0) / numeraire(fixingTime,yg[i]);
		}

		CubicInterpolation payoff(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);

		Real price = 0.0;
		for(Size i=0;i<z.size()-1;i++) {
			price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[i], payoff.bCoefficients()[i], payoff.aCoefficients()[i], p[i], z[i], z[i], z[i+1] );
		}
		if(extrapolatePayoff) {
			if(flatPayoffExtrapolation) {
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[0], z[0], -100.0 , z[0] );
			}
			else {
				if(type == Option::Call) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[z.size()-2], payoff.bCoefficients()[z.size()-2], payoff.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				if(type == Option::Put) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[0], payoff.bCoefficients()[0], payoff.aCoefficients()[0], p[0], z[0], -100.0 , z[0] );
			}
		}


		return numeraire(referenceTime,y) * price;

	}

	const Real Gsr::capletPrice(const Option::Type& type, const Date& expiry, const Rate strike, boost::shared_ptr<IborIndex> iborIdx, const Date& referenceDate, const Real y,
		const Size yGridPoints, const Real yStdDevs, const bool extrapolatePayoff, const bool flatPayoffExtrapolation) const {

		calculate();

		QL_REQUIRE(iborIdx,"No ibor index given");

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Array yg = yGrid(yStdDevs,yGridPoints,fixingTime,referenceTime,y);
		Array z = yGrid(yStdDevs,yGridPoints);
		Array p(yg.size());

		Date valueDate = iborIdx->valueDate(expiry);
		Date endDate = iborIdx->fixingCalendar().advance(valueDate,iborIdx->tenor(),
								iborIdx->businessDayConvention(),iborIdx->endOfMonth()); // FIXME Here we should use the calculation date calendar ?
		Real dcf = iborIdx->dayCounter().yearFraction(valueDate,endDate);

		for(Size i=0;i<yg.size();i++) {
			Real annuity=zerobond(endDate,expiry,yg[i]) * dcf;
			Rate atm=forwardRate(expiry,iborIdx,expiry,yg[i]);
			p[i] = annuity * std::max((type == Option::Call ? 1.0 : -1.0) * (atm-strike), 0.0 ) / numeraire(fixingTime,yg[i]);
		}

		CubicInterpolation payoff(z.begin(),z.end(),p.begin(),CubicInterpolation::Spline,true,CubicInterpolation::Lagrange,0.0,CubicInterpolation::Lagrange,0.0);

		Real price = 0.0;
		for(Size i=0;i<z.size()-1;i++) {
			price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[i], payoff.bCoefficients()[i], payoff.aCoefficients()[i], p[i], z[i], z[i], z[i+1] );
		}
		if(extrapolatePayoff) {
			if(flatPayoffExtrapolation) {
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				price += gaussianShiftedPolynomialIntegral( 0.0, 0.0, 0.0, 0.0, p[0], z[0], -100.0 , z[0] );
			}
			else {
				if(type==Option::Call) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[z.size()-2], payoff.bCoefficients()[z.size()-2], payoff.aCoefficients()[z.size()-2], p[z.size()-2], z[z.size()-2], z[z.size()-1], 100.0 );
				if(type==Option::Put) price += gaussianShiftedPolynomialIntegral( 0.0, payoff.cCoefficients()[0], payoff.bCoefficients()[0], payoff.aCoefficients()[0], p[0], z[0], -100.0 , z[0] );
			}
		}

		return numeraire(referenceTime,y) * price;

	}

	const Real Gsr::gaussianPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real y0, const Real y1) const {
		#ifdef GSR_ENABLE_NTL
				const boost::math::ntl::RR aa=4.0*a, ba=2.0*M_SQRT2*b, ca=2.0*c, da=M_SQRT2*d;
				const boost::math::ntl::RR x0=y0*M_SQRT1_2, x1=y1*M_SQRT1_2;
				const boost::math::ntl::RR res = (0.125*(3.0*aa+2.0*ca+4.0*e)*boost::math::erf(x1)-1.0/(4.0*M_SQRTPI)*exp(-x1*x1)*(2.0*aa*x1*x1*x1+3.0*aa*x1+2.0*ba*(x1*x1+1.0)+2.0*ca*x1+2.0*da))-
													(0.125*(3.0*aa+2.0*ca+4.0*e)*boost::math::erf(x0)-1.0/(4.0*M_SQRTPI)*exp(-x0*x0)*(2.0*aa*x0*x0*x0+3.0*aa*x0+2.0*ba*(x0*x0+1.0)+2.0*ca*x0+2.0*da));
				return NTL::to_double(res.value());
		#else
		const Real aa=4.0*a, ba=2.0*M_SQRT2*b, ca=2.0*c, da=M_SQRT2*d;
		const Real x0=y0*M_SQRT1_2, x1=y1*M_SQRT1_2;
		return (0.125*(3.0*aa+2.0*ca+4.0*e)*boost::math::erf(x1)-1.0/(4.0*M_SQRTPI)*exp(-x1*x1)*(2.0*aa*x1*x1*x1+3.0*aa*x1+2.0*ba*(x1*x1+1.0)+2.0*ca*x1+2.0*da))-
			(0.125*(3.0*aa+2.0*ca+4.0*e)*boost::math::erf(x0)-1.0/(4.0*M_SQRTPI)*exp(-x0*x0)*(2.0*aa*x0*x0*x0+3.0*aa*x0+2.0*ba*(x0*x0+1.0)+2.0*ca*x0+2.0*da));
        #endif
	}

	const Real Gsr::gaussianShiftedPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real h, const Real x0, const Real x1) const {
		return gaussianPolynomialIntegral(a,-4.0*a*h+b,6.0*a*h*h-3.0*b*h+c,-4*a*h*h*h+3.0*b*h*h-2.0*c*h+d,a*h*h*h*h-b*h*h*h+c*h*h-d*h+e,x0,x1);
	}



}

