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

#include <markovfunctional.hpp>

namespace QuantLib {

    MarkovFunctional::MarkovFunctional(const Handle<YieldTermStructure>& termStructure,
						const Real reversion,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Handle<SwaptionVolatilityStructure>& swaptionVol,
						const std::vector<Date>& swaptionExpiries,
						const std::vector<Period>& swaptionTenors,
						const boost::shared_ptr<SwapIndex>& swapIndexBase,
						const int xGridPoints, const Real xStdDevs, const int gaussHermitePoints,
						const Real digitalGap, const int arbitrageCheckGridPoints, const Real rateCutLeft, const Real rateCutRight,
						const Real upperRateBound, const bool adjustDigitals, const bool forceExactYtsFit, const int digitalInterpolationDegree) :
	  CalibratedModel(1), TermStructureConsistentModel(termStructure), capletCalibrated_(false),
	  reversion_(ConstantParameter(reversion, NoConstraint()/*,PositiveConstraint()*/)),sigma_(arguments_[0]), volatilities_(volatilities),
	  volstepdates_(volstepdates), swaptionVol_(swaptionVol), swaptionExpiries_(swaptionExpiries), swaptionTenors_(swaptionTenors),
	  swapIndexBase_(swapIndexBase), capletVol_(Handle<OptionletVolatilityStructure>()), capletExpiries_(std::vector<Date>()),
	  modelSettings_(xGridPoints,xStdDevs,gaussHermitePoints,digitalGap,arbitrageCheckGridPoints,rateCutLeft,rateCutRight,upperRateBound,adjustDigitals,forceExactYtsFit,digitalInterpolationDegree)
	  {

	    QL_REQUIRE(volstepdates.size()==volatilities.size(),"number of volstepdates (" << volstepdates.size() << ") is different from number of volatilities (" << volatilities.size() << ")");
		QL_REQUIRE(volstepdates.size()>0,"at least one volstepdate must be given");
		QL_REQUIRE(swaptionExpiries.size()==swaptionTenors.size(),"number of swaption expiries (" << swaptionExpiries.size() << ") is differnt from number of swaption tenors (" << swaptionTenors.size() << ")");
		QL_REQUIRE(swaptionExpiries.size()>=1,"need at least one swaption expiry to calibrate numeraire");
		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		QL_REQUIRE(!swaptionVol.empty(),"swaption volatility structure is empty");

		initialize();

    }

	MarkovFunctional::MarkovFunctional(const Handle<YieldTermStructure>& termStructure,
						const Real reversion,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Handle<OptionletVolatilityStructure>& capletVol,
						const std::vector<Date>& capletExpiries,
						const boost::shared_ptr<IborIndex>& iborIndex,
						const int xGridPoints, const Real xStdDevs, const int gaussHermitePoints,
						const Real digitalGap, const int arbitrageCheckGridPoints, const Real rateCutLeft, const Real rateCutRight,
						const Real upperRateBound, const bool adjustDigitals, const bool forceExactYtsFit, const int digitalInterpolationDegree) :

	  CalibratedModel(1), TermStructureConsistentModel(termStructure), capletCalibrated_(true),
	  reversion_(ConstantParameter(reversion, NoConstraint()/*,PositiveConstraint()*/)),sigma_(arguments_[0]), volatilities_(volatilities),
	  volstepdates_(volstepdates), swaptionVol_(Handle<SwaptionVolatilityStructure>()), swaptionExpiries_(std::vector<Date>()), swaptionTenors_(std::vector<Period>()),
	  iborIndex_(iborIndex), capletVol_(capletVol), capletExpiries_(std::vector<Date>()),
	  modelSettings_(xGridPoints,xStdDevs,gaussHermitePoints,digitalGap,arbitrageCheckGridPoints,rateCutLeft,rateCutRight,upperRateBound,adjustDigitals,forceExactYtsFit,digitalInterpolationDegree)
	  {

	    QL_REQUIRE(volstepdates.size()==volatilities.size(),"number of volstepdates (" << volstepdates.size() << ") is different from number of volatilities (" << volatilities.size() << ")");
		QL_REQUIRE(volstepdates.size()>0,"at least one volstepdate must be given");
		QL_REQUIRE(capletExpiries.size()>=1,"need at least one caplet expiry to calibrate numeraire");
		QL_REQUIRE(!termStructure.empty(),"yield term structure handle is empty");
		QL_REQUIRE(!capletVol.empty(),"caplet volatility structure is empty");

		initialize();

    }

	void MarkovFunctional::initialize() {

		cpuTimer_.start();

		modelSettings_.validate();

		gaussHermite_ = boost::shared_ptr<GaussianQuadrature>(new GaussHermiteIntegration(modelSettings_.gaussHermitePoints_));

		if(capletCalibrated_)
			ytsLinkedIborIndex_ = iborIndex_->clone(termStructure());
		else
			ytsLinkedIborIndex_ = swapIndexBase_->iborIndex()->clone(termStructure());

		volsteptimes_.clear();
		volsteptimesArray_=Array(volstepdates_.size());
		int j=0;
		for(std::vector<Date>::const_iterator i = volstepdates_.begin(); i!= volstepdates_.end(); i++, j++) {
			volsteptimes_.push_back(termStructure()->timeFromReference(*i));
			volsteptimesArray_[j]=volsteptimes_[j];
		}

		std::vector<Date>::const_iterator i;
		if(capletCalibrated_) {
			for(i = capletExpiries_.begin() ; i != capletExpiries_.end() ; i++) {
				makeCapletCalibrationPoint(*i);
			}
		}
		else {
			std::vector<Period>::const_iterator j;
			for(i = swaptionExpiries_.begin(), j = swaptionTenors_.begin() ; i != swaptionExpiries_.end() ; i++,j++) {
				makeSwaptionCalibrationPoint(*i,*j);
			}
		}

		bool done;
		numeraireDate_ = Date::minDate();
		do {
			Date numeraireKnown = numeraireDate_;
			done=true;
			for(std::map<Date,CalibrationPoint>::reverse_iterator i = calibrationPoints_.rbegin(); i != calibrationPoints_.rend() && done; i++) {
				//std::cout << "point " << i->expiry << "/" << i->tenor << std::endl;
				if(i->second.paymentDates_.back() > numeraireDate_) {
					  numeraireDate_ = i->second.paymentDates_.back();
					  numeraireKnown = i->second.paymentDates_.back();
					  if(i != calibrationPoints_.rbegin()) {
						  done=false;
					  }
					  //std::cout << "   new num date " << numeraireDate_ << std::endl;
				}
				for(std::vector<Date>::const_reverse_iterator j = i->second.paymentDates_.rbegin(); j != i->second.paymentDates_.rend() && done; j++) {
					//std::cout << "   payment date " << (*j) << std::endl;
					if(*j < numeraireKnown) {
						if(capletCalibrated_) {
							makeCapletCalibrationPoint(*j);
							done = false;
							break;
						}
						else {
							UpRounding rounder(0);
							//makeSwaptionCalibrationPoint(*j,Period(static_cast<Integer>(rounder(termStructure()->dayCounter().yearFraction(*j,numeraireKnown)*12.0)), Months));
							makeSwaptionCalibrationPoint(*j,Period(static_cast<Integer>(rounder((swapIndexBase_->dayCounter().yearFraction(*j,numeraireKnown)-0.5/365)*12.0)), Months));
							done = false;
							//Period tmp(static_cast<Integer>(rounder((swapIndexBase_->dayCounter().yearFraction(*j,numeraireKnown)-0.5/365)*12.0)), Months);
							//std::cout << "      new point " << (*j) << "/" << tmp << std::endl;
							break;
						}
					}
				}
				if(done) {
					numeraireKnown = i->first;
				}
			}
		} while(!done);

		numeraireTime_ = termStructure()->timeFromReference(numeraireDate_);

		times_.clear();
		times_.push_back(0.0);

		modelOutputs_.expiries_.clear();
		modelOutputs_.tenors_.clear();
		for(std::map<Date,CalibrationPoint>::iterator k=calibrationPoints_.begin(); k != calibrationPoints_.end() ; k++) {
			times_.push_back(termStructure()->timeFromReference(k->first));
			modelOutputs_.expiries_.push_back(k->first);
			modelOutputs_.tenors_.push_back(k->second.tenor_);
		}
		times_.push_back(numeraireTime_);

		double h=modelSettings_.xStdDevs_/((double)modelSettings_.xGridPoints_);
		for(int k=-modelSettings_.xGridPoints_;k<=modelSettings_.xGridPoints_;k++) {
			y_.push_back(h*k);
		}

		for(unsigned int k=0;k<times_.size();k++) {
			boost::shared_ptr<std::vector<Real>> tmpD(new std::vector<Real>(y_.size(),1.0));
			boost::shared_ptr<Interpolation> tmpI(new LogLinearInterpolation(y_.begin(),y_.end(),tmpD->begin())); //FIXME Should we use a different interpolation method here?
			tmpI->enableExtrapolation(); //FIXME Use Flat Extrapolation (this is implemented somewhat below instead at the moment)
			tmpI->update();
			numeraireDiscretization_.push_back(tmpD);
			numeraire_.push_back(tmpI);
		}

		sigma_ = PiecewiseConstantParameter(volsteptimes_);   //FIXME sigma should have a positive constraint
		for(unsigned int i=0;i<sigma_.size();i++) {
			sigma_.setParam(i,volatilities_[i<sigma_.size()-1?i:i-1]); // after last volsteptime extrapolate flat with the last value
		}
		stateProcess_ = boost::shared_ptr<MfStateProcess>(new MfStateProcess(reversion_(0.0),volsteptimesArray_,sigma_.params()));

		cpuTimer_.stop();
		std::cout << "MF model initialize: " << boost::timer::format(cpuTimer_.elapsed()) << std::endl;
		
		generateArguments();
        registerWith(termStructure());
		if(!swaptionVol_.empty()) registerWith(swaptionVol_);
		if(!capletVol_.empty()) registerWith(capletVol_);

	}

	void MarkovFunctional::makeSwaptionCalibrationPoint(const Date& expiry, const Period& tenor) {

		QL_REQUIRE(calibrationPoints_.count(expiry)==0, "swaption expiry (" << expiry << ") occurs more than once in calibration set");
		
		CalibrationPoint p;
		p.isCaplet_ = false;
		//p.expiry_ = expiry;
		p.tenor_ = tenor;
		
		// the swaption we can actually calibrate the numeraire to has to have 0 fixing days
		SwapIndex tmpIndex(swapIndexBase_->familyName(),tenor,0/*swapIndexBase_->fixingDays()*/, 
                            swapIndexBase_->currency(), swapIndexBase_->fixingCalendar(), swapIndexBase_->fixedLegTenor(),
                            swapIndexBase_->fixedLegConvention(), swapIndexBase_->dayCounter(),
                            ytsLinkedIborIndex_);
		boost::shared_ptr<VanillaSwap> underlying = tmpIndex.underlyingSwap(expiry);
		Schedule sched = underlying->fixedSchedule();
		Calendar cal = sched.calendar();
		BusinessDayConvention bdc = underlying->paymentConvention();
		
		std::vector<Date> paymentDates;
		std::vector<Real> yearFractions;
		for(unsigned int k=1; k<sched.size(); k++) {
			yearFractions.push_back(swapIndexBase_->dayCounter().yearFraction(sched.date(k-1),sched.date(k)));
			paymentDates.push_back(cal.adjust(sched.date(k),bdc));
		}
		p.yearFractions_ = yearFractions;
		p.paymentDates_ = paymentDates;
		calibrationPoints_[expiry]=p;

	}

	void MarkovFunctional::makeCapletCalibrationPoint(const Date& expiry) {

		QL_REQUIRE(calibrationPoints_.count(expiry)==0, "caplet expiry (" << expiry << ") occurs more than once in calibration set");

		CalibrationPoint p;
		p.isCaplet_ = true;
		//p.expiry_ = expiry;
		p.tenor_ = ytsLinkedIborIndex_->tenor();
		std::vector<Date> paymentDates;
		std::vector<Real> yearFractions;
		Date valueDate = expiry; //ytsLinkedIborIndex_->valueDate(expiry); // see the comment in makeSwaptionCalibrationPoint
		Date endDate = ytsLinkedIborIndex_->fixingCalendar().advance(valueDate,ytsLinkedIborIndex_->tenor(),
			ytsLinkedIborIndex_->businessDayConvention(),ytsLinkedIborIndex_->endOfMonth()); // FIXME Here we should use the calculation date calendar ?
		paymentDates.push_back(endDate);
		yearFractions.push_back(ytsLinkedIborIndex_->dayCounter().yearFraction(valueDate,endDate));
		p.yearFractions_ = yearFractions;
		p.paymentDates_ = paymentDates;
		calibrationPoints_[expiry]=p;

	}

    void MarkovFunctional::generateArguments() {
        updateNumeraireTabulation();
    }

	void MarkovFunctional::updateNumeraireTabulation() {

		modelOutputs_.adjustmentFactors_.clear();
		modelOutputs_.digitalsAdjustmentFactors_.clear();
		modelOutputs_.afLeftBounds_.clear();
		modelOutputs_.afRightBounds_.clear();

		int idx = times_.size()-2;
		std::vector<Real> deflatedAnnuities(y_.size(),0.0);
		std::vector<Real> deflatedFinalPayments(y_.size(),0.0);

		cpuTimer_.start();

		for(std::map<Date,CalibrationPoint>::reverse_iterator i = calibrationPoints_.rbegin(); i != calibrationPoints_.rend(); i++,idx--) {

			std::cout << "calibration point " << (i->first) << "/" << (i->second.tenor_) << std::endl;

			if(i->second.isCaplet_) {
				i->second.annuity_= i->second.yearFractions_[0] * termStructure()->discount(i->second.paymentDates_[0],true);
				i->second.atm_=ytsLinkedIborIndex_->fixing(i->first);
				i->second.smileSection_=capletVol_->smileSection(i->first,true);
			}
			else {
				SwapIndex tmpIndex(swapIndexBase_->familyName(),i->second.tenor_,0/*swapIndexBase_->fixingDays()*/, 
                            swapIndexBase_->currency(), swapIndexBase_->fixingCalendar(), swapIndexBase_->fixedLegTenor(),
                            swapIndexBase_->fixedLegConvention(), swapIndexBase_->dayCounter(),
                            ytsLinkedIborIndex_);
				Real annuity=0.0;
				for(unsigned int k=0; k<i->second.paymentDates_.size(); k++) {
					annuity += i->second.yearFractions_[k] * termStructure()->discount(i->second.paymentDates_[k],true);
				}
				i->second.annuity_=annuity;
				i->second.atm_=tmpIndex.fixing(i->first);
				i->second.smileSection_=swaptionVol_->smileSection(i->first,i->second.tenor_,true);
			}
			arbitrageFreeStrikeRange(i->first,i->second);
			modelOutputs_.afLeftBounds_.insert(modelOutputs_.afLeftBounds_.begin(),i->second.afLeftBound_);
			modelOutputs_.afRightBounds_.insert(modelOutputs_.afRightBounds_.begin(),i->second.afRightBound_);


			Real stdDev=stateProcess_->stdDeviation(0.0,0.0,times_[idx]);
			Real numeraire0=numeraire(0.0);

			for(int j=y_.size()-1;j>=0;j--) {
				Real tmpAnn=0.0, tmpFnl;
				for(unsigned int k=0;k<i->second.paymentDates_.size();k++) {
					tmpFnl=deflatedZerobond(termStructure()->timeFromReference(i->second.paymentDates_[k]),times_[idx],y_[j]*stdDev);
					tmpAnn+=tmpFnl*i->second.yearFractions_[k];
				}
				deflatedAnnuities[j] = tmpAnn;
				deflatedFinalPayments[j] = tmpFnl;
			}

			Real digitalsCorrectionFactor = 1.0;
			modelOutputs_.digitalsAdjustmentFactors_.insert(modelOutputs_.digitalsAdjustmentFactors_.begin(),digitalsCorrectionFactor);

			Real digital;
			int i0,i1,i2,i3,i4;
			Real x0,x1,x2,x3,x4,r0,r1,r2,r3,r4;
			//Real atmCallPrice,swapRateBefore;

			for(int c=0;c==0 || (c==1 && modelSettings_.adjustDigitals_);c++) {

				if(c==1) {
					digitalsCorrectionFactor = i->second.annuity_ / digital;
					modelOutputs_.digitalsAdjustmentFactors_.front() = digitalsCorrectionFactor;
				}
				
				digital=0.0;
				//atmCallPrice=0.0;
				for(int j=y_.size()-1;j>=0;j--) {
				
					if(modelSettings_.digitalInterpolationDegree_==0) {
						 Real dp = ((j==y_.size()-1 ? 1.0 : cnd_(y_[j+1]))-cnd_(y_[j]));
						 //digital+= dp * deflatedAnnuities[j] * numeraire0 * digitalsCorrectionFactor;
						 digital+= gaussianPolynomialIntegral(0.0,0.0,0.0,0.0,deflatedAnnuities[j],
																y_[j]/M_SQRT2,(j<y_.size()-1 ? y_[j+1] : 100.0) /M_SQRT2)*numeraire0* digitalsCorrectionFactor;
					}

					if(modelSettings_.digitalInterpolationDegree_==1) {
						if(j>0) {
							i0=j;i1=j-1;
						}
						if(j==0) {
							i0=1;i1=0;
						}
						x0=y_[i0]/M_SQRT2;x1=y_[i1]/M_SQRT2;
						r0=deflatedAnnuities[i0]; r1=deflatedAnnuities[i1];
						digital+= gaussianPolynomialIntegral(0.0,0.0,0.0,r1/(x1-x0)+r0/(x0-x1),-x0*r1/(x1-x0)-x1*r0/(x0-x1),
																y_[j]/M_SQRT2,(j<y_.size()-1 ? y_[j+1] : 100.0) /M_SQRT2)*numeraire0* digitalsCorrectionFactor;
					}

					if(modelSettings_.digitalInterpolationDegree_==2) {
						if(j==y_.size()-1) {
							i0=j;i1=j-1;i2=j-2;
						}
						if(j<=y_.size()-2 && j>=1) {
							i0=j+1;i1=j;i2=j-1;
						}
						if(j==0) {
							i0=j+2;i1=j+1;i2=j;
						}
						x0=y_[i0]/M_SQRT2;x1=y_[i1]/M_SQRT2;x2=y_[i2]/M_SQRT2;
						r0=deflatedAnnuities[i0]; r1=deflatedAnnuities[i1]; r2=deflatedAnnuities[i2];
						digital+= gaussianPolynomialIntegral(0.0,0.0,r2/((x2-x0)*(x2-x1))+r1/((x1-x0)*(x1-x2))+r0/((x0-x1)*(x0-x2)),
												r2*(-x1-x0)/((x2-x0)*(x2-x1))+r1*(-x0-x2)/((x1-x0)*(x1-x2))+r0*(-x1-x2)/((x0-x1)*(x0-x2)),
												r2*x0*x1/((x2-x0)*(x2-x1))+r1*x0*x2/((x1-x0)*(x1-x2))+r0*x1*x2/((x0-x1)*(x0-x2)),
													y_[j]/M_SQRT2,(j<y_.size()-1 ? y_[j+1] : 100.0)/M_SQRT2)*numeraire0* digitalsCorrectionFactor;
					}

					if(modelSettings_.digitalInterpolationDegree_==3) {
						if(j==y_.size()-1) {
							i0=j;i1=j-1;i2=j-2;i3=j-3;
						}
						if(j<=y_.size()-2 && j>=2) {
							i0=j+1;i1=j;i2=j-1;i3=j-2;
						}
						if(j==1) {
							i0=j+2;i1=j+1;i2=j;i3=j-1;
						}
						if(j==0) {
							i0=j+3;i1=j+2;i2=j+1;i3=j;
						}
						x0=y_[i0]/M_SQRT2;x1=y_[i1]/M_SQRT2;x2=y_[i2]/M_SQRT2;x3=y_[i3]/M_SQRT2;
						r0=deflatedAnnuities[i0]; r1=deflatedAnnuities[i1]; r2=deflatedAnnuities[i2]; r3=deflatedAnnuities[i3];

						digital+=gaussianPolynomialIntegral(0.0,r3/((x3-x0)*(x3-x1)*(x3-x2))+r2/((x2-x0)*(x2-x1)*(x2-x3))+r1/((x1-x0)*(x1-x2)*(x1-x3))+r0/((x0-x1)*(x0-x2)*(x0-x3)),
								-r3*(x0+x1+x2)/((x3-x0)*(x3-x1)*(x3-x2))-r2*(x0+x1+x3)/((x2-x0)*(x2-x1)*(x2-x3))-r1*(x0+x2+x3)/((x1-x0)*(x1-x2)*(x1-x3))-
									r0*(x1+x2+x3)/((x0-x1)*(x0-x2)*(x0-x3)),
								r3*(x0*x1+x0*x2+x1*x2)/((x3-x0)*(x3-x1)*(x3-x2))+r2*(x0*x1+x0*x3+x1*x3)/((x2-x0)*(x2-x1)*(x2-x3))+r1*(x0*x2+x0*x3+x2*x3)/((x1-x0)*(x1-x2)*(x1-x3))+
									r0*(x1*x2+x1*x3+x2*x3)/((x0-x1)*(x0-x2)*(x0-x3)),
								-r3*(x0*x1*x2)/((x3-x0)*(x3-x1)*(x3-x2))-r2*(x0*x1*x3)/((x2-x0)*(x2-x1)*(x2-x3))-r1*(x0*x2*x3)/((x1-x0)*(x1-x2)*(x1-x3))-
									r0*(x1*x2*x3)/((x0-x1)*(x0-x2)*(x0-x3)),y_[j]/M_SQRT2,(j<y_.size()-1 ? y_[j+1] : 100.0)/M_SQRT2)*numeraire0* digitalsCorrectionFactor;
					}

					if(modelSettings_.digitalInterpolationDegree_==4) {
						if(j==y_.size()-1) {
							i0=j;i1=j-1;i2=j-2;i3=j-3;i4=j-4;
						}
						if(j==y_.size()-2) {
							i0=j+1;i1=j;i2=j-1;i3=j-2;i4=j-3;
						}
						if(j<=y_.size()-3 && j>=2) {
							i0=j+2;i1=j+1;i2=j;i3=j-1;i4=j-2;
						}
						if(j==1) {
							i0=j+3;i1=j+2;i2=j+1;i3=j;i4=j-1;
						}
						if(j==0) {
							i0=j+4;i1=j+3;i2=j+2;i3=j+1;i4=j;
						}
						x0=y_[i0]/M_SQRT2; x1=y_[i1]/M_SQRT2; x2=y_[i2]/M_SQRT2; x3=y_[i3]/M_SQRT2; x4=y_[i4]/M_SQRT2;
						r0=deflatedAnnuities[i0]; r1=deflatedAnnuities[i1]; r2=deflatedAnnuities[i2]; r3=deflatedAnnuities[i3]; r4=deflatedAnnuities[i4];
						digital+=gaussianPolynomialIntegral(r4/((x4-x0)*(x4-x1)*(x4-x2)*(x4-x3))+
												r3/((x3-x0)*(x3-x1)*(x3-x2)*(x3-x4))+
												r2/((x2-x0)*(x2-x1)*(x2-x3)*(x2-x4))+
												r1/((x1-x0)*(x1-x2)*(x1-x3)*(x1-x4))+
												r0/((x0-x1)*(x0-x2)*(x0-x3)*(x0-x4)),
												-r4*(x0+x1+x2+x3)/((x4-x0)*(x4-x1)*(x4-x2)*(x4-x3))
												-r3*(x0+x1+x2+x4)/((x3-x0)*(x3-x1)*(x3-x2)*(x3-x4))
												-r2*(x0+x1+x3+x4)/((x2-x0)*(x2-x1)*(x2-x3)*(x2-x4))
												-r1*(x0+x2+x3+x4)/((x1-x0)*(x1-x2)*(x1-x3)*(x1-x4))
												-r0*(x1+x2+x3+x4)/((x0-x1)*(x0-x2)*(x0-x3)*(x0-x4)),
											+r4*(x0*x1+x0*x2+x0*x3+x1*x2+x1*x3+x2*x3)/((x4-x0)*(x4-x1)*(x4-x2)*(x4-x3))
											+r3*(x0*x1+x0*x2+x0*x4+x1*x2+x1*x4+x2*x4)/((x3-x0)*(x3-x1)*(x3-x2)*(x3-x4))
											+r2*(x0*x1+x0*x3+x0*x4+x1*x3+x1*x4+x3*x4)/((x2-x0)*(x2-x1)*(x2-x3)*(x2-x4))
											+r1*(x0*x2+x0*x3+x0*x4+x2*x3+x2*x4+x3*x4)/((x1-x0)*(x1-x2)*(x1-x3)*(x1-x4))
											+r0*(x1*x2+x1*x3+x1*x4+x2*x3+x2*x4+x3*x4)/((x0-x1)*(x0-x2)*(x0-x3)*(x0-x4)),
												-r4*(x0*x1*x2+x0*x1*x3+x0*x2*x3+x1*x2*x3)/((x4-x0)*(x4-x1)*(x4-x2)*(x4-x3))
												-r3*(x0*x1*x2+x0*x1*x4+x0*x2*x4+x1*x2*x4)/((x3-x0)*(x3-x1)*(x3-x2)*(x3-x4))
												-r2*(x0*x1*x3+x0*x1*x4+x0*x3*x4+x1*x3*x4)/((x2-x0)*(x2-x1)*(x2-x3)*(x2-x4))
												-r1*(x0*x2*x3+x0*x2*x4+x0*x3*x4+x2*x3*x4)/((x1-x0)*(x1-x2)*(x1-x3)*(x1-x4))
												-r0*(x1*x2*x3+x1*x2*x4+x1*x3*x4+x2*x3*x4)/((x0-x1)*(x0-x2)*(x0-x3)*(x0-x4)),
											r4*(x0*x1*x2*x3)/((x4-x0)*(x4-x1)*(x4-x2)*(x4-x3))+
											r3*(x0*x1*x2*x4)/((x3-x0)*(x3-x1)*(x3-x2)*(x3-x4))+
											r2*(x0*x1*x3*x4)/((x2-x0)*(x2-x1)*(x2-x3)*(x2-x4))+
											r1*(x0*x2*x3*x4)/((x1-x0)*(x1-x2)*(x1-x3)*(x1-x4))+
											r0*(x1*x2*x3*x4)/((x0-x1)*(x0-x2)*(x0-x3)*(x0-x4)),y_[j]/M_SQRT2,(j<y_.size()-1 ? y_[j+1] : 100.0)/M_SQRT2)*numeraire0;
					}
			
					if(digital<0.0) { 
						//std::cout << "WARNING time " << times_[idx] << " y " << y_[j] << " digitalPrice is negative: " << digital << std::endl;
						digital=0.0;
					}
					if(digital>i->second.annuity_) {
						//std::cout << "WARNING time " << times_[idx] << " y " << y_[j] << " digitalPrice is above annuity: " << digital << std::endl;
						digital=i->second.annuity_;
					}
					Real swapRate = marketSwapRate(i->first,i->second,digital);
					Real numeraire = 1.0 / (swapRate*deflatedAnnuities[j]+deflatedFinalPayments[j]);
					(*numeraireDiscretization_[idx])[j] = numeraire * termStructure()->discount(times_[idx],true) / termStructure()->discount(numeraireTime_,true);
					//if(j<y_.size()-1 && swapRate>=i->atm) atmCallPrice+=digital*(swapRateBefore-swapRate);
					//swapRateBefore=swapRate;
					//std::cout << times_[idx] << ";" << y_[j] << ";" << swapRate << ";" << digital << ";" << i->annuity << ";" << 0.0/*atmCallPrice*/ << std::endl;
				}
			}
			//std::cout << "------------------------------------------------------------------------------------------------------------------" << std::endl;
			if(modelSettings_.forceExactYtsFit_) {
				numeraire_[idx]->update();
				// adjust discount factor
				Real modelDeflatedZerobond = deflatedZerobond(times_[idx],0.0);
				Real marketDeflatedZerobond = termStructure()->discount(times_[idx],true) / termStructure()->discount(numeraireTime_,true);
				for(int j=y_.size()-1;j>=0;j--) {
					(*numeraireDiscretization_[idx])[j] *= modelDeflatedZerobond/marketDeflatedZerobond;
				}
				modelOutputs_.adjustmentFactors_.insert(modelOutputs_.adjustmentFactors_.begin(),modelDeflatedZerobond/marketDeflatedZerobond);
			}
			else {
				modelOutputs_.adjustmentFactors_.insert(modelOutputs_.adjustmentFactors_.begin(),1.0);
			}
			numeraire_[idx]->update();
		}

		cpuTimer_.stop();
		std::cout << "MF model updateNumeraireTabulation: " << boost::timer::format(cpuTimer_.elapsed()) << std::endl;
		std::cout << "           thereof vol retrieval  : " << boost::timer::format(cpuTimer2_.elapsed()) << std::endl;


		// DEBUG output numeraire matrix to file
		/*FILE* out = fopen("num.txt","w");
		for(int x=0;x<numeraireDiscretization_.size();x++) {
			for(int y=0;y<numeraireDiscretizations_[x]->size();y++) {
				fprintf(out,"%1.6f %1.12f %1.12f\n",times_[x],x_[y],(*numeraireDiscretization_[x])[y]);
			}
			fprintf(out,"\n");
		}
		fclose(out);*/
	}

	Real MarkovFunctional::deflatedZerobond(Time T, Time t, Real x) const {

		Real stdDev = stateProcess_->stdDeviation(t,x,T-t);
		const Array& z=gaussHermite_->x();
		const Array& w=gaussHermite_->weights();
		Real deflZero=0.0;
		for(int i=0;i<modelSettings_.gaussHermitePoints_;i++) {
			deflZero+=1.0/numeraire(T,stdDev*z[i]*M_SQRT2+x)*w[i]*exp(-z[i]*z[i]);
		}
		return sqrt(1.0/M_PI)*deflZero;

	}
	
	Real MarkovFunctional::zerobond(Time T, Time t, Real x) const {

		return deflatedZerobond(T,t,x)*numeraire(t,x);

	}

	Real MarkovFunctional::zerobond(const Date& maturity, const Date& referenceDate, const Real x) const {

		return zerobond(termStructure()->timeFromReference(maturity),
			referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate),x);

	}

	Real MarkovFunctional::numeraire(Time t, Real x) const {

		if(t<QL_EPSILON) return termStructure()->discount(numeraireTime_,true);
		unsigned int i=0; 
		if(t>=times_.back()) i=times_.size()-1;
		else i=std::upper_bound(times_.begin(),times_.end()-1,t)-times_.begin();
		//while(t>=times_[i] && i<times_.size()-1) i++; // FIXME here we could search more efficiently using binary search
		Real y = x / stateProcess_->stdDeviation(0.0,0.0,t);
		if(y<y_.front()) y=y_.front(); // FIXME flat extrapolation should be incoperated into interpolation object, see above
		if(y>y_.back()) y=y_.back();
		Real inverseNormalization=termStructure()->discount(numeraireTime_,true)/termStructure()->discount(t,true);
		if(i==times_.size()) return inverseNormalization*(*numeraire_[i-1])(y);
		Real na= (*numeraire_[i-1])(y);
		Real nb = (*numeraire_[i])(y);
		Real ta = times_[i-1];
		Real tb = times_[i];
		Real dt = tb-ta;
		//std::cout << "   numeraire t=" << t << " y=" << y << " (i=" << i << ")*" << (t-ta)/dt << " (i-1)*" << (tb-t)/dt << "=" << exp ( (t-ta)/dt * log(yb) + (tb-t)/dt * log(ya) ) * inverseNormalization << std::endl;
		//return inverseNormalization*( (t-ta)/dt * nb + (tb-t)/dt * na ); // linear in numeraire
		return inverseNormalization* exp ( (t-ta)/dt * log(nb) + (tb-t)/dt * log(na) ); // linear in log(numeraire)

	}

	Real MarkovFunctional::forwardRate(const Date& fixing, const Date& referenceDate, const Real x) const {

		std::vector<Date> paymentDates;
		paymentDates.push_back(ytsLinkedIborIndex_->fixingCalendar().advance(ytsLinkedIborIndex_->valueDate(fixing),ytsLinkedIborIndex_->tenor(),ytsLinkedIborIndex_->businessDayConvention(),ytsLinkedIborIndex_->endOfMonth()));
		Real annuity = ytsLinkedIborIndex_->dayCounter().yearFraction(ytsLinkedIborIndex_->valueDate(fixing),paymentDates.back()) * termStructure()->discount(paymentDates.back());
		Rate atm = ( zerobond(ytsLinkedIborIndex_->valueDate(fixing),referenceDate,x) - zerobond(paymentDates.back(),referenceDate,x) ) / annuity;
		return atm;

	}

	Real MarkovFunctional::swapRate(const Date& fixing, const Period& tenor, const Date& referenceDate, const Real x) const {

		SwapIndex swapIdx = SwapIndex(swapIndexBase_->familyName(), tenor, swapIndexBase_->fixingDays(),
                                 swapIndexBase_->currency(), swapIndexBase_->fixingCalendar(), swapIndexBase_->fixedLegTenor(),
                                 swapIndexBase_->fixedLegConvention(), swapIndexBase_->dayCounter(), ytsLinkedIborIndex_);
		boost::shared_ptr<VanillaSwap> underlying = swapIdx.underlyingSwap(fixing);
		Schedule sched = underlying->fixedSchedule();
		Real annuity = swapAnnuity(fixing,tenor,referenceDate,x);
		Rate atm = ( zerobond(sched.dates().front(),referenceDate,x) - zerobond(sched.dates().back(),referenceDate,x) ) / annuity;
		return atm;

	}

	Real MarkovFunctional::swapAnnuity(const Date& fixing, const Period& tenor, const Date& referenceDate, const Real x) const {

		SwapIndex swapIdx = SwapIndex(swapIndexBase_->familyName(), tenor, swapIndexBase_->fixingDays(),
                                 swapIndexBase_->currency(), swapIndexBase_->fixingCalendar(), swapIndexBase_->fixedLegTenor(),
                                 swapIndexBase_->fixedLegConvention(), swapIndexBase_->dayCounter(), ytsLinkedIborIndex_);
		boost::shared_ptr<VanillaSwap> underlying = swapIdx.underlyingSwap(fixing);
		Schedule sched = underlying->fixedSchedule();
		BusinessDayConvention bdc = underlying->paymentConvention();
		Calendar cal = sched.calendar();

		Real annuity=0.0;
		for(unsigned int j=1; j<sched.size(); j++) {
			annuity += zerobond(cal.adjust(sched.date(j),bdc),referenceDate,x) * 
				swapIndexBase_->dayCounter().yearFraction(sched.date(j-1),sched.date(j));
		}
		return annuity;

	}

	Real MarkovFunctional::zerobondOption(const Option::Type& type, const Date& expiry, const Date& maturity, const Rate strike, const Date& referenceDate, const Real x) const {

		const Array& z=gaussHermite_->x();
		const Array& w=gaussHermite_->weights();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Real stdDev = stateProcess_->stdDeviation(referenceTime,x,fixingTime-referenceTime);
		Real price=0.0;
		for(unsigned int i=0;i<z.size();i++) {
			Real discount = zerobond(maturity,expiry,x+z[i]*stdDev*M_SQRT2);
			price+= std::max((type == Option::Call ? 1.0 : -1.0) * (discount-strike), 0.0 ) / numeraire(fixingTime,x+z[i]*stdDev*M_SQRT2) * w[i] * exp(-z[i]*z[i]);
		}
		price *= numeraire(referenceTime,x) / sqrt(M_PI);
		return price;

	}

	Real MarkovFunctional::swaptionPrice(const Option::Type& type, const Date& expiry, const Period& tenor, const Rate strike, const Date& referenceDate, const Real x) const {
		
		const Array& z=gaussHermite_->x();
		const Array& w=gaussHermite_->weights();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Real stdDev = stateProcess_->stdDeviation(referenceTime,x,fixingTime-referenceTime);
		Real price=0.0;
		for(unsigned int i=0;i<z.size();i++) {
			Real annuity=swapAnnuity(expiry,tenor,expiry,x+z[i]*stdDev*M_SQRT2);
			Rate atm=swapRate(expiry,tenor,expiry,x+z[i]*stdDev*M_SQRT2);
			price+= annuity * std::max((type == Option::Call ? 1.0 : -1.0) * (atm-strike), 0.0 ) / numeraire(fixingTime,x+z[i]*stdDev*M_SQRT2) * w[i] * exp(-z[i]*z[i]);
		}
		price *= numeraire(referenceTime,x) / sqrt(M_PI);
		return price;

	}

	Real MarkovFunctional::capletPrice(const Option::Type& type, const Date& expiry, const Rate strike, const Date& referenceDate, const Real x) const {

		const Array& z=gaussHermite_->x();
		const Array& w=gaussHermite_->weights();

		Time fixingTime = termStructure()->timeFromReference(expiry);
		Time referenceTime = referenceDate == Null<Date>() ? 0.0 : termStructure()->timeFromReference(referenceDate);

		Real stdDev = stateProcess_->stdDeviation(referenceTime,x,fixingTime-referenceTime);
		Real price=0.0;
		for(unsigned int i=0;i<z.size();i++) {
			Real annuity=zerobond(ytsLinkedIborIndex_->fixingCalendar().advance(ytsLinkedIborIndex_->valueDate(expiry),ytsLinkedIborIndex_->tenor(),ytsLinkedIborIndex_->businessDayConvention(),ytsLinkedIborIndex_->endOfMonth()),
				referenceDate,x+z[i]*stdDev*M_SQRT2);
			Rate atm=forwardRate(expiry,referenceDate,x+z[i]*stdDev*M_SQRT2);
			price+= annuity * std::max((type == Option::Call ? 1.0 : -1.0) * (atm-strike), 0.0 ) / numeraire(fixingTime,x+z[i]*stdDev*M_SQRT2) * w[i] * exp(-z[i]*z[i]);
		}
		price *= numeraire(referenceTime,x) / sqrt(M_PI);
		return price;

	}

	void MarkovFunctional::arbitrageFreeStrikeRange(const Date& expiry, CalibrationPoint& p) const {

		//std::cout << "af check " << expiry << "/" << p.tenor_ << std::endl; // test
		//std::cout << "strike;digitalcall" << std::endl;
		//result.first=p.atm_-0.01; result.second=p.atm_+0.01; // test
		         
		Real k = p.atm_;
		Real h = p.atm_ / ((Real)modelSettings_.arbitrageCheckGridPoints_);
		Real p0 = 0.0, p1 = 0.0;
		int iter=0;
		while(/*p1>=p0 && p1>=0.0 &&*/ iter++<modelSettings_.arbitrageCheckGridPoints_) { 
			p0 = p1;
			Real kl=std::max(k-modelSettings_.digitalGap_/2.0,0.0);
			Real kr=kl+modelSettings_.digitalGap_;
			Real stdDev=p.smileSection_->volatility(kl)*sqrt(termStructure()->timeFromReference(expiry));
			Real stdDevG=p.smileSection_->volatility(kr)*sqrt(termStructure()->timeFromReference(expiry));
			p1 = (blackFormula(Option::Call,kl,p.atm_,stdDev,p.annuity_)-blackFormula(Option::Call,kr,p.atm_,stdDevG,p.annuity_)) / modelSettings_.digitalGap_;
			//BlackCalculator c(Option::Call,k,p.atm_,stdDev,p.annuity_);
			//p1 = c.itmCashProbability() * p.annuity_; // fixme plus vega times smile slope ...
			//std::cout << std::setprecision(16) << k << ";" << p1 << std::endl;
			k-=h;
		}
		Real l = std::max(k+ (p1>=p0 ? 0.0 : h) , 0.0); // numerics ...
		Real L = (p1>=p0 ? p1 : p0);

		k = p.atm_;
		h = (modelSettings_.upperRateBound_- p.atm_) / ((Real)modelSettings_.arbitrageCheckGridPoints_);
		p0 = p1 = p.annuity_; 
		iter=0;
		while(p1<=p0 && p1>=0.0 && iter++<modelSettings_.arbitrageCheckGridPoints_) { 
			p0 = p1;
			Real kl=std::max(k-modelSettings_.digitalGap_/2.0,0.0);
			Real kr=kl+modelSettings_.digitalGap_;
			Real stdDev=p.smileSection_->volatility(kl)*sqrt(termStructure()->timeFromReference(expiry));
			Real stdDevG=p.smileSection_->volatility(kr)*sqrt(termStructure()->timeFromReference(expiry));
			p1 = (blackFormula(Option::Call,kl,p.atm_,stdDev,p.annuity_)-blackFormula(Option::Call,kr,p.atm_,stdDevG,p.annuity_)) / modelSettings_.digitalGap_;
			//std::cout << k << ";" << p1 << std::endl;
			k+=h;
		}
		Real r = k- (p1<=p0 ? 0.0 : h);
		Real R = (p1<=p0 ? p1 : p0);

		Real cL = blackFormula(Option::Call,l,p.atm_,p.smileSection_->volatility(l)*sqrt(termStructure()->timeFromReference(expiry)),p.annuity_);
		Real cR = blackFormula(Option::Call,r,p.atm_,p.smileSection_->volatility(r)*sqrt(termStructure()->timeFromReference(expiry)),p.annuity_);
		Real alphaL = l*(p.annuity_-L)/(l*p.annuity_-p.atm_*p.annuity_+cL)-1.0;
		Real betaL = (p.annuity_-L)/std::pow(l,alphaL);
		Real alphaR = R/cR;
		Real betaR = std::log(R)+r*alphaR;

		p.afLeftBound_ = l;
		p.afRightBound_ = r;

		//if(boost::math::isnormal(alphaL) && boost::math::isnormal(betaL) && betaL >= 0 && alphaL >= 0) {
			p.alphaL_ = alphaL;
			p.betaL_ = betaL;
	    //}
		//else {
		//	p.alphaL_ = 0.0;
		//	p.betaL_ = 0.0;
		//}
		//if(boost::math::isnormal(alphaR) && boost::math::isnormal(betaR) && alphaR >= QL_EPSILON) {
			p.alphaR_ = alphaR;
			p.betaR_ = betaR;
		//}
		//else {
			p.alphaR_ = 1.0;
			p.betaR_ = 0.0;
		//}

		std::cout << "alphaL=" << alphaL << " betaL=" << betaL << " IntMatch=" << p.atm_*p.annuity_-cL << std::endl;

		// test: output digital prices
		for(int i=0;i<modelSettings_.arbitrageCheckGridPoints_;i++) {
			Real k = i * p.atm_ / ((Real)modelSettings_.arbitrageCheckGridPoints_);
			std::cout << std::setprecision(16) << k << ";" << marketDigitalPrice(expiry,p,Option::Call,k) << ";" << p.annuity_ << ";" << (k<=l ? "*" : "") << std::endl;
		}
		for(int i=0;i<modelSettings_.arbitrageCheckGridPoints_;i++) {
			Real k = p.atm_ + i*(modelSettings_.upperRateBound_-p.atm_) / ((Real)modelSettings_.arbitrageCheckGridPoints_);
			std::cout << std::setprecision(16) << k << ";" << marketDigitalPrice(expiry,p,Option::Call,k) << ";" << p.annuity_ << ";" << (k>=r ? "*" : "") << std::endl;
		}

	}

	
	Real MarkovFunctional::marketSwapRate(const Date& expiry, const CalibrationPoint& p, const Real digitalPrice) const {

		class ZeroHelper {
			public:
			ZeroHelper(const MarkovFunctional *model, const Date& expiry, const CalibrationPoint& p, const Real marketPrice) :
				model_(model), expiry_(expiry), p_(p), marketPrice_(marketPrice) {}
			double operator()(double strike) const {
				/*Option::Type type = strike >= p_.atm ? Option::Type::Call : Option::Type::Put;
				Real modelPrice = model_->marketDigitalPrice(p_,type,strike);
				if(type == Option::Type::Put) {
					modelPrice = p_.annuity - modelPrice;
				}*/
				//std::cout << "strike = " << strike;
				Real modelPrice = model_->marketDigitalPrice(expiry_,p_,Option::Call,strike);
				//std::cout << " model = " << modelPrice << " market = " << marketPrice_ << std::endl;
				return modelPrice-marketPrice_;
			};
			const MarkovFunctional *model_;
			const Real marketPrice_;
			const Date& expiry_;
			const CalibrationPoint& p_;
		};

		ZeroHelper z(this,expiry,p,digitalPrice);
		Brent b;
		//Real solution = digitalPrice >= p.annuity/2.0 ? 0.0 : modelSettings_.upperRateBound_;
		//try {
		  Real solution=b.solve(z,1E-8,0.05,0.0,modelSettings_.upperRateBound_);
		//} catch(QuantLib::Error e) {}
		return solution;

	}

	Real MarkovFunctional::marketDigitalPrice(const Date& expiry,const CalibrationPoint& p, const Option::Type& type, const Real strike) const {

		QL_REQUIRE(type==Option::Call,"At the moment marketDigitalPrice is only allowed for calls due to the extrapolation formulas!");

		if(strike >= modelSettings_.upperRateBound_) return 0.0;
		if(strike < QL_EPSILON) return p.annuity_; // FIXME sqrt(QL_EPSILON) ?

		Time fixingTime = termStructure()->timeFromReference(expiry);

		Real stdDev, stdDevGap, p0;

		Real leftCut = p.afLeftBound_;
		Real rightCut = p.afRightBound_;

		Real strike0 = strike;
		if(strike < leftCut) strike0=leftCut;
		if(strike > rightCut) strike0=rightCut;

		Real kl=std::max(strike0-modelSettings_.digitalGap_/2.0,0.0);
		Real kr=kl+modelSettings_.digitalGap_;
		cpuTimer2_.resume();
		stdDev=p.smileSection_->volatility(kl)*sqrt(fixingTime);
		stdDevGap=p.smileSection_->volatility(kr)*sqrt(fixingTime);
		cpuTimer2_.stop();

		p0 = (blackFormula(type,kl,p.atm_,stdDev,p.annuity_)-blackFormula(type,kr,p.atm_,stdDevGap,p.annuity_)) / modelSettings_.digitalGap_;
		//BlackCalculator c(type,strike,p.atm,stdDev,p.annuity);
		//p0 = c.itmCashProbability() * p.annuity; // fixme plus vega times smile slope ...
		p0 *= (type == Option::Call ? 1.0 : -1.0);
		p0 = std::max(std::min(p.annuity_,p0),0.0);

		if(strike <= leftCut) {
			//Real boundaryPrice = type == Option::Call ? p.annuity_ : 0.0;
			//p0 = boundaryPrice+(p0-boundaryPrice)/leftCut*strike;
			//return p0;
			return p.annuity_ - p.betaL_*std::pow(strike,p.alphaL_);
		}

		if(strike >= rightCut) {
			//Real boundaryPrice = type == Option::Put ? p.annuity_ : 0.0;
			//p0 = p0+(boundaryPrice-p0)/(modelSettings_.upperRateBound_-rightCut)*(strike-rightCut);
			//return p0;
			return std::exp(-p.alphaR_*strike+p.betaR_);
		}

		return p0;

	}

	Real MarkovFunctional::gaussianPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real x0, const Real x1) {
		ErrorFunction erf;
		return (0.125*(3.0*a+2.0*c+4.0*e)*erf(x1)-1.0/(4.0*sqrt(M_PI))*exp(-x1*x1)*(2.0*a*x1*x1*x1+3.0*a*x1+2.0*b*(x1*x1+1.0)+2.0*c*x1+2.0*d))-
			(0.125*(3.0*a+2.0*c+4.0*e)*erf(x0)-1.0/(4.0*sqrt(M_PI))*exp(-x0*x0)*(2.0*a*x0*x0*x0+3.0*a*x0+2.0*b*(x0*x0+1.0)+2.0*c*x0+2.0*d));
	}

	//bool operator<(const MarkovFunctional::CalibrationPoint& a, const MarkovFunctional::CalibrationPoint& b) {
	//	return a.expiry_ < b.expiry_;
	//}

}

