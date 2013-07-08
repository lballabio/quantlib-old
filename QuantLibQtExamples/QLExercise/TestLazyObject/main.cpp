/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 

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

#include <ql/quantlib.hpp>
#include <boost/math/distributions.hpp>
#include <iostream>

using namespace QuantLib;


Real blackScholesPriceFwd(const Real& fwd, 
							const Real& strike,
							const Volatility& vol,
							const Rate& rd,
							const Rate& rf,
							const Time& tau,
							const Integer& phi){
	boost::math::normal_distribution<> d(0.0,1.0);
	Real dp,dm, stdDev, res, domDf, forDf;

	domDf=std::exp(-rd*tau); forDf=std::exp(-rf*tau);
	stdDev=vol*std::sqrt(tau);

	dp=(std::log(fwd/strike)+0.5*stdDev*stdDev)/stdDev;
	dm=(std::log(fwd/strike)-0.5*stdDev*stdDev)/stdDev;

	res=phi*domDf*(fwd*cdf(d,phi*dp)-strike*cdf(d,phi*dm));
	return res;
}

class SimpleVolQuote: public Quote{
private:
	Volatility vol_;
	Real strike_;

public:
	SimpleVolQuote(Volatility vol, Real strike) : vol_(vol), strike_(strike){}

	bool isValid() const {return vol_!=Null<Real>();}
	Real value() const {return vol_;}
	Real strike()const {return strike_;}

	void setVolatility(const Volatility& vol){
		vol_=vol;
		notifyObservers();	
	}	 
};

class SimpleSmile: public Observable, Observer{
private:
	std::vector<boost::shared_ptr<SimpleVolQuote>> volVec_;
public:
	SimpleSmile(const std::vector<boost::shared_ptr<SimpleVolQuote>>& volVec)
		:volVec_(volVec){
			for(Size i=0;i<volVec_.size();i++){
				registerWith(volVec_[i]);
			}
	}
	void update(){
		notifyObservers();
	}

	std::vector<boost::shared_ptr<SimpleVolQuote>> getVolVec() const {return volVec_;}
	Size getVolNumber() const{return volVec_.size();}
};

class SabrModel: public LazyObject{

public:
	SabrModel(const boost::shared_ptr<SimpleSmile>& smile,
		const Real& fwd, const Time& tau, const Real& rd,const Real& rf)
		:smile_(smile),fwd_(fwd),tau_(tau),rd_(rd),rf_(rf),
		strikeVec_(std::vector<Real>(smile->getVolNumber())),
		volVec_(std::vector<Real>(smile->getVolNumber())){
			// register smile as observable
			registerWith(smile_);
	}

	Real getVanillaPrice(const Real& strike){
		calculate();
		return blackScholesPriceFwd(fwd_,strike,(*intp_)(strike),rd_,rf_,tau_,1);
	}

private:
	void performCalculations() const {
		volQuoteVec_=smile_->getVolVec();	
		for(Size i=0;i< volQuoteVec_.size();++i){
			strikeVec_[i]=(*volQuoteVec_[i]).strike();
			volVec_[i]=(*volQuoteVec_[i]).value();
		}
		if(intp_==NULL){
			intp_.reset(new SABRInterpolation(strikeVec_.begin(), strikeVec_.end(), 
				volVec_.begin(),tau_,fwd_,0.1,0.1,0.1,0.1,false, false, false, false));
		}
		intp_->update();
		std::cout << "\nRecalibration Performed!" << std::endl;
	}

	Real fwd_,rd_,rf_;
	Time tau_;
	boost::shared_ptr<SimpleSmile> smile_;
	mutable boost::shared_ptr<SABRInterpolation> intp_;
	mutable std::vector<Real> strikeVec_,volVec_;
	mutable std::vector<boost::shared_ptr<SimpleVolQuote>> volQuoteVec_;
};


int main() {

	boost::shared_ptr<SimpleVolQuote> v1(new SimpleVolQuote(0.20, 90.0)); 
	boost::shared_ptr<SimpleVolQuote> v2(new SimpleVolQuote(0.194,95.0)); 
	boost::shared_ptr<SimpleVolQuote> v3(new SimpleVolQuote(0.187,100.0)); 
	boost::shared_ptr<SimpleVolQuote> v4(new SimpleVolQuote(0.191,105.0)); 

	std::vector<boost::shared_ptr<SimpleVolQuote>> volVec;
	volVec.push_back(v1); volVec.push_back(v2);
	volVec.push_back(v3); volVec.push_back(v4);
	boost::shared_ptr<SimpleSmile> mySmile(new SimpleSmile(volVec));

	Time tau=0.5; Real spot=100.0, rd=0.03, rf=0.024;
	Real fwd=spot*std::exp((rd-rf)*tau);

	SabrModel myModel(mySmile, fwd, tau, rd, rf);

	Real res=myModel.getVanillaPrice(100.0);
	//std::cout << "\nInitial Sabr ATM Vanilla Price: " << res << std::endl;
	res=myModel.getVanillaPrice( 90.0); std::cout << "Initial Sabr Vanilla Price at K= 90.0: " << res << std::endl;
	res=myModel.getVanillaPrice( 95.0); std::cout << "Initial Sabr Vanilla Price at K= 95.0: " << res << std::endl;
	res=myModel.getVanillaPrice(100.0); std::cout << "Initial Sabr Vanilla Price at K=100.0: " << res << std::endl;
	res=myModel.getVanillaPrice(101.0); std::cout << "Initial Sabr Vanilla Price at K=101.0: " << res << std::endl;
	res=myModel.getVanillaPrice(102.0); std::cout << "Initial Sabr Vanilla Price at K=102.0: " << res << std::endl;
	res=myModel.getVanillaPrice(103.0); std::cout << "Initial Sabr Vanilla Price at K=103.0: " << res << std::endl;
	res=myModel.getVanillaPrice(104.0); std::cout << "Initial Sabr Vanilla Price at K=104.0: " << res << std::endl;
	res=myModel.getVanillaPrice(105.0); std::cout << "Initial Sabr Vanilla Price at K=105.0: " << res << std::endl;

	v1->setVolatility(0.22);
	v2->setVolatility(0.214);
	v3->setVolatility(0.207);
	v4->setVolatility(0.211);

	//res=myModel.getVanillaPrice(100.0);
	//std::cout << "\nUpdated Sabr ATM Vanilla Price: " << res << std::endl;
	res=myModel.getVanillaPrice( 90.0); std::cout << "Updated Sabr Vanilla Price at K= 90.0: " << res << std::endl;
	res=myModel.getVanillaPrice( 95.0); std::cout << "Updated Sabr Vanilla Price at K= 95.0: " << res << std::endl;
	res=myModel.getVanillaPrice(100.0); std::cout << "Updated Sabr Vanilla Price at K=100.0: " << res << std::endl;
	res=myModel.getVanillaPrice(105.0); std::cout << "Updated Sabr Vanilla Price at K=105.0: " << res << std::endl;

	char tmp;
	std::cin >> tmp;

}