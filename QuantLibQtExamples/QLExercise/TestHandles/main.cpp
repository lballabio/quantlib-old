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


class SimpleYield: public Observable{
private:
	Rate yield_;
public:
	SimpleYield(const Rate& yield) : yield_(yield){}
	Rate getYield() const {return yield_;}

	void setYield(const Rate& yield){
		yield_=yield;
		std::cout << "yield has changed, notify observers!" << std::endl;
		notifyObservers();
	}
};

class SimpleDiscountFactor2: public Observable, Observer{
private:
	DiscountFactor df_;
	Time mat_;
	Handle<SimpleYield> y_;
public:
	SimpleDiscountFactor2(const Handle<SimpleYield>& y, 
		Time& mat):y_(y),mat_(mat){
			// register yield as an observable!
			registerWith(y_);
			df_=exp(-(*y_)->getYield()*mat_);
	}
	void update(){
		std::cout << "Handle<SimpleYield> has changed, recalculate discount factor and notify observers." << std::endl;
		df_=exp(-(*y_)->getYield()*mat_);
		//std::cout << "yield has changed, notify observers!" << std::endl;
		notifyObservers();
	}
	Real getDiscount() const{
		return df_;
	}
};


int main() {

	boost::shared_ptr<SimpleYield> yield(new SimpleYield(0.04));
	RelinkableHandle<SimpleYield> yieldHandle(yield);
	Time mat=10.0;
	SimpleDiscountFactor2 myDf(yieldHandle,mat);
	std::cout << "Initial Discount:" << myDf.getDiscount() << std::endl << std::endl;

	std::cout << std::endl;
	yield->setYield(0.06);
	std::cout << "Discount after yield update: " << myDf.getDiscount() << std::endl;

	std::cout << std::endl;
	boost::shared_ptr<SimpleYield> yieldNew(new SimpleYield(0.01));
	yieldHandle.linkTo(yieldNew);
	std::cout << "Discount after relinking: " << myDf.getDiscount() << std::endl;

	std::cout << std::endl;
	boost::shared_ptr<SimpleYield> yieldNew2(new SimpleYield(0.001));
	yieldHandle.linkTo(yieldNew2);
	std::cout << "Discount after relinking: " << myDf.getDiscount() << std::endl;

	std::cout << std::endl;
	yieldHandle->setYield(0.10);
	std::cout << "Discount after yield update: " << myDf.getDiscount() << std::endl;

	char tmp;
	std::cin >> tmp;

}