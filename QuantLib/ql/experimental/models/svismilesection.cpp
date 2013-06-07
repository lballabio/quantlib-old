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

#include <ql/experimental/models/svismilesection.hpp>
#include <ql/experimental/models/smilesectionutils.hpp>

namespace QuantLib {

    SviSmileSection::SviSmileSection(const boost::shared_ptr<SmileSection> source, const Real atm,
				     const std::vector<Real>& moneynessGrid)
		: source_(source), SmileSection(*source) {
		

                k_ = SmileSectionUtils().makeStrikeGrid(*source_, moneynessGrid);

		QL_REQUIRE(k_.size() >=5, "at least five strikes must be given (" << k_.size() <<")");
		QL_REQUIRE(k_[0] >= 0.0,"strikes must be non negative (" << k_[0] << ")");

		if(atm==Null<Real>()) {
		    f_ = source_->atmLevel();
		}
		else {
		    f_ = atm;
		}

		for(Size i=1; i<k_.size(); i++) {
			QL_REQUIRE(k_[i]>k_[i-1], "strikes must be strictly increasing (" << k_[i-1] << " >= " << k_[i] << ")");
		}

		compute();

    }

	void SviSmileSection::compute() {

		// set default values for parameters
		s_=0.1; r_=-0.4; m_=0.0; b_= 2.0 / ((1.0+fabs(r_))*exerciseTime());
		//a_=0.04;
		a_=source_->variance(f_)-b_*(r_*(-m_)*sqrt((-m_)*(-m_)+s_*s_));

		SviCostFunction cost(this);
		SviConstraint constraint(exerciseTime());
		//NoConstraint constraint;

		Array initial(5);
		initial[0] = a_; initial[1] = b_; initial[2] = s_; initial[3] = r_; initial[4] = m_;

		Problem p(cost,constraint,initial);
		
		//LevenbergMarquardt lm;
		//Simplex lm(0.01);
		BFGS lm;
		EndCriteria ec(5000,100,1e-8,1e-8,1e-8);

		EndCriteria::Type ret = lm.minimize(p,ec);
		QL_REQUIRE(ret!=EndCriteria::MaxIterations,"Optimizer returns maxiterations");

		Array res = p.currentValue();

		a_ = res[0]; b_ = res[1]; s_= res[2]; r_ = res[3]; m_ = res[4];

		std::cout << "SVI result: a=" << a_ << " b=" << b_ << " s=" << s_ << " r=" << r_ << " m=" << m_ << std::endl;
		std::cout << "Constraint: " << b_*(1.0+fabs(r_)) << " <= " << 4.0/exerciseTime() << std::endl;

	}
 
	
    Real SviSmileSection::volatilityImpl(Rate strike) const {
		return sqrt(variance(strike)/exerciseTime());
    }

	Real SviSmileSection::varianceImpl(Rate strike) const {
		strike = std::max ( strike, QL_EPSILON );
		Real k = log(strike/f_);
		return a_+b_*(r_*(k-m_)+sqrt((k-m_)*(k-m_)+s_*s_));
	}



}
