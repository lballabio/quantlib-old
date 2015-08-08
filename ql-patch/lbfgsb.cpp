/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Klaus Spanderen

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

#include <ql/math/optimization/constraint.hpp>
#include <ql/math/optimization/lbfgsb.hpp>
#include <ql/math/optimization/lbfgsbroutines.hpp>

#include <limits>
#include <string.h>

namespace QuantLib {

    L_BFGS_b::L_BFGS_b(Size m, 
    				   const BoundaryConditionType & bounds)
    :  m_(m),
       bounds_(bounds) {
    }
    
    const std::string& L_BFGS_b::getInfoMessage() const {
    	return infoStr_;
    }
    
    EndCriteria::Type L_BFGS_b::minimize(Problem& P,
                                         const EndCriteria& endCriteria) {

        P.reset();
        Array x = P.currentValue();
        Real f = P.costFunction().value(x);
        const Real initf = f;
        int n = x.size();
 
        boost::scoped_array<double> l(new double[n]);
        boost::scoped_array<double> u(new double[n]);

        boost::scoped_array<int> nbd(new int[n]);
        std::fill(nbd.get(), nbd.get()+n, 0); // no boundaries yet
        
        for (Size i=0; i < bounds_.size(); ++i) {
        	nbd[bounds_[i].first] = 2;
        	l[bounds_[i].first] = bounds_[i].second.first;
        	u[bounds_[i].first] = bounds_[i].second.second;
        }
        
        Array g(n, 0.0);

        double factr = endCriteria.functionEpsilon()
        				/std::numeric_limits<double>::epsilon();
        double pgtol = endCriteria.gradientNormEpsilon();
        
        boost::scoped_array<double> wa(new double[(2*m_+4)*n+12*m_*m_+12*m_]);
        boost::scoped_array<int> iwa(new int[3*n]);

        int iprint = -100;
        
        boost::scoped_array<char> task(new char[60]);
        boost::scoped_array<char> csave(new char[60]);

        boost::scoped_array<int> lsave(new int[4]);
        boost::scoped_array<int> isave(new int[44]);

        boost::scoped_array<double> dsave(new double[30]);
        strncpy(task.get(), "START", 6);

        Size iterations = 0;
        EndCriteria::Type ecType = EndCriteria::Unknown;
        
        while (   (   !memcmp(task.get(),"START", 5) 
                   || !memcmp(task.get(),"NEW_X", 5)
                   || !memcmp(task.get(), "FG", 2) )
                && !endCriteria.checkMaxIterations(iterations++, ecType)) {

            LBFGSb::setulb_(&n, &m_, 
                    x.begin(), l.get(), u.get(), nbd.get(), &f, g.begin(), 
                    &factr, &pgtol, wa.get(), iwa.get(), task.get(),
                    &iprint, csave.get(), lsave.get(), isave.get(),
                    dsave.get(), 60, 60); 

            
            if (!memcmp(task.get(), "FG", 2)) {
            	if (P.constraint().test(x)) {
            		f = P.valueAndGradient(g, x);
            	}
            	else {
            		f = initf;
            		std::fill(g.begin(), g.end(), 0.0);
            	}
            }
        }
        
        endCriteria.checkMaxIterations(iterations, ecType);
        infoStr_ = task.get();
        if (!strcmp(task.get(), 
        	  "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH")) {
            ecType = EndCriteria::StationaryFunctionValue;
        }
        else if (!strcmp(task.get(), 
	  	      "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL")) {
        	ecType = EndCriteria::ZeroGradientNorm;
	  	}
        else if (!endCriteria.checkMaxIterations(iterations, ecType)) {
        	QL_FAIL("Error in LBFGSB: " + infoStr_);
        }       
        	
        P.setCurrentValue(x);
        
        return ecType;
    } 
}

