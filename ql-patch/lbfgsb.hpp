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

/*! \file l_bfgs_b.hpp
    \brief limited memory BFGS method to solve the bound
           constrained optimization problem
*/

#ifndef quantlib_optimization_l_bfgs_b_hpp
#define quantlib_optimization_l_bfgs_b_hpp

#include <ql/math/optimization/problem.hpp>

namespace QuantLib {

    //! L-BFGS-B optimization method
    /*! This implementation is based on Lbfgsb.2.1
     */

    /*! References:

       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
       memory algorithm for bound constrained optimization'',
       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.

       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a
       limited memory FORTRAN code for solving bound constrained
       optimization problems'', Tech. Report, NAM-11, EECS Department,
       Northwestern University, 1994.

       (Postscript files of these papers are available via anonymous
        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)

    */
    class L_BFGS_b : public OptimizationMethod {
      public:
    	typedef std::vector<std::pair<Size, std::pair<Real, Real> > > 
    		BoundaryConditionType;
    	
   	    L_BFGS_b(Size m, 
   	    		const BoundaryConditionType & bounds = BoundaryConditionType());

        EndCriteria::Type minimize(Problem& P, 
                                   const EndCriteria& endCriteria);
        
        const std::string& getInfoMessage() const;
        
      private:
        int m_;
        std::string infoStr_;
        const BoundaryConditionType bounds_;
    };

}


#endif
