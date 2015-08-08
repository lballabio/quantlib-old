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

/*! \file lbfgsbroutines.hpp
    \brief c-interface to the f2c code. See lbfgs.hpp for the C++ interface
*/

#ifndef quantlib_optimization_l_bfgs_b_routines_hpp
#define quantlib_optimization_l_bfgs_b_routines_hpp

namespace QuantLib {
	namespace LBFGSb {
		int setulb_(int* n, int* m, 
                     double* x, double* l, double* u,  
                     int* nbd,
                     double* f, double* g, double* factr, 
                     double* pgtol, double* wa,
                     int* iwa, char* task, int* iprint, 
                     char* csave, int* lsave, int* isave,
                     double* dsave, int task_len, int csave_len);
	}
}

#endif
