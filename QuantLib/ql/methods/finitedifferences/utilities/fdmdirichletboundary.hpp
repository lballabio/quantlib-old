/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Andreas Gaida
 Copyright (C) 2008 Ralph Schreyer
 Copyright (C) 2008 Klaus Spanderen
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


/*! \file fdmdirichletboundary.hpp
    \brief Dirichlet boundary conditions for differential operators
*/

#ifndef quantlib_fdm_dirichlet_boundary_hpp
#define quantlib_fdm_dirichlet_boundary_hpp

#include <ql/methods/finitedifferences/boundarycondition.hpp>
#include <ql/methods/finitedifferences/operators/fdmlinearop.hpp>
#include <boost/function.hpp>

namespace QuantLib {

    class FdmMesher;
    class FdmLinearOpLayout;

    class FdmDirichletBoundary : public BoundaryCondition<FdmLinearOp> {
      public:
        // types and enumerations
        typedef FdmLinearOp operator_type;
        typedef FdmLinearOp::array_type array_type;
        typedef BoundaryCondition<FdmLinearOp>::Side Side;

        FdmDirichletBoundary(const boost::shared_ptr<FdmMesher>& mesher,
                             Real valueOnBoundary, Size direction, Side side);

		FdmDirichletBoundary(const boost::shared_ptr<FdmMesher>& mesher,
                             const Array valuesOnBoundary, Size direction, Side side);

		FdmDirichletBoundary(const boost::shared_ptr<FdmMesher>& mesher,
							 const boost::function<Real (Real)>& valueOnBoundary, Size direction, Side side);

		FdmDirichletBoundary(const boost::shared_ptr<FdmMesher>& mesher,
							 const boost::function<Disposable<Array> (Real)>& valuesOnBoundary, Size direction, 
                             Side side);

        void applyBeforeApplying(operator_type&) const {}
        void applyBeforeSolving(operator_type&, array_type&) const;
        void applyAfterApplying(array_type&) const;
        void applyAfterSolving(array_type&) const;
        void setTime(Time);
        
        Real applyAfterApplying(Real x, Real value) const;

      private:
		void init();
		const boost::shared_ptr<FdmMesher>& mesher_;
		const Size direction_;
        const Side side_;  
        Real valueOnBoundary_;
		Array valuesOnBoundary_;
		const boost::function<Real (Real)> valueOnBoundaryTimeDep_;
		const boost::function<Disposable<Array> (Real)> valuesOnBoundaryTimeDep_;
		const bool spatialDependentBoundary_;
		const bool timeDependentBoundary_;
        Real xExtreme_;
        std::vector<Size> indicies_;

    };



}

#endif
