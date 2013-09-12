/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
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

#include <ql/experimental/finitedifferences/fdmsabrfpeop.hpp>
#include <ql/methods/finitedifferences/operators/secondderivativeop.hpp>
#include <ql/methods/finitedifferences/operators/secondordermixedderivativeop.hpp>

namespace QuantLib {

    FdmSabrFpeUnderlyingPart::FdmSabrFpeUnderlyingPart(const boost::shared_ptr<FdmMesher>& mesher, const Real beta, const Real nu, const Real rho) :
      volatilityValues_(mesher->locations(1)),
	  forwardValues_(mesher->locations(0)),
      dxMap_ (FirstDerivativeOp(0, mesher).mult(2.0*beta*Pow(forwardValues_,2.0*beta-1.0)*volatilityValues_*volatilityValues_+
												2.0*nu*rho*Pow(forwardValues_,beta)*volatilityValues_)),
      dxxMap_(SecondDerivativeOp(0, mesher).mult(0.5*volatilityValues_*volatilityValues_*Pow(forwardValues_,2.0*beta))),
      mapT_  (dxMap_.add(dxxMap_).add(beta*volatilityValues_*volatilityValues_*Pow(forwardValues_,2.0*beta-2.0)*(2.0*beta-1.0)+
	                                  2.0*beta*nu*rho*Pow(forwardValues_,beta-1.0)*volatilityValues_+nu*nu)),
      mesher_(mesher) {
        
    }

    void FdmSabrFpeUnderlyingPart::setTime(Time t1, Time t2) {
		
    }

    const TripleBandLinearOp& FdmSabrFpeUnderlyingPart::getMap() const {
        return mapT_;
    }

    FdmSabrFpeVolatilityPart::FdmSabrFpeVolatilityPart(
            const boost::shared_ptr<FdmMesher>& mesher,
			const Real beta, const Real nu, const Real rho) :
      volatilityValues_(mesher->locations(1)),
	  forwardValues_(mesher->locations(0)),
	  dyMap_(FirstDerivativeOp(1,mesher).mult(beta*nu*rho*volatilityValues_*volatilityValues_*Pow(forwardValues_,beta-1.0)+2.0*nu*nu*volatilityValues_)),
	  dyyMap_(SecondDerivativeOp(1,mesher).mult(0.5*nu*nu*volatilityValues_*volatilityValues_)),
      mapT_(dyMap_.add(dyyMap_)) {
    }

    void FdmSabrFpeVolatilityPart::setTime(Time t1, Time t2) {

    }

    const TripleBandLinearOp& FdmSabrFpeVolatilityPart::getMap() const {
        return mapT_;
    }

    FdmSabrFpeOp::FdmSabrFpeOp(const boost::shared_ptr<FdmMesher> &mesher,
                               const Real beta, const Real nu, const Real rho)
        : volatilityValues_(mesher->locations(1)),
          forwardValues_(mesher->locations(0)), dxMap_(mesher, beta, nu, rho),
          dyMap_(mesher, beta, nu, rho),
          dxyMap_(SecondOrderMixedDerivativeOp(0, 1, mesher)
                      .mult(nu * rho * volatilityValues_ * volatilityValues_ *
                            Pow(forwardValues_, beta))) {}

    void FdmSabrFpeOp::setTime(Time t1, Time t2) {
        dxMap_.setTime(t1, t2);
        dyMap_.setTime(t1, t2);
    }

    Size FdmSabrFpeOp::size() const {
        return 2;
    }

    Disposable<Array> FdmSabrFpeOp::apply(const Array& u) const {
		return dyMap_.getMap().apply(u)+ dxMap_.getMap().apply(u) + dxyMap_.apply(u);
    }

    Disposable<Array> FdmSabrFpeOp::apply_direction(Size direction,
                                                   const Array& r) const {
        if (direction == 0)
            return dxMap_.getMap().apply(r);
        else if (direction == 1)
            return dyMap_.getMap().apply(r);
        else
            QL_FAIL("direction too large");
    }

    Disposable<Array> FdmSabrFpeOp::apply_mixed(const Array& r) const {
        return dxyMap_.apply(r);
    }

    Disposable<Array>
        FdmSabrFpeOp::solve_splitting(Size direction,
                                     const Array& r, Real a) const {

        if (direction == 0) {
            return dxMap_.getMap().solve_splitting(r, a, 1.0);
        }
        else if (direction == 1) {
            return dyMap_.getMap().solve_splitting(r, a, 1.0);
        }
        else
            QL_FAIL("direction too large");
    }

    Disposable<Array>
        FdmSabrFpeOp::preconditioner(const Array& r, Real dt) const {

        return solve_splitting(0, r, dt);
    }

#if !defined(QL_NO_UBLAS_SUPPORT)
    Disposable<std::vector<SparseMatrix> >
    FdmSabrFpeOp::toMatrixDecomp() const {
        std::vector<SparseMatrix> retVal(3);
        retVal[0] = dxMap_.getMap().toMatrix();
        retVal[1] = dyMap_.getMap().toMatrix();
        retVal[2] = dxyMap_.toMatrix();
        return retVal;
    }
#endif

}
