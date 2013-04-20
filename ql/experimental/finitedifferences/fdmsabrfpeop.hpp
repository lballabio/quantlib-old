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

/*! \file fdmsabrfpeop.hpp
    \brief Sabr Fokker Planck Equation linear operator
	Note that time is reversed in order to make backward solvers work
*/

#ifndef quantlib_fdm_sabrfpe_op_hpp
#define quantlib_fdm_sabrfpe_op_hpp

#include <ql/experimental/finitedifferences/firstderivativeop.hpp>
#include <ql/experimental/finitedifferences/triplebandlinearop.hpp>
#include <ql/experimental/finitedifferences/ninepointlinearop.hpp>
#include <ql/experimental/finitedifferences/fdmlinearopcomposite.hpp>

namespace QuantLib {

	class FdmSabrFpeUnderlyingPart {
      public:
        FdmSabrFpeUnderlyingPart(const boost::shared_ptr<FdmMesher>& mesher, const Real beta, const Real nu, const Real rho);

        void setTime(Time t1, Time t2);
        const TripleBandLinearOp& getMap() const;

      protected:
        const Array volatilityValues_;
		const Array forwardValues_;
        const TripleBandLinearOp dxMap_;
        const TripleBandLinearOp dxxMap_;
        TripleBandLinearOp mapT_;

        const boost::shared_ptr<FdmMesher> mesher_;
    };

	class FdmSabrFpeVolatilityPart {
      public:
        FdmSabrFpeVolatilityPart(
            const boost::shared_ptr<FdmMesher>& mesher,
			const Real beta, const Real nu, const Real rho);

        void setTime(Time t1, Time t2);
        const TripleBandLinearOp& getMap() const;

      protected:
  	    const Array volatilityValues_;
		const Array forwardValues_;
        const TripleBandLinearOp dyMap_;
        const TripleBandLinearOp dyyMap_;
        TripleBandLinearOp mapT_;
    };


    class FdmSabrFpeOp : public FdmLinearOpComposite {
      public:
        FdmSabrFpeOp(const boost::shared_ptr<FdmMesher>& mesher,
			const Real beta,
			const Real nu,
			const Real rho);

        Size size() const;
        void setTime(Time t1, Time t2);

        Disposable<Array> apply(const Array& r) const;
        Disposable<Array> apply_mixed(const Array& r) const;

        Disposable<Array> apply_direction(Size direction,
                                          const Array& r) const;
        Disposable<Array> solve_splitting(Size direction,
                                          const Array& r, Real s) const;
        Disposable<Array> preconditioner(const Array& r, Real s) const;

      private:
	    const Array volatilityValues_;
		const Array forwardValues_;
        NinePointLinearOp dxyMap_;
        FdmSabrFpeUnderlyingPart dxMap_;
        FdmSabrFpeVolatilityPart dyMap_;
    };
}

#endif
