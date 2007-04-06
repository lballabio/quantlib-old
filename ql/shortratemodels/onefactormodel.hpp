/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2001, 2002, 2003 Sadruddin Rejeb
 Copyright (C) 2005 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/reference/license.html>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file onefactormodel.hpp
    \brief Abstract one-factor interest rate model class
*/

#ifndef quantlib_one_factor_model_hpp
#define quantlib_one_factor_model_hpp

#include <ql/qldefines.hpp>

#if !defined(QL_PATCH_BORLAND)

#include <ql/stochasticprocess.hpp>
#include <ql/shortratemodels/model.hpp>
#include <ql/methods/lattices/lattice1d.hpp>
#include <ql/methods/lattices/trinomialtree.hpp>

namespace QuantLib {

    //! Single-factor short-rate model abstract class
    /*! \ingroup shortrate */
    class OneFactorModel : public ShortRateModel {
      public:
        OneFactorModel(Size nArguments);
        virtual ~OneFactorModel() {}

        class ShortRateDynamics;
        class ShortRateTree;

        //! returns the short-rate dynamics
        virtual boost::shared_ptr<ShortRateDynamics> dynamics() const = 0;

        //! Return by default a trinomial recombining tree
        boost::shared_ptr<Lattice> tree(const TimeGrid& grid) const;
    };

    //! Base class describing the short-rate dynamics
    class OneFactorModel::ShortRateDynamics {
      public:
        ShortRateDynamics(
                        const boost::shared_ptr<StochasticProcess1D>& process)
        : process_(process) {}
        virtual ~ShortRateDynamics() {};

        //! Compute state variable from short rate
        virtual Real variable(Time t, Rate r) const = 0;

        //! Compute short rate from state variable
        virtual Rate shortRate(Time t, Real variable) const = 0;

        //! Returns the risk-neutral dynamics of the state variable
        const boost::shared_ptr<StochasticProcess1D>& process() {
            return process_;
        }
      private:
        boost::shared_ptr<StochasticProcess1D> process_;
    };

    //! Recombining trinomial tree discretizing the state variable
    class OneFactorModel::ShortRateTree
        : public TreeLattice1D<OneFactorModel::ShortRateTree> {
      public:
        //! Plain tree build-up from short-rate dynamics
        ShortRateTree(const boost::shared_ptr<TrinomialTree>& tree,
                      const boost::shared_ptr<ShortRateDynamics>& dynamics,
                      const TimeGrid& timeGrid);
        //! Tree build-up + numerical fitting to term-structure
        ShortRateTree(const boost::shared_ptr<TrinomialTree>& tree,
                      const boost::shared_ptr<ShortRateDynamics>& dynamics,
                      const boost::shared_ptr
                          <TermStructureFittingParameter::NumericalImpl>& phi,
                      const TimeGrid& timeGrid);

        Size size(Size i) const {
            return tree_->size(i);
        }
        DiscountFactor discount(Size i, Size index) const {
            Real x = tree_->underlying(i, index);
            Rate r = dynamics_->shortRate(timeGrid()[i], x);
            return std::exp(-r*timeGrid().dt(i));
        }
        Real underlying(Size i, Size index) const {
            return tree_->underlying(i, index);
        }
        Size descendant(Size i, Size index, Size branch) const {
            return tree_->descendant(i, index, branch);
        }
        Real probability(Size i, Size index, Size branch) const {
            return tree_->probability(i, index, branch);
        }
      private:
        boost::shared_ptr<TrinomialTree> tree_;
        boost::shared_ptr<ShortRateDynamics> dynamics_;
        class Helper;
    };

    //! Single-factor affine base class
    /*! Single-factor models with an analytical formula for discount bonds
        should inherit from this class. They must then implement the
        functions \f$ A(t,T) \f$ and \f$ B(t,T) \f$ such that
        \f[
            P(t, T, r_t) = A(t,T)e^{ -B(t,T) r_t}.
        \f]

        \ingroup shortrate
    */
    class OneFactorAffineModel : public OneFactorModel,
                                 public AffineModel {
      public:
        OneFactorAffineModel(Size nArguments)
        : OneFactorModel(nArguments) {}

        virtual Real discountBond(Time now,
                                  Time maturity,
                                  Array factors) const {
            return discountBond(now, maturity, factors[0]);
        }

        Real discountBond(Time now, Time maturity, Rate rate) const {
            return A(now, maturity)*std::exp(-B(now, maturity)*rate);
        }

        DiscountFactor discount(Time t) const {
            Real x0 = dynamics()->process()->x0();
            Rate r0 = dynamics()->shortRate(0.0, x0);
            return discountBond(0.0, t, r0);
        }
      protected:
        virtual Real A(Time t, Time T) const = 0;
        virtual Real B(Time t, Time T) const = 0;
    };

}

#endif

#endif

