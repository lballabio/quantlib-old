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

/*! \file 
\brief abstract base classes for 1-D flat extrapolations
*/

#ifndef quantlib_flatextrapolation_hpp
#define quantlib_flatextrapolation_hpp

#include <ql/math/interpolation.hpp>

namespace QuantLib {

    
    class FlatExtrapolator : public Interpolation {
      public:
        FlatExtrapolator(boost::shared_ptr<Interpolation> decoratedInterpolation) {
            impl_ = boost::shared_ptr<Interpolation::Impl>(
                  new FlatExtrapolatorImpl(decoratedInterpolation));
        }
      protected:
       class FlatExtrapolatorImpl: public Interpolation::Impl{
          public:
            FlatExtrapolatorImpl(boost::shared_ptr<Interpolation> decoratedInterpolation)
            :decoratedInterp_(decoratedInterpolation){
                calculate();
            }
            Real xMin() const {
                return decoratedInterp_->xMin();
            }
            Real xMax() const {
                return decoratedInterp_->xMax();
            }
            bool isInRange(Real x) const {
                return decoratedInterp_->isInRange(x);
            }
            void update() {
                decoratedInterp_->update();
            }
            void calculate() {}
            Real value(Real x) const {
                x = bindX(x);
                return decoratedInterp_->operator()(x);
            }

          private:
            boost::shared_ptr<Interpolation> decoratedInterp_;

            Real bindX(Real x) const {
                if(x < xMin())
                    return xMin();
                if (x > xMax()) 
                    return xMax();
                return x;
            }

        };
    };


}


#endif
