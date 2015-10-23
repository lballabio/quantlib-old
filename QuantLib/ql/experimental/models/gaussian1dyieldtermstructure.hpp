/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file gaussian1dyieldtermstructure.hpp
    \brief gaussian 1d model implied yield term structure
    The yield term structure has the same reference date as the model's
    underlying yield term structure (at construction time only !).
    The discount factor is 1.0 until the reference time t given in
    the constructor.
*/

#ifndef quantlib_gaussian1dyieldtermstructure_hpp
#define quantlib_gaussian1dyieldtermstructure_hpp

#include <ql/experimental/models/gaussian1dyieldtermstructure.hpp>

namespace QuantLib {

class Gaussian1dYieldTermStructure : public YieldTermStructure {
  public:
    Gaussian1dYieldTermStructure(const boost::shared_ptr<Gaussian1dModel> model,
                                 const Time t, const Real y = 0.0)
        : YieldTermStructure(0, model->termStructure()->calendar(),
                             model->termStructure()->dayCounter()),
          model_(model), t_(t), y_(y) {
        registerWith(model_);
    }

    Date maxDate() const {
        return Date::maxDate();
    }

  protected:
    Real discountImpl(Time t) const {
        if (t < t_)
            return 1.0;
        return model_->zerobond(t, t_, y_);
    }

  private:
    boost::shared_ptr<Gaussian1dModel> model_;
    Real t_, y_;
};
} // namespace QuantLib

#endif
