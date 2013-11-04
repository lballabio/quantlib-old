/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file gaussian1dswaptionsmilesection.hpp
    \brief Gaussian1d model generated swaption smile section
    \warning the smile section is always initialized with a fixed reference date
             even if the model's termstructure is floating
*/

#ifndef quantlib_swaptionsmilesection_gaussian1d_hpp
#define quantlib_swaptionsmilesection_gaussian1d_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/experimental/models/gaussian1dmodel.hpp>

namespace QuantLib {


    class Gaussian1dSwaptionSmileSection : public SmileSection {

    public:
        Gaussian1dSwaptionSmileSection(const boost::shared_ptr<Gaussian1dModel> &model,
                                       const boost::shared_ptr<SwapIndex> &index,
                                       const Date &expiry,
                                       const Period &tenor) :
            SmileSection(expiry,
                         model->termStructure()->dayCounter(),
                         model->termStructure()->referenceDate()),
            model_(model), index_(index->clone(tenor)), expiry_(expiry), tenor_(tenor) {

            forward_ = model_->swapRate(expiry_, tenor_, referenceDate(), 0.0, index_);

        }

        Real minStrike() const { return QL_MIN_REAL; }
        Real maxStrike() const { return QL_MAX_REAL; }
        Real atmLevel() const { return forward_; }

        Real optionPrice(Rate strike, Option::Type type = Option::Call, Real discount=1.0) const;

    protected:
        Volatility volatilityImpl(Rate strike) const;

    private:
        const boost::shared_ptr<Gaussian1dModel> model_;
        const boost::shared_ptr<SwapIndex> index_;
        const Date expiry_;
        const Period tenor_;
        Real forward_;

    };

}


#endif
