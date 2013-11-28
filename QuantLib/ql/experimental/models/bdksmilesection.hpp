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

/*! \file bdksmilesection.hpp
    \brief smile section using the method described in 
           the benim kainth dodgson paper
*/

#ifndef quantlib_bdk_smile_section_hpp
#define quantlib_bdk_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <vector>

namespace QuantLib {

    class BdkSmileSection : public SmileSection {

      public:
        BdkSmileSection(const boost::shared_ptr<SmileSection> source,
                        const Real leftCutoff, const Real rightCutoff,
                        const Real mu = 3.0, const Real nu = 3.0,
                        const Real atm = Null<Real>());

        Real minStrike() const { return 0.0; }
        Real maxStrike() const { return QL_MAX_REAL; }
        Real atmLevel() const { return f_; }

        Real optionPrice(Rate strike, Option::Type type = Option::Call,
                         Real discount = 1.0) const;

      protected:
        Volatility volatilityImpl(Rate strike) const;

      private:
        boost::shared_ptr<SmileSection> source_;
        Real mu_, nu_;
        Real f_, leftCutoff_, rightCutoff_;
        Real la_, lb_, lc_, ra_, rb_, rc_;
    };
}

#endif
