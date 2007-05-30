
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif
#include <qlo/shortratemodels.hpp>
#include <ql/termstructure.hpp>

namespace QuantLibAddin {

    Vasicek::Vasicek(
            const double &a,
            const double &b,
            const double &lambda,
            const double &sigma) {

        libraryObject_ = boost::shared_ptr<QuantLib::AffineModel>(
            new QuantLib::Vasicek(a, b, lambda, sigma));
    }

    HullWhite::HullWhite(
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const double &a,
            const double &sigma)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::AffineModel>(
            new QuantLib::HullWhite(hYTS, a, sigma));
    }

}

