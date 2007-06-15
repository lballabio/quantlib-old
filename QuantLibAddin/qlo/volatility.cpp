
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/volatility.hpp>
#include <ql/legacy/libormarketmodels/lmextlinexpvolmodel.hpp>
#include <ql/models/marketmodels/models/piecewiseconstantabcdvariance.hpp>

namespace QuantLibAddin {
      
     PiecewiseConstantAbcdVariance::PiecewiseConstantAbcdVariance(
                            QuantLib::Real a, QuantLib::Real b,
                            QuantLib::Real c, QuantLib::Real d,
                            const QuantLib::Size resetIndex,
                            const std::vector<QuantLib::Time>& rateTimes) {

        libraryObject_ =
            boost::shared_ptr<QuantLib::PiecewiseConstantVariance>(new
                QuantLib::PiecewiseConstantAbcdVariance(a, b, c, d,
                                                        resetIndex,
                                                        rateTimes));

    }

    //Volatility model
    LmExtLinearExponentialVolModel::LmExtLinearExponentialVolModel(
        const std::vector<QuantLib::Time>& fixingTimes,
        QuantLib::Real a,
        QuantLib::Real b,
        QuantLib::Real c,
        QuantLib::Real d) {

            libraryObject_ = boost::shared_ptr<QuantLib::LmExtLinearExponentialVolModel>(
            new QuantLib::LmExtLinearExponentialVolModel(fixingTimes,a,b,c,d));
    }   
}
