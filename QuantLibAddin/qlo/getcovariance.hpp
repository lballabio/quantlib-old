/*
 Copyright (C) 2006 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/


#ifndef qla_getcovariance_hpp
#define qla_getcovariance_hpp

#include <oh/objhandler.hpp>
#include <ql/methods/montecarlo/getcovariance.hpp>

namespace QuantLibAddin {

    inline QuantLib::Matrix getCovariance(std::vector<double> vols,
                                          const QuantLib::Matrix& corr,
                                          double tol) 
    {
        return QuantLib::getCovariance(vols.begin(), vols.end(), corr, tol);
    }

    class CovarianceDecomposition : public ObjHandler::LibraryObject<
        QuantLib::CovarianceDecomposition>
    {
      public:
        CovarianceDecomposition(
            const QuantLib::Matrix& cov,
            double tol);
    };
}

#endif
