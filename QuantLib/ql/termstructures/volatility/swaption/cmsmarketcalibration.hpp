/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Giorgio Facchinetti
 Copyright (C) 2014 Peter Caspers

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

/*! \file cmsmarketcalibration.hpp
*/

#ifndef quantlib_cms_market_calibration_h
#define quantlib_cms_market_calibration_h

#include <ql/math/optimization/endcriteria.hpp>
#include <ql/math/matrix.hpp>
#include <ql/math/array.hpp>
#include <ql/handle.hpp>

namespace QuantLib {

    class SwaptionVolatilityStructure;
    class SwaptionVolCube1;
    class SwaptionVolCube1a;
    class CmsMarket;
    class OptimizationMethod;

    class CmsMarketCalibration {
      public:
        enum CalibrationType {OnSpread, OnPrice, OnForwardCmsPrice };

        CmsMarketCalibration(
            Handle<SwaptionVolatilityStructure>& volCube,
            boost::shared_ptr<CmsMarket>& cmsMarket,
            const Matrix& weights,
            CalibrationType calibrationType);

        Handle<SwaptionVolatilityStructure> volCube_;
        boost::shared_ptr<CmsMarket> cmsMarket_;
        Matrix weights_;
        CalibrationType calibrationType_;
        Matrix sparseXabrParameters_, denseXabrParameters_, browseCmsMarket_;

        Array compute(const boost::shared_ptr<EndCriteria>& endCriteria,
                      const boost::shared_ptr<OptimizationMethod>& method,
                      const Array& guess,
                      bool isMeanReversionFixed);

        Matrix compute(const boost::shared_ptr<EndCriteria>& endCriteria,
                      const boost::shared_ptr<OptimizationMethod>& method,
                      const Matrix& guess,
                      bool isMeanReversionFixed,
                      const Real meanReversionGuess = Null<Real>());

        Matrix
        computeParametric(const boost::shared_ptr<EndCriteria> &endCriteria,
                          const boost::shared_ptr<OptimizationMethod> &method,
                          const Matrix &guess, bool isMeanReversionFixed,
                          const Real meanReversionGuess = Null<Real>());

        Real error() { return error_; }
        EndCriteria::Type endCriteria() { return endCriteria_; };

        Real freeParamTransformInverse(Real p) {
            if(boost::dynamic_pointer_cast<SwaptionVolCube1>(*volCube_) != NULL)
                return betaTransformInverse(p);
            if(boost::dynamic_pointer_cast<SwaptionVolCube1a>(*volCube_) != NULL)
                return gammaTransformInverse(p);
            QL_FAIL("unknown swaption vol cube type for cms calibration");
        }
        Real freeParamTransformDirect(Real y) {
            if(boost::dynamic_pointer_cast<SwaptionVolCube1>(*volCube_) != NULL)
                return betaTransformDirect(y);
            if(boost::dynamic_pointer_cast<SwaptionVolCube1a>(*volCube_) != NULL)
                return gammaTransformDirect(y);
            QL_FAIL("unknown swaption vol cube type for cms calibration");
        }

        static Real betaTransformInverse(Real beta) {
            return std::sqrt(-std::log(beta));
        }
        static Real betaTransformDirect(Real y) {
            return std::max(
                std::min(std::fabs(y) < 10.0 ? std::exp(-(y * y)) : 0.0,
                         0.999999),
                0.000001);
        }

        static Real gammaTransformInverse(Real gamma) {
            return std::sqrt(gamma - 0.000001);
        }
        static Real gammaTransformDirect(Real y) {
            return std::fabs(y) < 10.0 ? y*y + 0.000001 : 100.0;
        }


        static Real reversionTransformInverse(Real reversion) {
            return reversion * reversion;
        }
        static Real reversionTransformDirect(Real y) {
            return std::sqrt(y);
        }

      private:
        Real error_;
        EndCriteria::Type endCriteria_;
    };

}

#endif
