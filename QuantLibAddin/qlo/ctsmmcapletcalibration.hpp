
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

#ifndef qla_ctsmmcapletcalibration_hpp
#define qla_ctsmmcapletcalibration_hpp

#include <oh/libraryobject.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class CTSMMCapletCalibration;
    class PiecewiseConstantCorrelation;
    class EvolutionDescription;
    class PiecewiseConstantVariance;
    class CurveState;
    class AlphaForm;
}

namespace QuantLibAddin {

    class CTSMMCapletCalibration : public
        ObjectHandler::LibraryObject<QuantLib::CTSMMCapletCalibration> {
    };

    class CTSMMCapletOriginalCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletOriginalCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            const std::vector<QuantLib::Real>& alpha,
            bool lowestRoot,
			bool useFullApprox);
    };

    class CTSMMCapletAlphaFormCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletAlphaFormCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            const std::vector<QuantLib::Real>& alphaInitial,
            const std::vector<QuantLib::Real>& alphaMax,
            const std::vector<QuantLib::Real>& alphaMin,
            bool maximizeHomogeneity,
            boost::shared_ptr<QuantLib::AlphaForm>& parametricForm);
    };

    class CTSMMCapletMaxHomogeneityCalibration : public CTSMMCapletCalibration {
      public:
        CTSMMCapletMaxHomogeneityCalibration(
            const QuantLib::EvolutionDescription& evolution,
            const boost::shared_ptr<QuantLib::PiecewiseConstantCorrelation>& corr,
            const std::vector<boost::shared_ptr<QuantLib::PiecewiseConstantVariance> >& swapVariances,
            const std::vector<QuantLib::Volatility>& capletVols,
            const boost::shared_ptr<QuantLib::CurveState>& cs,
            QuantLib::Spread displacement,
            QuantLib::Real caplet0Swaption1Priority);
    };


 }

#endif
