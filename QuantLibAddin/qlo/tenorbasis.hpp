/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Ferdinando Ametrano
 Copyright (C) 2015 Paolo Mazzocchi

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

#ifndef qla_tenorbasis_hpp
#define qla_tenorbasis_hpp

#include <oh/libraryobject.hpp>
#include <ql/types.hpp>

#include <qlo/model.hpp>

namespace QuantLib {
    class TenorBasis;
    class Date;
    class Quote;
    class AbcdMathFunction;
    class PolynomialFunction;
    class IborIndex;
    class CalibratedModel;
    class YieldTermStructure;
    // for calibration
    class PolynomialCalibration;
    class AbcdCalibration2;
    class EndCriteria;
    class OptimizationMethod;


    template <class T>
    class Handle;
}

namespace QuantLibAddin {


    class TenorBasis : public CalibratedModel {
      public:
        TenorBasis(const boost::shared_ptr<ObjectHandler::ValueObject>& p,
                   bool permanent);
    };

    class AbcdTenorBasis : public TenorBasis {
    public:
        AbcdTenorBasis(
            const boost::shared_ptr<ObjectHandler::ValueObject>& p,
            boost::shared_ptr<QuantLib::IborIndex> iborIndex,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            QuantLib::Date referenceDate,
            bool isSimple,
            const std::vector<QuantLib::Real>& coeff,
            bool permanent);
    };

    class PolynomialTenorBasis : public TenorBasis {
    public:
        PolynomialTenorBasis(
            const boost::shared_ptr<ObjectHandler::ValueObject>& p,
            boost::shared_ptr<QuantLib::IborIndex> iborIndex,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            QuantLib::Date referenceDate,
            bool isSimple,
            const std::vector<QuantLib::Real>& coeff,
            bool permanent);
    };

    class AbcdCalibration2 :
        public ObjectHandler::LibraryObject<QuantLib::AbcdCalibration2> {
    public:
        AbcdCalibration2(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Time>& t,
            const std::vector<QuantLib::Rate>& r,
            const std::vector<QuantLib::Real>& w,
            std::vector<QuantLib::Real> coeff,
            const std::vector<bool>& fixedCoeff,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method,
            bool permanent);
    };

    class PolynomialCalibration :
        public ObjectHandler::LibraryObject<QuantLib::PolynomialCalibration> {
    public:
        PolynomialCalibration(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Time>& t,
            const std::vector<QuantLib::Rate>& rates,
            const std::vector<QuantLib::Real>& weights,
            std::vector<QuantLib::Real> coeff,
            const std::vector<bool>& fixedCoeff,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method,
            bool permanent);
    };

}

#endif
