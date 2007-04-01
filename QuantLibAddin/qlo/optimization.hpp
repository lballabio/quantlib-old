/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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


#ifndef qla_optimization_hpp
#define qla_optimization_hpp

#include <oh/objhandler.hpp>
#include <ql/optimization/method.hpp>
#include <ql/optimization/linesearch.hpp>

namespace QuantLibAddin {

    class EndCriteria :
        public ObjHandler::LibraryObject<QuantLib::EndCriteria> {
      public:
        EndCriteria(QuantLib::Size maxIterations,
                    QuantLib::Size maxStationaryStateIterations,
                    QuantLib::Real rootEpsilon,
                    QuantLib::Real functionEpsilon,
                    QuantLib::Real gradientNormEpsilon);
    };

    class OptimizationMethod :
        public ObjHandler::LibraryObject<QuantLib::OptimizationMethod> {};

    class Simplex : public OptimizationMethod {
      public:
        Simplex(QuantLib::Real lambda);
    };

    class LevenbergMarquardt : public OptimizationMethod {
      public:
        LevenbergMarquardt(QuantLib::Real epsfcn,
                           QuantLib::Real xtol,
                           QuantLib::Real gtol);
    };
   
    class LineSearch :
        public ObjHandler::LibraryObject<QuantLib::LineSearch> {};

    class ArmijoLineSearch : public LineSearch {
      public:
        ArmijoLineSearch(QuantLib::Real eps,
                         QuantLib::Real alpha,
                         QuantLib::Real beta);
    };

    class LineSearchBasedMethod : public OptimizationMethod {};

    class ConjugateGradient : public LineSearchBasedMethod {
      public:
        ConjugateGradient(const boost::shared_ptr<QuantLib::LineSearch>&);
    };

    class SteepestDescent : public LineSearchBasedMethod {
      public:
        SteepestDescent(const boost::shared_ptr<QuantLib::LineSearch>&);
    };

}

#endif
