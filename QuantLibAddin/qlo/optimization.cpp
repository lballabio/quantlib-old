
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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif
#include <qlo/optimization.hpp>
#include <ql/Optimization/armijo.hpp>
#include <ql/Optimization/conjugategradient.hpp>
#include <ql/Optimization/levenbergmarquardt.hpp>
#include <ql/Optimization/simplex.hpp>
#include <ql/Optimization/steepestdescent.hpp>

namespace QuantLibAddin {

    EndCriteria::EndCriteria(QuantLib::Size maxIteration,
                             QuantLib::Real functionEpsilon,
                             QuantLib::Real gradientEpsilon) {
        libraryObject_ = boost::shared_ptr<QuantLib::EndCriteria>(new
            QuantLib::EndCriteria(maxIteration,
                                  functionEpsilon,
                                  gradientEpsilon));
    }


    Simplex::Simplex(QuantLib::Real lambda,
                     const QuantLib::Array& initVal) {
        libraryObject_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
            QuantLib::Simplex(lambda, initVal));
    }

    LevenbergMarquardt::LevenbergMarquardt(QuantLib::Real epsfcn,
                                           QuantLib::Real xtol,
                                           QuantLib::Real gtol,
                                           const QuantLib::Array& initVal)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
            QuantLib::LevenbergMarquardt(epsfcn, xtol, gtol, initVal));
    }

    ArmijoLineSearch::ArmijoLineSearch(QuantLib::Real eps,
                                       QuantLib::Real alpha,
                                       QuantLib::Real beta) {
        libraryObject_ = boost::shared_ptr<QuantLib::LineSearch>(new
            QuantLib::ArmijoLineSearch(eps, alpha, beta));
    }

    ConjugateGradient::ConjugateGradient(
                   const QuantLib::Array& initVal,
                   const boost::shared_ptr<QuantLib::LineSearch>& lineSearch)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
            QuantLib::ConjugateGradient(initVal, lineSearch));
    }

    SteepestDescent::SteepestDescent(
                   const QuantLib::Array& initVal,
                   const boost::shared_ptr<QuantLib::LineSearch>& lineSearch)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
            QuantLib::SteepestDescent(initVal, lineSearch));
    }
      
}
