/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier

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

#ifndef quantlib_optimizers_i
#define quantlib_optimizers_i

%include functions.i

// 1D Solvers

%{
using QuantLib::Bisection;
using QuantLib::Brent;
using QuantLib::FalsePosition;
using QuantLib::Newton;
using QuantLib::NewtonSafe;
using QuantLib::Ridder;
using QuantLib::Secant;
%}

#if defined(SWIGMZSCHEME)
%typecheck(SWIG_TYPECHECK_POINTER) Scheme_Object* {
    $1 = 1;
}
#elif defined(SWIGGUILE)
%typecheck(SWIG_TYPECHECK_POINTER) SCM {
    $1 = 1;
}
#endif

%define DeclareSolver(SolverName)
class SolverName {
    #if defined(SWIGRUBY)
    %rename("maxEvaluations=")      setMaxEvaluations;
    %rename("lowerBound=")          setLowerBound;
    %rename("upperBound=")          setUpperBound;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("max-evaluations-set!") setMaxEvaluations;
    %rename("lower-bound-set!")     setLowerBound;
    %rename("upper-bound-set!")     setUpperBound;
    #endif
  public:
    void setMaxEvaluations(Size evaluations);
    void setLowerBound(Real lowerBound);
    void setUpperBound(Real upperBound);
    %extend {
        #if defined(SWIGPYTHON)
        Real solve(PyObject* function, Real xAccuracy,
                   Real guess, Real step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        Real solve(PyObject* function, Real xAccuracy,
                   Real guess, Real xMin, Real xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGRUBY)
        Real solve(Real xAccuracy, Real guess, Real step) {
            UnaryFunction f;
            return self->solve(f, xAccuracy, guess, step);
        }
        Real solve(Real xAccuracy, Real guess,
                   Real xMin, Real xMax) {
            UnaryFunction f;
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGMZSCHEME)
        Real solve(Scheme_Object* function, Real xAccuracy,
                   Real guess, Real step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        Real solve(Scheme_Object* function, Real xAccuracy,
                   Real guess, Real xMin, Real xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGGUILE)
        Real solve(SCM function, Real xAccuracy,
                   Real guess, Real step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        Real solve(SCM function, Real xAccuracy,
                   Real guess, Real xMin, Real xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGJAVA)
        Real solve(UnaryFunctionDelegate* function, Real xAccuracy,
                   Real guess, Real step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        Real solve(UnaryFunctionDelegate* function, Real xAccuracy,
                   Real guess, Real xMin, Real xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #endif
    }
};
%enddef

// Actual solvers
DeclareSolver(Brent);
DeclareSolver(Bisection);
DeclareSolver(FalsePosition);
DeclareSolver(Ridder);
DeclareSolver(Secant);

#if defined(SWIGPYTHON)
// these two need f.derivative()
DeclareSolver(Newton);
DeclareSolver(NewtonSafe);
#endif


// Optimizers

%{
using QuantLib::Constraint;
using QuantLib::BoundaryConstraint;
using QuantLib::NoConstraint;
using QuantLib::PositiveConstraint;
%}

class Constraint {
    // prevent direct instantiation
  private:
    Constraint();
};

class BoundaryConstraint : public Constraint {
  public:
    BoundaryConstraint(Real lower, Real upper);
};

class NoConstraint : public Constraint {
  public:
    NoConstraint();
};

class PositiveConstraint : public Constraint {
  public:
    PositiveConstraint();
};


%{
using QuantLib::EndCriteria;
%}

class EndCriteria {
    #if defined(SWIGRUBY)
    %rename("setPositiveOptimization!") setPositiveOptimization;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call) operator();
    %rename("positive-optimization-set!") setPositiveOptimization;
    #elif defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(call) operator();
    #elif defined(SWIGPYTHON)
    %rename(NoCriteria) None;
    #endif
  public:
    enum Type {
        None,
        MaxIterations,
        StationaryPoint,
        StationaryFunctionValue,
        StationaryFunctionAccuracy,
        ZeroGradientNorm,
        Unknown
    };
    EndCriteria(Size maxIteration,
                Size maxStationaryStateIterations,
                Real rootEpsilon,
                Real functionEpsilon,
                Real gradientNormEpsilon);
    bool operator()(Size iteration,
                    Size &statState,
                    const bool positiveOptimization,
                    const Real fold,
                    const Real normgold,
                    const Real fnew,
                    const Real normgnewx,
                    EndCriteria::Type & ecType) const;
};


%{
using QuantLib::OptimizationMethod;
using QuantLib::ConjugateGradient;
using QuantLib::Simplex;
using QuantLib::SteepestDescent;
using QuantLib::BFGS;
using QuantLib::LevenbergMarquardt;
%}

class OptimizationMethod {
  private:
    // prevent direct instantiation
    OptimizationMethod();
};

class ConjugateGradient : public OptimizationMethod {
  public:
    ConjugateGradient();
};

class Simplex : public OptimizationMethod {
  public:
    Simplex(Real lambda);
};

class SteepestDescent : public OptimizationMethod {
  public:
    SteepestDescent();
};

class BFGS : public OptimizationMethod {
  public:
    BFGS();
};

class LevenbergMarquardt : public OptimizationMethod {
  public:
	LevenbergMarquardt(Real epsfcn = 1.0e-8,
	                   Real xtol = 1.0e-8,
    	               Real gtol = 1.0e-8);
};


%{
using QuantLib::Problem;
%}

%inline %{
    class Optimizer {};
%}
#if defined(SWIGPYTHON)
%extend Optimizer {
    Array solve(PyObject* function, Constraint& c,
                OptimizationMethod& m, EndCriteria &e,
                Array &iv) {
        PyCostFunction f(function);
        Problem p(f,c,iv);
        m.minimize(p, e);
        return p.currentValue();
    }
}
#elif defined(SWIGRUBY)
%extend Optimizer {
    Array solve(Constraint& c, OptimizationMethod& m,
                EndCriteria &e,
                Array &iv) {
        RubyCostFunction f;
        Problem p(f,c,iv);
        m.minimize(p, e);
        return p.currentValue();
    }
}
#elif defined(SWIGMZSCHEME)
%extend Optimizer {
    Array solve(Scheme_Object* function, Constraint& c,
                OptimizationMethod& m,
                EndCriteria &e,
                Array &iv) {
        MzCostFunction f(function);
        Problem p(f,c,iv);
        m.minimize(p, e);
        return p.currentValue();
    }
}
#elif defined(SWIGGUILE)
%extend Optimizer {
    Array solve(SCM function, Constraint& c, OptimizationMethod& m,
                EndCriteria &e, Array &iv) {
        GuileCostFunction f(function);
        Problem p(f,c,iv);
        m.minimize(p, e);
        return p.currentValue();
    }
}
#endif


#endif
