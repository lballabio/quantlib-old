
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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
    void setMaxEvaluations(int evaluations);
    void setLowerBound(double lowerBound);
    void setUpperBound(double upperBound);
    %extend {
        #if defined(SWIGPYTHON)
        double solve(PyObject* function, double xAccuracy, 
                     double guess, double step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        double solve(PyObject* function, double xAccuracy,
                     double guess, double xMin, double xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGRUBY)
        double solve(double xAccuracy, double guess, double step) {
            UnaryFunction f;
            return self->solve(f, xAccuracy, guess, step);
        }
        double solve(double xAccuracy, double guess,
                     double xMin, double xMax) {
            UnaryFunction f;
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGMZSCHEME)
        double solve(Scheme_Object* function, double xAccuracy, 
                     double guess, double step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        double solve(Scheme_Object* function, double xAccuracy,
                     double guess, double xMin, double xMax) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, xMin, xMax);
        }
        #elif defined(SWIGGUILE)
        double solve(SCM function, double xAccuracy, 
                     double guess, double step) {
            UnaryFunction f(function);
            return self->solve(f, xAccuracy, guess, step);
        }
        double solve(SCM function, double xAccuracy,
                     double guess, double xMin, double xMax) {
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
using QuantLib::Optimization::Constraint;
using QuantLib::Optimization::BoundaryConstraint;
using QuantLib::Optimization::NoConstraint;
using QuantLib::Optimization::PositiveConstraint;
%}
        
class Constraint {
    // prevent direct instantiation
  private:
    Constraint();
};

class BoundaryConstraint : public Constraint {
  public:
    BoundaryConstraint(double lower, double upper);
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
using QuantLib::Optimization::Method;
using QuantLib::Optimization::ConjugateGradient;
using QuantLib::Optimization::Simplex;
using QuantLib::Optimization::SteepestDescent;
%}

class Method {
    #if defined(SWIGRUBY)
    %rename("initialValue=") setInitialValue;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("initial-value-set!") setInitialValue;
    #endif
  private:
    // prevent direct instantiation
    Method();
  public:
    void setInitialValue(const Array&);
};

class ConjugateGradient : public Method {
  public:
    ConjugateGradient();
};

class Simplex : public Method {
  public:
    Simplex(double lambda, double tol);
};

class SteepestDescent : public Method {
  public:
    SteepestDescent();
};


%{
using QuantLib::Optimization::Problem;
%}

%inline %{
    class Optimizer {};
%}
#if defined(SWIGPYTHON)
%extend Optimizer {
    Array solve(PyObject* function, Constraint& c, Method& m) {
        PyCostFunction f(function);
        Problem p(f,c,m);
        p.minimize();
        return p.minimumValue();
    }
}
#elif defined(SWIGRUBY)
%extend Optimizer {
    Array solve(Constraint& c, Method& m) {
        RubyCostFunction f;
        Problem p(f,c,m);
        p.minimize();
        return p.minimumValue();
    }
}
#elif defined(SWIGMZSCHEME)
%extend Optimizer {
    Array solve(Scheme_Object* function, Constraint& c, Method& m) {
        MzCostFunction f(function);
        Problem p(f,c,m);
        p.minimize();
        return p.minimumValue();
    }
}
#elif defined(SWIGGUILE)
%extend Optimizer {
    Array solve(SCM function, Constraint& c, Method& m) {
        GuileCostFunction f(function);
        Problem p(f,c,m);
        p.minimize();
        return p.minimumValue();
    }
}
#endif


#endif
