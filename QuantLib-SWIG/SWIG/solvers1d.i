
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_solver1d_i
#define quantlib_solver1d_i

%include functions.i

// 1D Solver interface

%{
using QuantLib::Solver1D;
using QuantLib::Solvers1D::Bisection;
using QuantLib::Solvers1D::Brent;
using QuantLib::Solvers1D::FalsePosition;
using QuantLib::Solvers1D::Newton;
using QuantLib::Solvers1D::NewtonSafe;
using QuantLib::Solvers1D::Ridder;
using QuantLib::Solvers1D::Secant;
%}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("max-evaluations-set!") Solver1D::setMaxEvaluations;
%rename("lower-bound-set!")     Solver1D::setLowerBound;
%rename("upper-bound-set!")     Solver1D::setUpperBound;
%rename("bracketed-solve")      Solver1D::bracketedSolve;
#endif

#if defined(SWIGRUBY)
%rename("maxEvaluations=") Solver1D::setMaxEvaluations;
%rename("lowerBound=")     Solver1D::setLowerBound;
%rename("upperBound=")     Solver1D::setUpperBound;
#endif

class Solver1D {
  private:
    Solver1D();
  public:
    void setMaxEvaluations(int evaluations);
    void setLowerBound(double lowerBound);
    void setUpperBound(double upperBound);
};

%extend Solver1D {
    #if defined(SWIGPYTHON)
    double solve(PyObject* function, double xAccuracy, double guess,
                 double step) {
        PyObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, step);
    }
    double bracketedSolve(PyObject* function, double xAccuracy,
                          double guess, double xMin, double xMax) {
        PyObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, xMin, xMax);
    }
    #elif defined(SWIGRUBY)
    double solve(double xAccuracy, double guess, double step) {
        RubyObjectiveFunction f;
        return self->solve(f, xAccuracy, guess, step);
    }
    double bracketedSolve(double xAccuracy, double guess,
                          double xMin, double xMax) {
        RubyObjectiveFunction f;
        return self->solve(f, xAccuracy, guess, xMin, xMax);
    }
    #elif defined(SWIGMZSCHEME)
    double solve(Scheme_Object* function, double xAccuracy, double guess,
                 double step) {
        MzObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, step);
    }
    double bracketedSolve(Scheme_Object* function, double xAccuracy,
                          double guess, double xMin, double xMax) {
        MzObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, xMin, xMax);
    }
    #elif defined(SWIGGUILE)
    double solve(SCM function, double xAccuracy, double guess,
                 double step) {
        GuileObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, step);
    }
    double bracketedSolve(SCM function, double xAccuracy,
                          double guess, double xMin, double xMax) {
        GuileObjectiveFunction f(function);
        return self->solve(f, xAccuracy, guess, xMin, xMax);
    }
    #endif
}


// Actual solvers

class Brent : public Solver1D {
  public:
    Brent();
    ~Brent();
};

class Bisection : public Solver1D {
  public:
    Bisection();
    ~Bisection();
};

class FalsePosition : public Solver1D {
  public:
    FalsePosition();
    ~FalsePosition();
};

class Ridder : public Solver1D {
  public:
    Ridder();
    ~Ridder();
};

class Secant : public Solver1D {
  public:
    Secant();
    ~Secant();
};

#if defined(SWIGPYTHON)
// these two need f.derivative()
class Newton : public Solver1D {
  public:
    Newton();
    ~Newton();
};

class NewtonSafe : public Solver1D {
  public:
    NewtonSafe();
    ~NewtonSafe();
};
#endif


#endif
