
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

#ifndef quantlib_operators_i
#define quantlib_operators_i

%include common.i
%include linearalgebra.i

%{
using QuantLib::FiniteDifferences::BoundaryCondition;
typedef BoundaryCondition::Type BoundaryConditionType;

BoundaryCondition::Type BCfromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "" || s == "none")
        return BoundaryCondition::None;
    else if (s == "neumann")
        return BoundaryCondition::Neumann;
    else if (s == "dirichlet")
        return BoundaryCondition::Dirichlet;
    else
        throw Error("unknown boundary condition type: "+s);
}

std::string BCtoString(BoundaryCondition::Type type) {
    switch (type) {
      case BoundaryCondition::None:
        return "None";
      case BoundaryCondition::Neumann:
        return "Neumann";
      case BoundaryCondition::Dirichlet:
        return "Dirichlet";
      default:
        throw Error("unknown boundary condition type");
    }
}
%}

MapToString(BoundaryConditionType,BCfromString,BCtoString);

class BoundaryCondition {
  public:
	BoundaryCondition(BoundaryConditionType type, double value);
	BoundaryConditionType type() const;
	double value() const;
};


%{
using QuantLib::FiniteDifferences::TridiagonalOperator;
%}

#if defined(SWIGPYTHON) || defined(SWIGRUBY)
%rename(__add__) add;
%rename(__sub__) sub;
%rename(__mul__) mul;
%rename(__div__) div;
#endif
#if defined(SWIGRUBY)
%rename("lowerBC=")  setLowerBC;
%rename("upperBC=")  setUpperBC;
%rename("firstRow=") setFirstRow;
%rename("midRow=")   setMidRow;
%rename("midRows=")  setMidRows;
%rename("lastRow=")  setLastRow;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("lowerBC-set!")  setLowerBC;
%rename("upperBC-set!")  setUpperBC;
%rename("firstRow-set!") setFirstRow;
%rename("midRow-set!")   setMidRow;
%rename("midRows-set!")  setMidRows;
%rename("lastRow-set!")  setLastRow;
#endif
#if defined(SWIGGUILE)
%scheme %{
    (define TridiagonalOperator+ TridiagonalOperator-add)
    (define TridiagonalOperator- TridiagonalOperator-sub)
    (define TridiagonalOperator* TridiagonalOperator-mul)
    (define TridiagonalOperator/ TridiagonalOperator-div)
    (export TridiagonalOperator+
            TridiagonalOperator-
            TridiagonalOperator*
            TridiagonalOperator/)
%}
#endif
ReturnByValue(TridiagonalOperator);

class TridiagonalOperator {
  public:
    // constructors
    TridiagonalOperator(const Array& low, const Array& mid, const Array& high);
    // operator interface
    Array solveFor(const Array& rhs) const;
    Array applyTo(const Array& v) const;
    // inspectors
    Size size() const;
    // modifiers
    void setLowerBC(const BoundaryCondition& bc);
    void setUpperBC(const BoundaryCondition& bc);
    void setFirstRow(double, double);
    void setMidRow(Size, double, double, double);
    void setMidRows(double, double, double);
    void setLastRow(double, double);
    // identity
    static TridiagonalOperator identity(Size size);
};

%extend TridiagonalOperator {
    TridiagonalOperator add(const TridiagonalOperator& O) {
        return *self+O;
    }
    TridiagonalOperator sub(const TridiagonalOperator& O) {
        return *self-O;
    }
    TridiagonalOperator mul(double a) {
        return *self*a;
    }
    TridiagonalOperator div(double a) {
        return *self/a;
    }
    #if defined(SWIGPYTHON)
    TridiagonalOperator __iadd__(const TridiagonalOperator& O) {
        return *self+O;
    }
    TridiagonalOperator __isub__(const TridiagonalOperator& O) {
        return *self-O;
    }
    TridiagonalOperator __imul__(double a) {
        return *self*a;
    }
    TridiagonalOperator __rmul__(double a) {
        return *self*a;
    }
    TridiagonalOperator __idiv__(double a) {
        return *self/a;
    }
    #endif
};

%{
using QuantLib::FiniteDifferences::DPlus;
using QuantLib::FiniteDifferences::DMinus;
using QuantLib::FiniteDifferences::DZero;
using QuantLib::FiniteDifferences::DPlusDMinus;
%}

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
// TridiagonalOperator doesn't have a virtual destructor:
// let's make sure users won't deallocate derived classes
// with the wrong one
%rename("new-D+")   makeDPlus;
%rename("new-D-")   makeDMinus;
%rename("new-D0")   makeDZero;
%rename("new-D+D-") makeDPlusDMinus;
%inline %{
    TridiagonalOperator makeDPlus(Size gridPoints, double h) {
        return DPlus(gridPoints,h);
    }
    TridiagonalOperator makeDMinus(Size gridPoints, double h) {
        return DMinus(gridPoints,h);
    }
    TridiagonalOperator makeDZero(Size gridPoints, double h) {
        return DZero(gridPoints,h);
    }
    TridiagonalOperator makeDPlusDMinus(Size gridPoints, double h) {
        return DPlusDMinus(gridPoints,h);
    }
%}
#else
class DPlus : public TridiagonalOperator {
  public:
    DPlus(Size gridPoints, double h);
};
class DMinus : public TridiagonalOperator {
  public:
    DMinus(Size gridPoints, double h);
};
class DZero : public TridiagonalOperator {
  public:
    DZero(Size gridPoints, double h);
};
class DPlusDMinus : public TridiagonalOperator {
  public:
    DPlusDMinus(Size gridPoints, double h);
};
#endif


%{
using QuantLib::FiniteDifferences::valueAtCenter;
using QuantLib::FiniteDifferences::firstDerivativeAtCenter;
using QuantLib::FiniteDifferences::secondDerivativeAtCenter;
%}

double valueAtCenter(const Array& a);
double firstDerivativeAtCenter(const Array& a, const Array& g);
double secondDerivativeAtCenter(const Array& a, const Array& g);

%{
using QuantLib::Math::SymmetricEigenvalues;
using QuantLib::Math::SymmetricEigenvectors;
%}

Array SymmetricEigenvalues(Matrix &s);
Matrix SymmetricEigenvectors(Matrix &s);


#endif
