
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

#ifndef quantlib_operators_i
#define quantlib_operators_i

%include common.i
%include linearalgebra.i

%{
typedef QuantLib::BoundaryCondition<QuantLib::TridiagonalOperator>
		BoundaryCondition;
typedef BoundaryCondition::Side BoundaryConditionSide;

BoundaryCondition::Side BCSideFromString(std::string s) {
    s = StringFormatter::toLowercase(s);
    if (s == "" || s == "none")
        return BoundaryCondition::None;
    else if (s == "upper")
        return BoundaryCondition::Upper;
    else if (s == "lower")
        return BoundaryCondition::Lower;
    else
        throw Error("unknown boundary condition side: "+s);
}

std::string BCSideToString(BoundaryCondition::Side type) {
    switch (type) {
      case BoundaryCondition::None:
        return "None";
      case BoundaryCondition::Upper:
        return "upper";
      case BoundaryCondition::Lower:
        return "lower";
      default:
        throw Error("unknown boundary condition side");
    }
}
%}

MapToString(BoundaryConditionSide,BCSideFromString,BCSideToString);

%ignore BoundaryCondition;
class BoundaryCondition {};
%template(BoundaryCondition) boost::shared_ptr<BoundaryCondition>;

%{
using QuantLib::NeumannBC;
using QuantLib::DirichletBC;
typedef boost::shared_ptr<BoundaryCondition> NeumannBCPtr;
typedef boost::shared_ptr<BoundaryCondition> DirichletBCPtr;
%}

%rename(NeumannBC) NeumannBCPtr;
class NeumannBCPtr: public boost::shared_ptr<BoundaryCondition> {
  public:
    %extend {
        NeumannBCPtr(double value, BoundaryConditionSide side) {
            return new NeumannBCPtr(new NeumannBC(value, side));
        }
    }
};

%rename(DirichletBC) DirichletBCPtr;
class DirichletBCPtr: public boost::shared_ptr<BoundaryCondition> {
  public:
    %extend {
        DirichletBCPtr(double value, BoundaryConditionSide side) {
            return new DirichletBCPtr(new DirichletBC(value, side));
        }
    }
};



%{
using QuantLib::TridiagonalOperator;
%}

class TridiagonalOperator {
    #if defined(SWIGRUBY)
    %rename("firstRow=")      setFirstRow;
    %rename("midRow=")        setMidRow;
    %rename("midRows=")       setMidRows;
    %rename("lastRow=")       setLastRow;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("solve-for")      solveFor;
    %rename("apply-to")       applyTo;
    %rename("first-row-set!") setFirstRow;
    %rename("mid-row-set!")   setMidRow;
    %rename("mid-rows-set!")  setMidRows;
    %rename("last-row-set!")  setLastRow;
    #endif
  public:
    // constructors
    TridiagonalOperator(const Array& low, const Array& mid, const Array& high);
    // operator interface
    Array solveFor(const Array& rhs) const;
    Array applyTo(const Array& v) const;
    // inspectors
    Size size() const;
    // modifiers
    void setFirstRow(double, double);
    void setMidRow(Size, double, double, double);
    void setMidRows(double, double, double);
    void setLastRow(double, double);
    // identity
    static TridiagonalOperator identity(Size size);
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    %extend {
        TridiagonalOperator __add__(const TridiagonalOperator& O) {
            return *self+O;
        }
        TridiagonalOperator __sub__(const TridiagonalOperator& O) {
            return *self-O;
        }
        TridiagonalOperator __mul__(double a) {
            return *self*a;
        }
        TridiagonalOperator __div__(double a) {
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
    }
    #endif
};

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("TridiagonalOperator+") TridiagonalOperator_add;
%rename("TridiagonalOperator-") TridiagonalOperator_sub;
%rename("TridiagonalOperator*") TridiagonalOperator_mul;
%rename("TridiagonalOperator/") TridiagonalOperator_div;
%inline %{
    TridiagonalOperator TridiagonalOperator_add(const TridiagonalOperator& p,
                                                const TridiagonalOperator& q) {
        return p+q;
    }
    TridiagonalOperator TridiagonalOperator_sub(const TridiagonalOperator& p,
                                                const TridiagonalOperator& q) {
        return p-q;
    }
    TridiagonalOperator TridiagonalOperator_mul(const TridiagonalOperator& p,
                                                double x) {
        return p*x;
    }
    TridiagonalOperator TridiagonalOperator_div(const TridiagonalOperator& p,
                                                double x) {
        return p/x;
    }
%}
#endif


%{
using QuantLib::DPlus;
using QuantLib::DMinus;
using QuantLib::DZero;
using QuantLib::DPlusDMinus;
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
using QuantLib::valueAtCenter;
using QuantLib::firstDerivativeAtCenter;
using QuantLib::secondDerivativeAtCenter;
%}

double valueAtCenter(const Array& a);
double firstDerivativeAtCenter(const Array& a, const Array& g);
double secondDerivativeAtCenter(const Array& a, const Array& g);

%{
using QuantLib::SymmetricEigenvalues;
using QuantLib::SymmetricEigenvectors;
%}

Array SymmetricEigenvalues(Matrix &s);
Matrix SymmetricEigenvectors(Matrix &s);


#endif
