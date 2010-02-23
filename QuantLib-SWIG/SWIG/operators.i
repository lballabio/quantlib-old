
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_operators_i
#define quantlib_operators_i

%include common.i
%include linearalgebra.i

%{
typedef QuantLib::BoundaryCondition<QuantLib::TridiagonalOperator>
        DefaultBoundaryCondition;
%}

#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_BoundaryCondition) DefaultBoundaryCondition;
#else
%ignore DefaultBoundaryCondition;
#endif
class DefaultBoundaryCondition {
  public:
    enum Side { None, Upper, Lower };
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
  private:
    DefaultBoundaryCondition();
#endif
};
%template(BoundaryCondition) boost::shared_ptr<DefaultBoundaryCondition>;
%extend boost::shared_ptr<DefaultBoundaryCondition> {
    static const DefaultBoundaryCondition::Side NoSide =
        DefaultBoundaryCondition::None;
    static const DefaultBoundaryCondition::Side Upper =
        DefaultBoundaryCondition::Upper;
    static const DefaultBoundaryCondition::Side Lower =
        DefaultBoundaryCondition::Lower;
}

%{
using QuantLib::NeumannBC;
using QuantLib::DirichletBC;
typedef boost::shared_ptr<DefaultBoundaryCondition> NeumannBCPtr;
typedef boost::shared_ptr<DefaultBoundaryCondition> DirichletBCPtr;
%}

%rename(NeumannBC) NeumannBCPtr;
class NeumannBCPtr: public boost::shared_ptr<DefaultBoundaryCondition> {
  public:
    %extend {
        NeumannBCPtr(Real value, DefaultBoundaryCondition::Side side) {
            return new NeumannBCPtr(new NeumannBC(value, side));
        }
    }
};

%rename(DirichletBC) DirichletBCPtr;
class DirichletBCPtr: public boost::shared_ptr<DefaultBoundaryCondition> {
  public:
    %extend {
        DirichletBCPtr(Real value, DefaultBoundaryCondition::Side side) {
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
    void setFirstRow(Real, Real);
    void setMidRow(Size, Real, Real, Real);
    void setMidRows(Real, Real, Real);
    void setLastRow(Real, Real);
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
        TridiagonalOperator __mul__(Real a) {
            return *self*a;
        }
        TridiagonalOperator __div__(Real a) {
            return *self/a;
        }
        #if defined(SWIGPYTHON)
        TridiagonalOperator __iadd__(const TridiagonalOperator& O) {
            return *self+O;
        }
        TridiagonalOperator __isub__(const TridiagonalOperator& O) {
            return *self-O;
        }
        TridiagonalOperator __imul__(Real a) {
            return *self*a;
        }
        TridiagonalOperator __rmul__(Real a) {
            return *self*a;
        }
        TridiagonalOperator __idiv__(Real a) {
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
                                                Real x) {
        return p*x;
    }
    TridiagonalOperator TridiagonalOperator_div(const TridiagonalOperator& p,
                                                Real x) {
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
    TridiagonalOperator makeDPlus(Size gridPoints, Real h) {
        return DPlus(gridPoints,h);
    }
    TridiagonalOperator makeDMinus(Size gridPoints, Real h) {
        return DMinus(gridPoints,h);
    }
    TridiagonalOperator makeDZero(Size gridPoints, Real h) {
        return DZero(gridPoints,h);
    }
    TridiagonalOperator makeDPlusDMinus(Size gridPoints, Real h) {
        return DPlusDMinus(gridPoints,h);
    }
%}
#else
class DPlus : public TridiagonalOperator {
  public:
    DPlus(Size gridPoints, Real h);
};
class DMinus : public TridiagonalOperator {
  public:
    DMinus(Size gridPoints, Real h);
};
class DZero : public TridiagonalOperator {
  public:
    DZero(Size gridPoints, Real h);
};
class DPlusDMinus : public TridiagonalOperator {
  public:
    DPlusDMinus(Size gridPoints, Real h);
};
#endif


#endif
