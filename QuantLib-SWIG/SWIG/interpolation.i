
/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano
 Copyright (C) 2000-2004 StatPro Italia srl

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

#ifndef quantlib_interpolation_i
#define quantlib_interpolation_i

%include linearalgebra.i

%{
// safe versions which copy their arguments
template <class I>
class SafeInterpolation {
  public:
    SafeInterpolation(const Array& x, const Array& y)
    : x_(x), y_(y), f_(x_.begin(),x_.end(),y_.begin()) {}
    double operator()(double x, bool allowExtrapolation=false) { 
        return f_(x, allowExtrapolation); 
    }
    Array x_, y_;
    I f_;
};
%}

%define make_safe_interpolation(T,Alias)
%{
typedef SafeInterpolation<QuantLib::T> Safe##T;
%}
%rename(Alias) Safe##T;
class Safe##T {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    Safe##T(const Array& x, const Array& y);
    double operator()(double x, bool allowExtrapolation=false);
};
%enddef

make_safe_interpolation(LinearInterpolation,LinearInterpolation);
make_safe_interpolation(NaturalCubicSpline,CubicSpline);
make_safe_interpolation(NaturalMonotonicCubicSpline,MonotonicCubicSpline);
make_safe_interpolation(LogLinearInterpolation,LogLinearInterpolation);

%define extend_spline(T)
%extend Safe##T {
    double derivative(double x, bool extrapolate = false) {
        return self->f_.derivative(x,extrapolate);
    }
    double secondDerivative(double x, bool extrapolate = false) {
        return self->f_.secondDerivative(x,extrapolate);
    }
}
%enddef

extend_spline(NaturalCubicSpline);
extend_spline(NaturalMonotonicCubicSpline);

%{
// safe versions which copy their arguments
template <class I>
class SafeInterpolation2D {
  public:
    SafeInterpolation2D(const Array& x, const Array& y, const Matrix& m)
    : x_(x), y_(y), m_(m), f_(x_.begin(),x_.end(),y_.begin(),y_.end(),m_) {}
    double operator()(double x, double y, bool allowExtrapolation=false) {
        return f_(x,y, allowExtrapolation);
    }
  protected:
    Array x_, y_;
    Matrix m_;
    I f_;
};
%}

%define make_safe_interpolation2d(T,Alias)
%{
typedef SafeInterpolation2D<QuantLib::T> Safe##T;
%}
%rename(Alias) Safe##T;
class Safe##T {
    #if defined(SWIGRUBY)
    %rename(__call__) operator();
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename(call)     operator();
    #endif
  public:
    Safe##T(const Array& x, const Array& y, const Matrix& m);
    double operator()(double x, double y, bool allowExtrapolation=false);
};
%enddef

make_safe_interpolation2d(BilinearInterpolation,BilinearInterpolation);
make_safe_interpolation2d(BicubicSpline,BicubicSpline);


#endif
