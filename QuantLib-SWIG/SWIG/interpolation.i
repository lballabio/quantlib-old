
/*
 Copyright (C) 2002 Ferdinando Ametrano
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

#ifndef quantlib_interpolation_i
#define quantlib_interpolation_i

%include linearalgebra.i

%{
// safe versions which copy their arguments
class Interpolation {
  public:
    Interpolation(const Array& x, const Array& y)
    : x_(x), y_(y) {}
    double operator()(double x, bool allowExtrapolation=false) { 
        return (*f_)(x, allowExtrapolation); 
    }
  protected:
    Array x_, y_;
    Handle<QuantLib::Math::Interpolation<Array::const_iterator,
                                         Array::const_iterator> > f_;
};

class Interpolation2D {
  public:
    Interpolation2D(const Array& x, const Array& y, const Matrix& m)
    : x_(x), y_(y), m_(m) {}
    double operator()(double x, double y, bool allowExtrapolation=false) { 
        return (*f_)(x,y, allowExtrapolation); 
    }
  protected:
    Array x_, y_;
    Matrix m_;
    Handle<QuantLib::Math::Interpolation2D<Array::const_iterator,
                                           Array::const_iterator,
                                           Matrix> > f_;
};


class LinearInterpolation : public Interpolation {
  public:
    LinearInterpolation(const Array& x, const Array& y)
    : Interpolation(x,y) {
        f_ = Handle<QuantLib::Math::Interpolation<Array::const_iterator,
                                                  Array::const_iterator> >(
            new QuantLib::Math::LinearInterpolation<Array::const_iterator,
                                                    Array::const_iterator>(
                x_.begin(),x_.end(),y_.begin()));
    }
};

class CubicSpline : public Interpolation {
  public:
    CubicSpline(const Array& x, const Array& y)
    : Interpolation(x,y) {
        f_ = Handle<QuantLib::Math::Interpolation<Array::const_iterator,
                                                  Array::const_iterator> >(
            new QuantLib::Math::CubicSpline<Array::const_iterator,
                                            Array::const_iterator>(
                x_.begin(),x_.end(),y_.begin()));
    }
};

class LogLinearInterpolation : public Interpolation {
  public:
    LogLinearInterpolation(const Array& x, const Array& y)
    : Interpolation(x,y) {
        f_ = Handle<QuantLib::Math::Interpolation<Array::const_iterator,
                                                  Array::const_iterator> >(
            new QuantLib::Math::LogLinearInterpolation<Array::const_iterator,
                                                       Array::const_iterator>(
                x_.begin(),x_.end(),y_.begin()));
    }
};


class BilinearInterpolation : public Interpolation2D {
  public:
    BilinearInterpolation(const Array& x, const Array& y, const Matrix& m)
    : Interpolation2D(x,y,m) {
        f_ = Handle<QuantLib::Math::Interpolation2D<Array::const_iterator,
                                                    Array::const_iterator,
                                                    Matrix> >(
            new QuantLib::Math::BilinearInterpolation<Array::const_iterator,
                                                      Array::const_iterator,
                                                      Matrix>(
                x_.begin(),x_.end(),y_.begin(),y_.end(),m_));
    }
};
%}


#if defined(SWIGPYTHON) || defined(SWIGRUBY)
%rename(__call__) operator();
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(call)     operator();
#endif

class Interpolation {
  private:
    Interpolation();
  public:
    double operator()(double x, bool allowExtrapolation=false);
};

class LinearInterpolation : public Interpolation {
  public:
    LinearInterpolation(const Array& x, const Array& y);
};

class LogLinearInterpolation : public Interpolation {
  public:
    LogLinearInterpolation(const Array& x, const Array& y);
};

class CubicSpline : public Interpolation {
  public:
    CubicSpline(const Array& x, const Array& y);
};


class Interpolation2D {
  private:
    Interpolation2D();
  public:
    double operator()(double x, double y, bool allowExtrapolation=false);
};

class BilinearInterpolation : public Interpolation2D {
  public:
    BilinearInterpolation(const Array& x, const Array& y, const Matrix& m);
};


#endif
