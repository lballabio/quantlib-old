
/*
 Copyright (C) 2006 Joseph Wang

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

#ifndef quantlib_sampled_curve_i
#define quantlib_sampled_curve_i

%include linearalgebra.i

class SampledCurve {
  public:
    SampledCurve();
    SampledCurve(const Array&);
    Array& grid();
    Array& values();
    Real gridValue(Size i);
    Real value(Size i);
    Size size() const;
    bool empty() const;
    void setGrid(const Array&);
    void setValues(const Array&);
    void swap(SampledCurve&);
    void setLogGrid(Real min, Real max);
    void regridLogGrid(Real min, Real max);
    void shiftGrid(Real s);
    void scaleGrid(Real s);
    void regrid(const Array &);
};


#endif
