/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl

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
%{
using QuantLib::Surface;
using QuantLib::Domain;
%}

%ignore Surface;
class Surface {
Real operator()(Real x, Real y);
boost::shared_ptr<Domain> domain();
};

%template(Surface) boost::shared_ptr<Surface>;
%extend boost::shared_ptr<Surface> {
Real operator()(Real x, Real y) {
return (**self)(x, y);
}

boost::shared_ptr<Domain> domain() {
return (*self)->domain();
}
}

// Surface
%{
using QuantLib::TestSurface;
typedef boost::shared_ptr<Surface> TestSurfacePtr;
%}

%rename(TestSurface) TestSurfacePtr;
class TestSurfacePtr : public boost::shared_ptr<Surface> {
      public:
%extend {
      TestSurfacePtr() {
      return new TestSurfacePtr(new TestSurface());
      }
}
};
