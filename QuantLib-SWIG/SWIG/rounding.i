
/*
 Copyright (C) 2004 StatPro Italia srl
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

#ifndef quantlib_rounding_i
#define quantlib_rounding_i

%include common.i
%include types.i

%{
using QuantLib::Rounding;
using QuantLib::UpRounding;
using QuantLib::DownRounding;
using QuantLib::ClosestRounding;
using QuantLib::CeilingTruncation;
using QuantLib::FloorTruncation;
%}

class Rounding {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) \
     || defined(SWIGCSHARP) || defined(SWIGPERL)
    %rename(apply) operator();
    #endif
  public:
    Rounding();
    Decimal operator()(Decimal value) const;
};


class UpRounding : public Rounding {
  public:
    UpRounding(Integer precision, Integer digit = 5);
};

class DownRounding : public Rounding {
  public:
    DownRounding(Integer precision, Integer digit = 5);
};

class ClosestRounding : public Rounding {
  public:
    ClosestRounding(Integer precision, Integer digit = 5);
};

class CeilingTruncation : public Rounding {
  public:
    CeilingTruncation(Integer precision, Integer digit = 5);
};

class FloorTruncation : public Rounding {
  public:
    FloorTruncation(Integer precision, Integer digit = 5);
};


#endif
