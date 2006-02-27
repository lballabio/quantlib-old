
/*
 Copyright (C) 2006 StatPro Italia srl

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

#ifndef quantlib_callability_i
#define quantlib_callability_i

%include date.i
%include vectors.i

%{
using QuantLib::Price;
using QuantLib::Callability;
using QuantLib::CallabilitySchedule;
%}

class Price {
  public:
    enum Type { Dirty, Clean };
    Price(Real amount, Type type);
    Real amount() const;
    Type type() const;
};

class Callability {
  public:
    enum Type { Call, Put };
    Callability(Price price, Type type, Date date);
    const Price& price() const;
    Type type() const;
    Date date() const;
};

/* This prevents default constructor and resize methods from getting
   wrapped, for swig frontends which use the typemap libary. This may
   benecessary for frontends other than R */

#if defined(SWIGR)
%std_nodefconst_type(Callability)
#endif

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_SPECIALIZE( Callability, Callability )
#endif


namespace std {
    %template(CallabilityVector) vector<Callability>;
}

typedef std::vector<Callability> CallabilitySchedule;

#endif
