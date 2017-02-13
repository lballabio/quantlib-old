
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl

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

#ifndef quantlib_vectors_i
#define quantlib_vectors_i

%include stl.i
%include date.i

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( std::pair<Date,double> )
#endif

namespace std {

    %template(IntVector) vector<int>;
    %template(UnsignedIntVector) vector<unsigned int>;
    %template(DoubleVector) vector<double>;
    %template(StrVector) vector<std::string>;
    %template(BoolVector) vector<bool>;

#if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
    %template(NodePair) pair<Date,double>;
    %template(NodeVector) vector<pair<Date,double> >;
#endif

#if defined(SWIGR)
    swigr_list_converter(IntVector,
    _p_std__vectorTint_std__allocatorTint_t_t,
    integer)

    swigr_list_converter(DoubleVector,
    _p_std__vectorTdouble_std__allocatorTdouble_t_t,
    numeric)

    swigr_list_converter(StrVector,
    _p_std__vectorTstd__string_std__allocatorTstd__string_t_t,
    character)
#endif
}


#endif
