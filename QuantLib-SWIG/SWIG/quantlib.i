
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

#if defined(SWIGPYTHON) || defined(SWIGMZSCHEME)
%module QuantLib
#elif defined(SWIGRUBY)
%module QuantLibc
#endif

%include exception.i

%exception {
    try {
        $action
    } catch (IndexError& e) {
        SWIG_exception(SWIG_IndexError,const_cast<char*>(e.what()));
    } catch (std::exception& e) {
        SWIG_exception(SWIG_RuntimeError,const_cast<char*>(e.what()));
    } catch (...) {
        SWIG_exception(SWIG_UnknownError,"unknown error");
    }
}

#if defined(SWIGPYTHON)

%{
#include <ql/quantlib.hpp>
const int    __hexversion__ = QL_HEX_VERSION;
const char* __version__    = QL_VERSION;
%}

const int __hexversion__;
%immutable;
const char* __version__;
%mutable;

#endif


%include ql.i

namespace std {
    %template(IntVector)    vector<int>;
    %template(DoubleVector) vector<double>;
    %template(DateVector  ) vector<Date>;
}

%inline%{
    std::vector<int> foo_int(std::vector<int> v) { return v; }
    const std::vector<int>& bar_int(const std::vector<int>& v) { return v; }
    std::vector<int>* baz_int(std::vector<int>* v) { return v; }

    std::vector<double> foo_double(std::vector<double> v) { return v; }
    const std::vector<double>& bar_double(const std::vector<double>& v) { return v; }
    std::vector<double>* baz_double(std::vector<double>* v) { return v; }

    std::vector<Date> foo_date(std::vector<Date> v) { return v; }
    const std::vector<Date>& bar_date(const std::vector<Date>& v) { return v; }
    std::vector<Date>* baz_date(std::vector<Date>* v) { return v; }
%}

