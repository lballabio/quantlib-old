
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

%{
#include <ql/quantlib.hpp>

using QuantLib::Error;
using QuantLib::IndexError;
%}

#if defined(SWIGPYTHON)
%except(python) {
    try {
        $function
    } catch (IndexError& e) {
        PyErr_SetString(PyExc_IndexError,e.what());
        return NULL;
    } catch (Error& e) {
        PyErr_SetString(PyExc_Exception,e.what());
        return NULL;
    } catch (std::exception& e) {
        PyErr_SetString(PyExc_Exception,e.what());
        return NULL;
    } catch (...) {
        PyErr_SetString(PyExc_Exception,"unknown error");
        return NULL;
    }
}

%{
    const int    __hexversion__ = QL_HEX_VERSION;
    const char* __version__    = QL_VERSION;
%}

const int __hexversion__;
%readonly
const char* __version__;
%readwrite
#elif defined(SWIGRUBY)
%except(ruby) {
    try {
        $function
    } catch (IndexError& e) {
        rb_raise(rb_eIndexError,e.what());
    } catch (Error& e) {
        rb_raise(rb_eStandardError,e.what());
    } catch (std::exception& e) {
        rb_raise(rb_eStandardError,e.what());
    } catch (...) {
        rb_raise(rb_eStandardError,"unknown error");
    }
}
#elif defined(SWIGMZSCHEME)
%except(mzscheme) {
    try {
        $function
    } catch (std::exception& e) {
        scheme_signal_error("%s",e.what());
    } catch (...) {
        scheme_signal_error("unknown error");
    }
}
#endif


%include ql.i

