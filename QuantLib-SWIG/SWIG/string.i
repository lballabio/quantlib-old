
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

#ifndef quantlib_string_i
#define quantlib_string_i

%{
using std::string;
%}

#if defined(SWIGPYTHON)

%typemap(in) string {
    if (PyString_Check($input)) {
        $1 = std::string(PyString_AsString($input));
    } else {
        PyErr_SetString(PyExc_TypeError,"string expected");
        return NULL;
    }
};

%typemap(in) const string & (string temp) {
    if (PyString_Check($input)) {
        temp = std::string(PyString_AsString($input));
        $1 = &temp;
    } else {
        PyErr_SetString(PyExc_TypeError,"string expected");
        return NULL;
    }
};

%typemap(out) string {
    $result = PyString_FromString($1.c_str());
};

%typemap(out) const string & {
    $result = PyString_FromString($1->c_str());
};

#elif defined(SWIGRUBY)

%typemap(in) string {
    if (TYPE($input) == T_STRING) {
        $1 = std::string(STR2CSTR($input));
    } else {
        rb_raise(rb_eTypeError, "not a string");
    }
};

%typemap(in) const string & (string temp) {
    if (TYPE($input) == T_STRING) {
        temp = std::string(STR2CSTR($input));
        $1 = &temp;
    } else {
        rb_raise(rb_eTypeError, "not a string");
    }
};

%typemap(out) string {
    $result = rb_str_new2($1.c_str());
};

%typemap(out) const string & {
    $result = rb_str_new2($1->c_str());
};

#elif defined(SWIGMZSCHEME)

%typemap(in) string {
    if (SCHEME_STRINGP($input)) {
        $1 = std::string(SCHEME_STR_VAL($input));
    } else {
        scheme_wrong_type("$name", "MZ_NAME", $argnum, argc, argv);
    }
};

%typemap(in) const string & (string temp) {
    if (SCHEME_STRINGP($input)) {
        temp = std::string(SCHEME_STR_VAL($input));
        $1 = &temp;
    } else {
        scheme_wrong_type("$name", "MZ_NAME", $argnum, argc, argv);
    }
};

%typemap(out) string {
    $result = scheme_make_string($1.c_str());
};

%typemap(out) const string & {
    $result = scheme_make_string($1->c_str());
};
#endif


#endif
