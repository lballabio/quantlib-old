
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

#ifndef quantlib_common_i
#define quantlib_common_i

%include string.i

// typemap a C++ type to integers in the scripting language

%define MapToInteger(Type)
#if defined(SWIGPYTHON)

%typemap(in) Type {
    if (PyInt_Check($input)) {
        $1 = Type(PyInt_AsLong($input));
    } else {
        PyErr_SetString(PyExc_TypeError,"int expected");
        return NULL;
    }
};

%typemap(out) Type {
    $result = PyInt_FromLong(long($1));
};

#elif defined(SWIGRUBY)

%typemap(in) Type {
    if (FIXNUM_P($input)) {
        $1 = Type(FIX2INT($input));
    } else {
        rb_raise(rb_eTypeError, "not an integer");
    }
};

%typemap(out) Type {
    $result = INT2NUM(int($1));
};

#elif defined(SWIGMZSCHEME)

%typemap(in) Type {
    if (SCHEME_INTP($input)) {
        $1 = Type(SCHEME_INT_VAL($input));
    } else {
        scheme_wrong_type("$name", "MZ_NAME", $argnum, argc, argv);
    }
};

%typemap(out) Type {
    $result = scheme_make_integer_value($1);
};

#endif
%enddef


// typemap a C++ type to strings in the scripting language

%define MapToString(Type,TypeFromString,TypeToString)
#if defined(SWIGPYTHON)

%typemap(in) Type {
    if (PyString_Check($input)) {
        std::string s(PyString_AsString($input));
        try {
            $1 = TypeFromString(s);
        } catch (Error&) {
            PyErr_SetString(PyExc_TypeError,"Type" " expected");
            return NULL;
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"Type" " expected");
        return NULL;
    }
};

%typemap(out) Type {
    $result = PyString_FromString(TypeToString($1).c_str());
};

#elif defined(SWIGRUBY)

%typemap(in) Type {
    if (TYPE($input) == T_STRING) {
        std::string s(STR2CSTR($input));
        try {
            $1 = TypeFromString(s);
        } catch (Error&) {
            rb_raise(rb_eTypeError,"not a " "Type");
        }
    } else {
        rb_raise(rb_eTypeError,"not a " "Type");
    }
};

%typemap(out) Type {
    $result = rb_str_new2(TypeToString($1).c_str());
};

#elif defined(SWIGMZSCHEME)

%typemap(in) Type {
    if (SCHEME_STRINGP($input)) {
        std::string s(SCHEME_STR_VAL($input));
        try {
            $1 = TypeFromString(s);
        } catch (Error&) {
            scheme_wrong_type("$name", "MZ_NAME", $argnum, argc, argv);
        }
    } else {
        scheme_wrong_type("$name", "MZ_NAME", $argnum, argc, argv);
    }
};

%typemap(out) Type {
    $result = scheme_make_string(TypeToString($1).c_str());
};

#endif
%enddef


#endif
