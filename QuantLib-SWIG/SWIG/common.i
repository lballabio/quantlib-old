
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_common_i
#define quantlib_common_i

%include stl.i
%include exception.i

%{
// generally useful classes
using QuantLib::Error;
using QuantLib::RelinkableHandle;
using QuantLib::IntegerFormatter;
using QuantLib::DecimalFormatter;
using QuantLib::StringFormatter;
%}

namespace boost {

    template <class T>
    class shared_ptr {
        #if defined(SWIGRUBY) || defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        %rename("null?") isNull;
        #endif
      public:
        T* operator->();
        #if defined(SWIGPYTHON)
        %extend {
            bool __nonzero__() {
                return !!(*self);
            }
        }
        #else
        %extend {
            bool isNull() {
                return !(*self);
            }
        }
        #endif
    };

}


template <class T>
class RelinkableHandle {
    #if defined(SWIGRUBY)
    %rename("null?")   isNull;
    %rename("linkTo!") linkTo;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("null?")    isNull;
    %rename("link-to!") linkTo;
    #endif
  public:
    RelinkableHandle(const boost::shared_ptr<T>& = boost::shared_ptr<T>());
    boost::shared_ptr<T> operator->();
    void linkTo(const boost::shared_ptr<T>&);
    #if defined(SWIGPYTHON)
    %extend {
        bool __nonzero__() {
            return !(self->isNull());
        }
    }
    #else
    bool isNull();
    #endif
};


// typemap a C++ type to integers in the scripting language

%define MapToInteger(Type)

%typemap(typecheck) Type = int;

#if defined(SWIGPYTHON)

%typemap(in) Type {
    if (PyInt_Check($input))
        $1 = Type(PyInt_AsLong($input));
    else
        SWIG_exception(SWIG_TypeError,"int expected");
};

%typemap(out) Type {
    $result = PyInt_FromLong(long($1));
};

#elif defined(SWIGRUBY)

%typemap(in) Type {
    if (FIXNUM_P($input))
        $1 = Type(FIX2INT($input));
    else
        SWIG_exception(SWIG_TypeError,"not an integer");
};

%typemap(out) Type {
    $result = INT2NUM(int($1));
};

#elif defined(SWIGMZSCHEME)

%typemap(in) Type {
    if (SCHEME_INTP($input))
        $1 = Type(SCHEME_INT_VAL($input));
    else
        SWIG_exception(SWIG_TypeError,"int expected");
};

%typemap(out) Type {
    $result = scheme_make_integer_value(int($1));
};

#elif defined(SWIGGUILE)

%typemap(in) Type {
    $1 = Type(gh_scm2int($input));
};

%typemap(out) Type {
    $result = gh_int2scm(int($1));
};

#endif
%enddef


// typemap a C++ type to strings in the scripting language

%define MapToString(Type,TypeFromString,TypeToString)

%typemap(typecheck) Type = char *;

#if defined(SWIGPYTHON)

%typemap(in) Type {
    if (PyString_Check($input)) {
        std::string s(PyString_AsString($input));
        try {
            $1 = TypeFromString(s);
        } catch (Error&) {
            SWIG_exception(SWIG_TypeError,"Type" " expected");
        }
    } else {
        SWIG_exception(SWIG_TypeError,"Type" " expected");
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
            SWIG_exception(SWIG_TypeError, "not a " "Type");
        }
    } else {
        SWIG_exception(SWIG_TypeError, "not a " "Type");
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
            SWIG_exception(SWIG_TypeError, "Type" " expected");
        }
    } else {
        SWIG_exception(SWIG_TypeError, "Type" " expected");
    }
};

%typemap(out) Type {
    $result = scheme_make_string(TypeToString($1).c_str());
};

#elif defined(SWIGGUILE)

%typemap(in) Type (char* temp) {
    if (gh_string_p($input)) {
        temp = gh_scm2newstr($input, NULL);
        std::string s(temp);
        if (temp) scm_must_free(temp);
        try {
            $1 = TypeFromString(s);
        } catch (Error&) {
            SWIG_exception(SWIG_TypeError, "Type" " expected");
        }
    } else {
        SWIG_exception(SWIG_TypeError, "Type" " expected");
    }
};

%typemap(out) Type {
    $result = gh_str02scm(TypeToString($1).c_str());
};

#endif
%enddef


#endif
