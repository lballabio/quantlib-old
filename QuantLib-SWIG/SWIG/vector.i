
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

#ifndef quantlib_vector_i
#define quantlib_vector_i

%include types.i
%include string.i

%{
#include <vector>
using std::vector;
using QuantLib::Null;
%}

#if defined(SWIGPYTHON)

%typemap(in) vector<double> {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            PyObject* o = PySequence_GetItem($input,i);
            if (o == Py_None) {
                $1[i] = Null<double>();
                Py_DECREF(o);
            } else if (PyFloat_Check(o)) {
                $1[i] = PyFloat_AsDouble(o);
                Py_DECREF(o);
            } else if (PyInt_Check(o)) {
                $1[i] = double(PyInt_AsLong(o));
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"number sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"number sequence expected");
        return NULL;
    }
}

%typemap(in) const vector<double>& (vector<double> temp) {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            PyObject* o = PySequence_GetItem($input,i);
            if (o == Py_None) {
                temp[i] = Null<double>();
                Py_DECREF(o);
            } else if (PyFloat_Check(o)) {
                temp[i] = PyFloat_AsDouble(o);
                Py_DECREF(o);
            } else if (PyInt_Check(o)) {
                temp[i] = double(PyInt_AsLong(o));
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"number sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"number sequence expected");
        return NULL;
    }
}

%typemap(out) vector<double> {
    $result = PyTuple_New($1.size());
    for (Size i=0; i<$1.size(); i++)
        PyTuple_SetItem($result,i,PyFloat_FromDouble($1[i]));
}



%typemap(in) vector<int> {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            PyObject* o = PySequence_GetItem($input,i);
            if (o == Py_None) {
                $1[i] = Null<int>();
                Py_DECREF(o);
            } else if (PyInt_Check(o)) {
                $1[i] = int(PyInt_AsLong(o));
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"integer sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"integer sequence expected");
        return NULL;
    }
}

%typemap(in) const vector<int>& (vector<int> temp) {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            PyObject* o = PySequence_GetItem($input,i);
            if (o == Py_None) {
                temp[i] = Null<int>();
                Py_DECREF(o);
            } else if (PyInt_Check(o)) {
                temp[i] = int(PyInt_AsLong(o));
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"integer sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"integer sequence expected");
        return NULL;
    }
}

%typemap(out) vector<int> {
    $result = PyTuple_New($1.size());
    for (Size i=0; i<$1.size(); i++)
        PyTuple_SetItem($result,i,PyInt_FromLong(long($1[i])));
}


%define TypemapVector(T)

%typemap(in) vector<T> {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            T* x;
            PyObject* o = PySequence_GetItem($input,i);
            if ((SWIG_ConvertPtr(o,(void **) &x,
                                 SWIGTYPE_p_##T,0)) != -1) {
                $1[i] = *x;
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"T" " sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"T" " sequence expected");
        return NULL;
    }
}

%typemap(in) const vector<T>& (vector<T> temp) {
    if (PyTuple_Check($input) || PyList_Check($input)) {
        Size size = (PyTuple_Check($input) ?
                     PyTuple_Size($input) :
                     PyList_Size($input));
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            T* x;
            PyObject* o = PySequence_GetItem($input,i);
            if ((SWIG_ConvertPtr(o,(void **) &x,
                                 SWIGTYPE_p_##T,0)) != -1) {
                temp[i] = *x;
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                PyErr_SetString(PyExc_TypeError,"T" " sequence expected");
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError,"T" " sequence expected");
        return NULL;
    }
}

%typemap(out) vector<T> {
    $result = PyTuple_New($1.size());
    for (Size i=0; i<$1.size(); i++) {
        T* ptr = new T($1[i]);
        PyTuple_SetItem($result,i,
                        SWIG_NewPointerObj((void *) ptr, SWIGTYPE_p_##T, 1));
    }
}
%enddef

#elif defined(SWIGRUBY)

%typemap(in) vector<double> {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (o == Qnil)
                $1[i] = Null<double>();
            else if (TYPE(o) == T_FLOAT)
                $1[i] = NUM2DBL(o);
            else if (FIXNUM_P(o))
                $1[i] = double(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type (expected numbers)");
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected numbers)");
    }
}

%typemap(in) const vector<double>& (vector<double> temp) {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (o == Qnil)
                temp[i] = Null<double>();
            else if (TYPE(o) == T_FLOAT)
                temp[i] = NUM2DBL(o);
            else if (FIXNUM_P(o))
                temp[i] = double(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type (expected numbers)");
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected numbers)");
    }
}

%typemap(out) vector<double> {
    $result = rb_ary_new2($1.size());
    for (Size i=0; i<$1.size(); i++)
        rb_ary_store($result,i,rb_float_new($1[i]));
}


%typemap(in) vector<int> {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (o == Qnil)
                $1[i] = Null<int>();
            else if (FIXNUM_P(o))
                $1[i] = int(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type (expected integers)");
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected integers)");
    }
}

%typemap(in) const vector<int>& (vector<int> temp) {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (o == Qnil)
                temp[i] = Null<int>();
            else if (FIXNUM_P(o))
                temp[i] = int(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type (expected integers)");
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected integers)");
    }
}

%typemap(out) vector<int> {
    $result = rb_ary_new2($1.size());
    for (Size i=0; i<$1.size(); i++)
        rb_ary_store($result,i,INT2NUM($1[i]));
}


%define TypemapVector(T)

%typemap(in) vector<T> {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        $1.resize(size);
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            T* x = (T*) SWIG_ConvertPtr(o, SWIGTYPE_p_##T);
            $1[i] = *x;
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected " "T" "s)");
    }
}

%typemap(in) const vector<T>& (vector<T> temp) {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        Size size = RARRAY($input)->len;
        temp.resize(size);
        $1 = &temp;
        for (Size i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            T* x = (T*) SWIG_ConvertPtr(o, SWIGTYPE_p_##T);
            temp[i] = *x;
        }
    } else {
        rb_raise(rb_eTypeError,
                 "wrong argument type (expected " "T" "s)");
    }
}

%typemap(out) vector<T> {
    $result = rb_ary_new2($1.size());
    for (Size i=0; i<$1.size(); i++) {
        T* x = new T($1[i]);
        rb_ary_store($result,i,
                     SWIG_NewPointerObj((void *) x, SWIGTYPE_p_##T, 1));
    }
}

%enddef

#elif defined(SWIGMZSCHEME)

// to do


#endif

// test
TypemapVector(Date);

%inline %{
vector<int> f1(vector<int> v) { return v; }
vector<int> f2(const vector<int>& v) { return v; }
vector<double> f3(vector<double> v) { return v; }
vector<double> f4(const vector<double>& v) { return v; }
vector<Date> f5(vector<Date> v) { return v; }
vector<Date> f6(const vector<Date>& v) { return v; }
%}




#endif
