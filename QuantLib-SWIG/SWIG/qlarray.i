
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

#ifndef quantlib_array_i
#define quantlib_array_i

%include common.i
%include types.i
%include stl.i

%{
using QuantLib::Array;
using QuantLib::ArrayFormatter;
using QuantLib::IndexError;
%}

#if defined(SWIGPYTHON)
%{
bool extractArray(PyObject* source, Array* target) {
    if (PyTuple_Check(source) || PyList_Check(source)) {
        int size = (PyTuple_Check(source) ?
                    PyTuple_Size(source) :
                    PyList_Size(source));
        *target = Array(size);
        for (int i=0; i<size; i++) {
            PyObject* o = PySequence_GetItem(source,i);
            if (PyFloat_Check(o)) {
                (*target)[i] = PyFloat_AsDouble(o);
                Py_DECREF(o);
            } else if (PyInt_Check(o)) {
                (*target)[i] = double(PyInt_AsLong(o));
                Py_DECREF(o);
            } else {
                Py_DECREF(o);
                return false;
            }
        }
        return true;
    } else {
        return false;
    }
}
%}

%typemap(in) Array {
    Array* v;
    if (extractArray($input,&$1)) {
        ;
    } else if ((SWIG_ConvertPtr($input,(void **) &v,
                                SWIGTYPE_p_Array,0)) != -1) {
        $1 = *v;
    } else {
        PyErr_SetString(PyExc_TypeError,"Array expected");
        return NULL;
    }
};

%typemap(in) const Array& (Array temp) {
    Array* v;
    if (extractArray($input,&temp)) {
        $1 = &temp;
    } else if ((SWIG_ConvertPtr($input,(void **) &v,
                                SWIGTYPE_p_Array,0)) != -1) {
        $1 = v;
    } else {
        PyErr_SetString(PyExc_TypeError,"Array expected");
        return NULL;
    }
};
#elif defined(SWIGRUBY)
%typemap(in) Array {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        unsigned int size = RARRAY($input)->len;
        $1 = Array(size);
        for (unsigned int i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (TYPE(o) == T_FLOAT)
                (($1_type &)$1)[i] = NUM2DBL(o);
            else if (FIXNUM_P(o))
                (($1_type &)$1)[i] = double(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type"
                         " (expected Array)");
        }
    } else {
        $1 = *(($&1_type) SWIG_ConvertPtr($input,$&1_descriptor));
    }
}
%typemap(in) const Array& (Array temp),
             const Array* (Array temp) {
    if (rb_obj_is_kind_of($input,rb_cArray)) {
        unsigned int size = RARRAY($input)->len;
        temp = Array(size);
        $1 = &temp;
        for (unsigned int i=0; i<size; i++) {
            VALUE o = RARRAY($input)->ptr[i];
            if (TYPE(o) == T_FLOAT)
                temp[i] = NUM2DBL(o);
            else if (FIXNUM_P(o))
                temp[i] = double(FIX2INT(o));
            else
                rb_raise(rb_eTypeError,
                         "wrong argument type"
                         " (expected Array)");
        }
    } else {
        $1 = ($1_ltype) SWIG_ConvertPtr($input,$1_descriptor);
    }
}
#elif defined(SWIGMZSCHEME)
%typemap(in) Array {
    if (SCHEME_VECTORP($input)) {
        unsigned int size = SCHEME_VEC_SIZE($input);
        $1 = Array(size);
        Scheme_Object** items = SCHEME_VEC_ELS($input);
        for (unsigned int i=0; i<size; i++) {
            Scheme_Object* o = items[i];
            if (SCHEME_REALP(o))
                (($1_type &)$1)[i] = scheme_real_to_double(o);
            else
                scheme_wrong_type(FUNC_NAME, "Array", 
                                  $argnum, argc, argv);
        }
    } else {
        $1 = *(($&1_type)
               SWIG_MustGetPtr($input,$&1_descriptor,$argnum));
    }
}
%typemap(in) const Array& (Array temp),
             const Array* (Array temp) {
    if (SCHEME_VECTORP($input)) {
        unsigned int size = SCHEME_VEC_SIZE($input);
        temp = Array(size);
        $1 = &temp;
        Scheme_Object** items = SCHEME_VEC_ELS($input);
        for (unsigned int i=0; i<size; i++) {
            Scheme_Object* o = items[i];
            if (SCHEME_REALP(o))
                temp[i] = scheme_real_to_double(o);
            else
                scheme_wrong_type(FUNC_NAME, "Array", 
                                  $argnum, argc, argv);
        }
    } else {
        $1 = ($1_ltype) SWIG_MustGetPtr($input,$1_descriptor,$argnum);
    }
}
#elif defined(SWIGGUILE)
%typemap(in) Array {
    if (gh_vector_p($input)) {
        unsigned long size = gh_vector_length($input);
        $1 = Array(size);
        double* data = gh_scm2doubles($input,NULL);
        std::copy(data,data+size,$1.begin());
        free(data);
    } else {
        $1 = *(($&1_type)
               SWIG_MustGetPtr($input,$&1_descriptor,$argnum));
    }
}
%typemap(in) const Array& (Array temp),
             const Array* (Array temp) {
    if (gh_vector_p($input)) {
        unsigned long size = gh_vector_length($input);
        temp = Array(size);
        $1 = &temp;
        double* data = gh_scm2doubles($input,NULL);
        std::copy(data,data+size,temp.begin());
        free(data);
    } else {
        $1 = ($1_ltype) SWIG_MustGetPtr($input,$1_descriptor,$argnum);
    }
}
#endif

#if defined(SWIGPYTHON) || defined(SWIGRUBY)
%rename(__len__) size;
#endif

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("length")  size;
%rename(">string") __str__;
%rename("set!")    set;
#endif
ReturnByValue(Array);

class Array {
  public:
    #if !defined(SWIGPYTHON)
    Array(Size n, double fill = 0);
    #endif
    Size size() const;
};

%extend Array {
    std::string __str__() {
        return ArrayFormatter::toString(*self);
    }

    #if defined(SWIGPYTHON)
    Array(const Array& a) {
        return new Array(a);
    }
    Array __getslice__(int i, int j) {
        int size_ = static_cast<int>(self->size());
        if (i<0)
            i = size_+i;
        if (j<0)
            j = size_+j;
        i = QL_MAX(0,i);
        j = QL_MIN(size_,j);
        Array tmp(j-i);
        std::copy(self->begin()+i,self->begin()+j,tmp.begin());
        return tmp;
    }
    void __setslice__(int i, int j, const Array& rhs) {
        int size_ = static_cast<int>(self->size());
        if (i<0)
            i = size_+i;
        if (j<0)
            j = size_+j;
        i = QL_MAX(0,i);
        j = QL_MIN(size_,j);
        QL_ENSURE(static_cast<int>(rhs.size()) == j-i,
            "Arrays are not resizable");
        std::copy(rhs.begin(),rhs.end(),self->begin()+i);
    }
    bool __nonzero__() {
        return (self->size() != 0);
    }
    #endif

    #if defined(SWIGRUBY)
    void each() {
        for (int i=0; i<self->size(); i++)
            rb_yield(rb_float_new((*self)[i]));
    }
    #endif

    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    double __getitem__(int i) {
        int size_ = static_cast<int>(self->size());
        if (i>=0 && i<size_) {
            return (*self)[i];
        } else if (i<0 && -i<=size_) {
            return (*self)[size_+i];
        } else {
            throw IndexError("Array index out of range");
        }
        QL_DUMMY_RETURN(0.0)
    }
    void __setitem__(int i, double x) {
        int size_ = static_cast<int>(self->size());
        if (i>=0 && i<size_) {
            (*self)[i] = x;
        } else if (i<0 && -i<=size_) {
            (*self)[size_+i] = x;
        } else {
            throw IndexError("Array index out of range");
        }
    }
    #endif

    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    double ref(Size i) {
        if (i<self->size())
            return (*self)[i];
        else
            throw IndexError("Array index out of range");
        QL_DUMMY_RETURN(0.0)
    }
    void set(Size i, double x) {
        if (i<self->size())
            (*self)[i] = x;
        else
            throw IndexError("Array index out of range");
    }
    #endif

};



// 2-D view

%{
typedef QuantLib::Math::LexicographicalView<Array::iterator>  
    LexicographicalView;
typedef QuantLib::Math::LexicographicalView<Array::iterator>::y_iterator 
    LexicographicalViewColumn;
%}

#if defined(SWIGPYTHON) || defined(SWIGRUBY)
class LexicographicalViewColumn {
  private:
    // access control - no constructor exported
    LexicographicalViewColumn();
};

%extend LexicographicalViewColumn {
    double __getitem__(int i) {
        return (*self)[i];
    }
    void __setitem__(int i, double x) {
        (*self)[i] = x;
    }
};
#endif

#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(">string") __str__;
%rename("set!")    set;
#endif

class LexicographicalView {
  public:
    Size xSize() const;
    Size ySize() const;
};

%extend LexicographicalView {
    LexicographicalView(Array& a, Size xSize) {
        return new LexicographicalView(a.begin(),a.end(),xSize);
    }
    std::string __str__() {
        std::string s;
        for (int j=0; j<static_cast<int>(self->ySize()); j++) {
    	    s += "\n";
            for (int i=0; i<static_cast<int>(self->xSize()); i++) {
                if (i != 0)
                    s += ",";
                s += DoubleFormatter::toString((*self)[i][j]);
            }
        }
        s += "\n";
        return s;
    }

    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    LexicographicalViewColumn __getitem__(Size i) {
        return (*self)[i];
    }
    #endif
    
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    double ref(Size i, Size j) {
        return (*self)[i][j];
    }
    void set(Size i, Size j, double x) {
        (*self)[i][j] = x;
    }
    #endif

};



#endif
