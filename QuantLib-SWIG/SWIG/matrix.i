
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

#ifndef quantlib_matrix_i
#define quantlib_matrix_i

%include common.i
%include types.i
%include qlarray.i
%include stl.i

%{
using QuantLib::Math::Matrix;
typedef QuantLib::Math::Matrix::row_iterator MatrixRow;
using QuantLib::Math::outerProduct;
using QuantLib::Math::transpose;
using QuantLib::Math::matrixSqrt;
%}

#if defined(SWIGPYTHON) || defined(SWIGRUBY)
class MatrixRow {
  private:
    MatrixRow();
  public:
    %extend {
        double __getitem__(int i) {
            return (*self)[i];
        }
        void __setitem__(int i, double x) {
            (*self)[i] = x;
        }
    }
};
#endif

/*
#if defined(SWIGPYTHON)
// typemap Python list of lists of numbers to Matrix
%typemap(in) Matrix (Matrix temp),
             Matrix & (Matrix temp),
             const Matrix & (Matrix temp) {
    Matrix* m;
    if (PyTuple_Check($source) || PyList_Check($source)) {
        Size rows, cols;
        rows = (PyTuple_Check($source) ?
                PyTuple_Size($source) :
                PyList_Size($source));
        // look ahead
        PyObject* o = PySequence_GetItem($source,0);
        if (PyTuple_Check(o) || PyList_Check(o)) {
            cols = (PyTuple_Check(o) ?
                    PyTuple_Size(o) :
                    PyList_Size(o));
            Py_DECREF(o);
        } else {
            PyErr_SetString(PyExc_TypeError, "Matrix expected");
            Py_DECREF(o);
            return NULL;
        }

        temp = Matrix(rows,cols);
        for (Size i=0; i<rows; i++) {
            PyObject* o = PySequence_GetItem($source,i);
            if (PyTuple_Check(o) || PyList_Check(o)) {
                Size items = (PyTuple_Check(o) ?
                                        PyTuple_Size(o) :
                                        PyList_Size(o));
                if (items != cols) {
                    PyErr_SetString(PyExc_TypeError, 
                        "Matrix must have equal-length rows");
                    Py_DECREF(o);
                    return NULL;
                }
                for (Size j=0; j<cols; j++) {
                    PyObject* d = PySequence_GetItem(o,j);
                    if (d == Py_None) {
                        temp[i][j] = Null<double>();
                        Py_DECREF(d);
                    } else if (PyFloat_Check(d)) {
                        temp[i][j] = PyFloat_AsDouble(d);
                        Py_DECREF(d);
                    } else if (PyInt_Check(d)) {
                        temp[i][j] = double(PyInt_AsLong(d));
                        Py_DECREF(d);
                    } else {
                        PyErr_SetString(PyExc_TypeError,"doubles expected");
                        Py_DECREF(d);
                        Py_DECREF(o);
                        return NULL;
                    }
                }
                Py_DECREF(o);
            } else {
                PyErr_SetString(PyExc_TypeError, "Matrix expected");
                Py_DECREF(o);
                return NULL;
            }
        }
        $target = &temp;
    } else if ((SWIG_ConvertPtr($source,(void **) &m,
                                SWIGTYPE_p_Matrix,0)) != -1) {
        $target = m;
    } else {
        PyErr_SetString(PyExc_TypeError,"Matrix expected");
        return NULL;
    }
};
*/


#if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename("set!")     setitem;
%rename(">string")  __str__;
%rename("add")      __add__;
%rename("subtract") __sub__;
%rename("multiply") __mul__;
%rename("divide")   __div__;
%rename("Matrix-transpose")    transpose;
%rename("Array-outer-product") outerProduct;
%rename("Matrix-product")      matrixProduct;
%rename("Matrix-sqrt")         matrixSqrt;
// aliases
#if defined(SWIGGUILE)
%scheme %{
    (define Matrix+ Matrix-add)
    (define Matrix- Matrix-subtract)
    (define Matrix* Matrix-multiply)
    (define Matrix/ Matrix-divide)
    (export Matrix+
            Matrix-
            Matrix*
            Matrix/)
%}
#endif
#endif
ReturnByValue(Matrix);

class Matrix {
  public:
    Matrix(Size rows, Size columns, double fill = 0.0);
    Size rows() const;
    Size columns() const;
};

%extend Matrix {
    std::string __str__() {
        std::string s;
        for (Size j=0; j<self->rows(); j++) {
    	    s += "\n";
            s += QuantLib::DoubleFormatter::toString((*self)[j][0]);
            for (Size i=1; i<self->columns(); i++) {
                s += ",";
                s += QuantLib::DoubleFormatter::toString((*self)[j][i]);
            }
        }
        s += "\n";
        return s;
    }
    Matrix __add__(const Matrix& m) {
        return *self+m;
    }
    Matrix __sub__(const Matrix& m) {
        return *self-m;
    }
    Matrix __mul__(double x) {
        return *self*x;
    }
    Matrix __div__(double x) {
        return *self/x;
    }
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    MatrixRow __getitem__(Size i) {
        return (*self)[i];
    }
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    double ref(Size i, Size j) {
        return (*self)[i][j];
    }
    void setitem(Size i, Size j, double x) {
        (*self)[i][j] = x;
    }
    #endif
    #if defined(SWIGPYTHON)
    Matrix __iadd__(const Matrix& m) {
        return *self+m;
    }
    Matrix __isub__(const Matrix& m) {
        return *self-m;
    }
    Matrix __imul__(double x) {
        return *self*x;
    }
    Matrix __rmul__(double x) {
        return *self*x;
    }
    Matrix __idiv__(double x) {
        return *self/x;
    }
    #endif
};


// functions
Matrix transpose(const Matrix& m);
Matrix outerProduct(const Array& v1, const Array& v2);
%inline %{
    Matrix matrixProduct(const Matrix& m1, const Matrix& m2) {
        return m1*m2;
    }
%}
Matrix matrixSqrt(const Matrix& m);



#endif
