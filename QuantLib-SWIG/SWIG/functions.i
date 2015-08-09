
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2015 Klaus Spanderen
 
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

#ifndef quantlib_functions_i
#define quantlib_functions_i

%include linearalgebra.i
%include types.i

%{
using QuantLib::CostFunction;
using QuantLib::Disposable;
%}

#if defined(SWIGPYTHON)

%{
class UnaryFunction {
  public:
    UnaryFunction(PyObject* function) : function_(function) {
        Py_XINCREF(function_);
    }
    UnaryFunction(const UnaryFunction& f) : function_(f.function_) {
        Py_XINCREF(function_);
    }
    UnaryFunction& operator=(const UnaryFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            Py_XDECREF(function_);
            function_ = f.function_;
            Py_XINCREF(function_);
        }
        return *this;
    }
    ~UnaryFunction() {
        Py_XDECREF(function_);
    }
    Real operator()(Real x) const {
        PyObject* pyResult = PyObject_CallFunction(function_,"d",x);
        QL_ENSURE(pyResult != NULL, "failed to call Python function");
        Real result = PyFloat_AsDouble(pyResult);
        Py_XDECREF(pyResult);
        return result;
    }
    Real derivative(Real x) const {
        PyObject* pyResult =
            PyObject_CallMethod(function_,"derivative","d",x);
        QL_ENSURE(pyResult != NULL,
                  "failed to call derivative() on Python object");
        Real result = PyFloat_AsDouble(pyResult);
        Py_XDECREF(pyResult);
        return result;
    }
  private:
    PyObject* function_;
};

class BinaryFunction {
  public:
    BinaryFunction(PyObject* function) : function_(function) {
        Py_XINCREF(function_);
    }
    BinaryFunction(const BinaryFunction& f)
    : function_(f.function_) {
        Py_XINCREF(function_);
    }
    BinaryFunction& operator=(const BinaryFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            Py_XDECREF(function_);
            function_ = f.function_;
            Py_XINCREF(function_);
        }
        return *this;
    }
    ~BinaryFunction() {
        Py_XDECREF(function_);
    }
    Real operator()(Real x, Real y) const {
        PyObject* pyResult = PyObject_CallFunction(function_,"dd",x,y);
        QL_ENSURE(pyResult != NULL, "failed to call Python function");
        Real result = PyFloat_AsDouble(pyResult);
        Py_XDECREF(pyResult);
        return result;
    }
  private:
    PyObject* function_;
};

class PyCostFunction : public CostFunction {
  public:
    PyCostFunction(PyObject* function) : function_(function) {
        Py_XINCREF(function_);
    }
    PyCostFunction(const PyCostFunction& f)
    : function_(f.function_) {
        Py_XINCREF(function_);
    }
    PyCostFunction& operator=(const PyCostFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            Py_XDECREF(function_);
            function_ = f.function_;
            Py_XINCREF(function_);
        }
        return *this;
    }
    ~PyCostFunction() {
        Py_XDECREF(function_);
    }
    Real value(const Array& x) const {
        PyObject* tuple = PyTuple_New(x.size());
        for (Size i=0; i<x.size(); i++)
            PyTuple_SetItem(tuple,i,PyFloat_FromDouble(x[i]));
        PyObject* pyResult = PyObject_CallObject(function_,tuple);
        Py_XDECREF(tuple);
        QL_ENSURE(pyResult != NULL, "failed to call Python function");
        Real result = PyFloat_AsDouble(pyResult);
        Py_XDECREF(pyResult);
        return result;
    }
    Disposable<Array> values(const Array& x) const {
        QL_FAIL("Not implemented");
        // Should be straight forward to copy from a python list
        // to an array
    }
  private:
    PyObject* function_;
};
%}

#elif defined(SWIGRUBY)

%{
class UnaryFunction {
  public:
    Real operator()(Real x) const {
        return NUM2DBL(rb_yield(rb_float_new(x)));
    }
};

class RubyCostFunction : public CostFunction {
  public:
    Real value(const Array& x) const {
        VALUE a = rb_ary_new2(x.size());
        for (Size i=0; i<x.size(); i++)
            rb_ary_store(a,i,rb_float_new(x[i]));
        return NUM2DBL(rb_yield(a));
    }
    Disposable<Array> values(const Array& x) const {
        QL_FAIL("Not implemented");
    }
};
%}

#elif defined(SWIGMZSCHEME)

%{
class UnaryFunction {
  public:
    UnaryFunction(Scheme_Object* function) : function_(function) {
        QL_REQUIRE(SCHEME_PROCP(function), "procedure expected");
        scheme_dont_gc_ptr(function_);
    }
    UnaryFunction(const UnaryFunction& f) : function_(f.function_) {
        scheme_dont_gc_ptr(function_);
    }
    UnaryFunction& operator=(const UnaryFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            scheme_gc_ptr_ok(function_);
            function_ = f.function_;
            scheme_dont_gc_ptr(function_);
        }
        return *this;
    }
    ~UnaryFunction() {
        scheme_gc_ptr_ok(function_);
    }
    Real operator()(Real x) const {
        Scheme_Object* arg = scheme_make_double(x);
        Scheme_Object* mzResult = scheme_apply(function_,1,&arg);
        QL_ENSURE(SCHEME_REALP(mzResult),
                  "the function did not return a double");
        Real result = scheme_real_to_double(mzResult);
        return result;
    }
  private:
    Scheme_Object* function_;
};

class BinaryFunction {
  public:
    BinaryFunction(Scheme_Object* function) : function_(function) {
        QL_REQUIRE(SCHEME_PROCP(function), "procedure expected");
        scheme_dont_gc_ptr(function_);
    }
    BinaryFunction(const BinaryFunction& f) : function_(f.function_) {
        scheme_dont_gc_ptr(function_);
    }
    BinaryFunction& operator=(const BinaryFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            scheme_gc_ptr_ok(function_);
            function_ = f.function_;
            scheme_dont_gc_ptr(function_);
        }
        return *this;
    }
    ~BinaryFunction() {
        scheme_gc_ptr_ok(function_);
    }
    Real operator()(Real x, Real y) const {
        Scheme_Object* arg1 = scheme_make_double(x);
        Scheme_Object* arg2 = scheme_make_double(y);
        Scheme_Object* arg  = scheme_make_pair(arg1,
                                               scheme_make_pair(arg2,
                                                                scheme_null));
        Scheme_Object* mzResult = scheme_apply_to_list(function_,arg);
        QL_ENSURE(SCHEME_REALP(mzResult),
                  "the function did not return a double");
        Real result = scheme_real_to_double(mzResult);
        return result;
    }
  private:
    Scheme_Object* function_;
};

class MzCostFunction : public CostFunction {
  public:
    MzCostFunction(Scheme_Object* function) : function_(function) {
        QL_REQUIRE(SCHEME_PROCP(function), "procedure expected");
        scheme_dont_gc_ptr(function_);
    }
    MzCostFunction(const MzCostFunction& f)
    : function_(f.function_) {
        scheme_dont_gc_ptr(function_);
    }
    MzCostFunction& operator=(const MzCostFunction& f) {
        if ((this != &f) && (function_ != f.function_)) {
            scheme_gc_ptr_ok(function_);
            function_ = f.function_;
            scheme_dont_gc_ptr(function_);
        }
        return *this;
    }
    ~MzCostFunction() {
        scheme_gc_ptr_ok(function_);
    }
    Real value(const Array& x) const {
        Scheme_Object** args = new Scheme_Object*[x.size()];
        for (Size i=0; i<x.size(); i++)
            args[i] = scheme_make_double(x[i]);
        Scheme_Object* mzResult = scheme_apply(function_,x.size(),args);
        delete[] args;
        QL_ENSURE(SCHEME_REALP(mzResult),
                  "the function did not return a double");
        Real result = scheme_real_to_double(mzResult);
        return result;
    }
    Disposable<Array> values(const Array& x) const {
        QL_FAIL("Not implemented");
    }
  private:
    Scheme_Object* function_;
};
%}

#elif defined(SWIGGUILE)

%{
class SCM_Holder {
  public:
    SCM_Holder(SCM data) : data_(data) {
        scm_protect_object(data_);
    }
    ~SCM_Holder() {
        scm_unprotect_object(data_);
    }
    SCM data() const { return data_; }
  private:
    SCM data_;
    // inhibit copy
    SCM_Holder(const SCM_Holder&) {}
    SCM_Holder& operator=(const SCM_Holder&) {
        return *this;
    }
};

class UnaryFunction {
  private:
    boost::shared_ptr<SCM_Holder> function_;
  public:
    UnaryFunction(SCM function) : function_(new SCM_Holder(function)) {
        QL_REQUIRE(gh_procedure_p(function), "procedure expected");
    }
    Real operator()(Real x) const {
        SCM arg = gh_double2scm(x);
        SCM guileResult = gh_call1(function_->data(),arg);
        Real result = gh_scm2double(guileResult);
        return result;
    }
};

class GuileCostFunction : public CostFunction {
  private:
    boost::shared_ptr<SCM_Holder> function_;
  public:
    GuileCostFunction(SCM function) : function_(new SCM_Holder(function)) {
        QL_REQUIRE(gh_procedure_p(function), "procedure expected");
    }
    Real value(const Array& x) const {
        SCM v = gh_make_vector(gh_long2scm(x.size()),SCM_UNSPECIFIED);
        for (Size i=0; i<x.size(); i++)
            gh_vector_set_x(v,gh_long2scm(i),gh_double2scm(x[i]));
        SCM guileResult = gh_apply(function_->data(),gh_vector_to_list(v));
        Real result = gh_scm2double(guileResult);
        return result;
    }
    Disposable<Array> values(const Array& x) const {
        QL_FAIL("Not implemented");
    }
};
%}

#elif defined(SWIGJAVA)

%{
class UnaryFunctionDelegate {
  public:
    virtual ~UnaryFunctionDelegate() {}
    virtual Real value(Real x) const {
        QL_FAIL("implementation of UnaryFunctionDelegate.value is missing");
    };
};

class UnaryFunction : public std::unary_function<Real, Real> {
  public:
    UnaryFunction(UnaryFunctionDelegate* delegate)
    : delegate_(delegate) { }

    virtual ~UnaryFunction() { }

    Real operator()(Real x) const {
        return delegate_->value(x);
    }

  private:
    UnaryFunctionDelegate* delegate_;
};
%}

class UnaryFunction {
  public:
    UnaryFunction(UnaryFunctionDelegate*);
    Real operator()(Real x) const;
};

%feature("director") UnaryFunctionDelegate;

class UnaryFunctionDelegate {
  public:
    virtual ~UnaryFunctionDelegate();
    virtual Real value(Real x) const;
};

%{
class CostFunctionDelegate {
  public:
    virtual ~CostFunctionDelegate() {}
    virtual Real value(const Array& x) const {
      QL_FAIL("implementation of CostFunctionDelegate.value is missing");
    }

    virtual Array values(const Array& x) const {
      QL_FAIL("implementation of CostFunctionDelegate.values is missing");
    }
};

class JavaCostFunction : public CostFunction {
  public:
    JavaCostFunction(CostFunctionDelegate* delegate)
    : delegate_(delegate) { }

    virtual ~JavaCostFunction(){ }

    virtual Real value(const Array& x ) const{
      return delegate_->value(x);
    }

    virtual Disposable<Array> values(const Array& x) const {
      Array retVal = delegate_->values(x);
      return retVal;
    }

  private:
    CostFunctionDelegate* delegate_;
};
%}

class JavaCostFunction {
  public:
    JavaCostFunction(CostFunctionDelegate* delegate);

    virtual ~JavaCostFunction();	
    virtual Real value(const Array& x ) const;	
    virtual Disposable<Array> values(const Array& x) const;

  private:
    CostFunctionDelegate* delegate_;
};

%feature("director") CostFunctionDelegate;

class CostFunctionDelegate {
  public:
    virtual ~CostFunctionDelegate();

    virtual Real value(const Array& x) const;
    virtual Array values(const Array& x) const;
};

#endif
#endif
