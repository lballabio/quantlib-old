
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

#ifndef quantlib_observer_i
#define quantlib_observer_i

%include common.i

%{
using QuantLib::Observer;
using QuantLib::Observable;
%}

%template(Observable) boost::shared_ptr<Observable>;
%define IsObservable(Type)
#if defined(SWIGRUBY)
%rename("toObservable") Type::asObservable;
#elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
%rename(">Observable")  Type::asObservable;
#endif
%extend Type {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    boost::shared_ptr<Observable>* asObservable() {
        return new boost::shared_ptr<Observable>(*self);
    }
    #else
    boost::shared_ptr<Observable> asObservable() {
        return boost::shared_ptr<Observable>(*self);
    }
    #endif
}
%enddef


#if defined(SWIGPYTHON)

%{
// C++ wrapper for Python observer
class PyObserver : public Observer {
  public:
    PyObserver(PyObject* callback)
    : callback_(callback) {
        /* make sure the Python object stays alive
           as long as we need it */
        Py_XINCREF(callback_);
    }
    PyObserver(const PyObserver& o)
    : callback_(o.callback_) {
        /* make sure the Python object stays alive
           as long as we need it */
        Py_XINCREF(callback_);
    }
    PyObserver& operator=(const PyObserver& o) {
        if ((this != &o) && (callback_ != o.callback_)) {
            Py_XDECREF(callback_);
            callback_ = o.callback_;
            Py_XINCREF(callback_);
        }
        return *this;
    }
    ~PyObserver() {
        // now it can go as far as we are concerned
        Py_XDECREF(callback_);
    }
    void update() {
        PyObject* pyResult = PyObject_CallFunction(callback_,NULL);
        QL_ENSURE(pyResult != NULL, "failed to notify Python observer");
        Py_XDECREF(pyResult);
    }
  private:
    PyObject* callback_;
};
%}

// Python wrapper
%rename(Observer) PyObserver;
class PyObserver {
    %rename(_registerWith)   registerWith;
    %rename(_unregisterWith) unregisterWith;
  public:
    PyObserver(PyObject* callback);
    void registerWith(const boost::shared_ptr<Observable>&);
    void unregisterWith(const boost::shared_ptr<Observable>&);
    %pythoncode %{
        def registerWith(self,x):
            self._registerWith(x.asObservable())
        def unregisterWith(self,x):
            self._unregisterWith(x.asObservable())
    %}
};

#elif defined(SWIGRUBY)

%{
// C++ wrapper for Ruby observer
class RubyObserver : public Observer {
  public:
    RubyObserver(VALUE callback)
    : callback_(callback) {}
    void mark() { ((void (*)(VALUE))(rb_gc_mark))(callback_); }
    void update() {
        ID method = rb_intern("call");
        rb_funcall(callback_,method,0);
    }
  private:
    VALUE callback_;
    // inhibit copies
    RubyObserver(const RubyObserver&) {}
    RubyObserver& operator=(const RubyObserver&) { return *this; }
};

void markRubyObserver(void* p) {
    RubyObserver* o = static_cast<RubyObserver*>(p);
    o->mark();
}
%}

// Ruby wrapper
%rename(Observer) RubyObserver;
%markfunc RubyObserver "markRubyObserver";
class RubyObserver {
    %rename(_registerWith)   registerWith;
    %rename(_unregisterWith) unregisterWith;
  public:
    RubyObserver(VALUE callback);
    void registerWith(const boost::shared_ptr<Observable>&);
    void unregisterWith(const boost::shared_ptr<Observable>&);
};

#elif defined(SWIGMZSCHEME)

%{
// C++ wrapper for MzScheme observer
class MzObserver : public Observer {
  public:
    MzObserver(Scheme_Object* callback)
    : callback_(callback) {
        QL_REQUIRE(SCHEME_PROCP(callback), "procedure expected");
        /* make sure the MzScheme object stays alive
           as long as we need it */
        scheme_dont_gc_ptr(callback_);
    }
    MzObserver(const MzObserver& o)
    : callback_(o.callback_) {
        /* make sure the MzScheme object stays alive
           as long as we need it */
        scheme_dont_gc_ptr(callback_);
    }
    MzObserver& operator=(const MzObserver& o) {
        if ((this != &o) && (callback_ != o.callback_)) {
            scheme_gc_ptr_ok(callback_);
            callback_ = o.callback_;
            scheme_dont_gc_ptr(callback_);
        }
        return *this;
    }
    ~MzObserver() {
        // now it can go as far as we are concerned
        scheme_gc_ptr_ok(callback_);
    }
    void update() {
        scheme_apply(callback_,0,0);
    }
  private:
    Scheme_Object* callback_;
};
%}

// MzScheme wrapper
%rename(Observer) MzObserver;
class MzObserver {
    %rename("register-with")   registerWith;
    %rename("unregister-with") unregisterWith;
  public:
    MzObserver(Scheme_Object* callback);
    void registerWith(const boost::shared_ptr<Observable>&);
    void unregisterWith(const boost::shared_ptr<Observable>&);
};

#elif defined(SWIGGUILE)

%{
// C++ wrapper for Guile observer
class GuileObserver : public Observer {
  public:
    GuileObserver(SCM callback)
    : callback_(callback) {
        scm_protect_object(callback_);
    }
    ~GuileObserver() {
        scm_unprotect_object(callback_);
    }
    void update() {
        gh_call0(callback_);
    }
  private:
    SCM callback_;
    // inhibit copies
    GuileObserver(const GuileObserver&) {}
    GuileObserver& operator=(const GuileObserver&) { return *this; }
};
%}

// Guile wrapper
%rename(Observer) GuileObserver;
class GuileObserver {
    %rename("register-with")   registerWith;
    %rename("unregister-with") unregisterWith;
  public:
    GuileObserver(SCM callback);
    void registerWith(const boost::shared_ptr<Observable>&);
    void unregisterWith(const boost::shared_ptr<Observable>&);
};

#endif


#endif
