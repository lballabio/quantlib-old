
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

#ifndef quantlib_observer_i
#define quantlib_observer_i

%{
using QuantLib::Patterns::Observer;
using QuantLib::Patterns::Observable;
using QuantLib::Handle;
typedef Handle<Observer> ObserverHandle;
typedef Handle<Observable> ObservableHandle;
%}

%rename(Observable) ObservableHandle;
class ObservableHandle {
  private:
    ObservableHandle();
};


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
  public:
	PyObserver(PyObject* callback);
    void registerWith(const ObservableHandle&);
    void unregisterWith(const ObservableHandle&);
};
#endif



#if defined(SWIGMZSCHEME)

%{
// C++ wrapper for MzScheme observer
class MzObserver : public Observer {
  public:
	MzObserver(Scheme_Object* callback)
	: callback_(callback) {
        if (!SCHEME_PROCP(callback))
            throw Error("procedure expected");
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
  public:
	MzObserver(Scheme_Object* callback);
    void registerWith(const ObservableHandle&);
    void unregisterWith(const ObservableHandle&);
};
#endif



#endif
