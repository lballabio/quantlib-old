
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

#ifndef quantlib_timebasket_i
#define quantlib_timebasket_i

%include common.i
%include types.i

%{
using QuantLib::CashFlows::TimeBasket;
typedef Handle<TimeBasket> TimeBasketHandle;
typedef TimeBasket::Entry TimeBasketEntry; 
%}

class TimeBasketEntry {
  public:
    Date date() const;
    double value() const;
};
   
// typemap it out to native pair in the scripting language
#if defined(SWIGPYTHON)
%typemap(out) TimeBasketEntry {
    $result = PyTuple_New(2);
    Date* d = new Date($1.date());
    PyTuple_SetItem($result,0,
                    SWIG_NewPointerObj(d, SWIGTYPE_p_Date, 1));
    if ($1.value() == Null<double>()) {
        Py_INCREF(Py_None);
        PyTuple_SetItem($result,1,Py_None);
    } else {
        PyTuple_SetItem($result,1,
                        PyFloat_FromDouble($1.value()));
    }
}
#elif defined(SWIGRUBY)
%typemap(out) TimeBasketEntry {
    $result = rb_ary_new2(2);
    Date* d = new Date($1.date());
    rb_ary_store($result,0,
                 SWIG_NewPointerObj(d, SWIGTYPE_p_Date, 1));
    if ($1.value() == Null<double>())
        rb_ary_store($result,1,Qnil);
    else 
        rb_ary_store($result,1,rb_float_new($1.value()));
}
#elif defined(SWIGMZSCHEME)
%typemap(out) TimeBasketEntry {
    if ($1.value() == Null<double>()) {
        $result = scheme_false;
    } else {
        Date* d = new Date($1.date());
        Scheme_Object* car = SWIG_MakePtr(d, SWIGTYPE_p_Date);
        Scheme_Object* cdr = scheme_make_double($1.value());
        $result = scheme_make_pair(car,cdr);
    }
}
#elif defined(SWIGGUILE)
%typemap(out) TimeBasketEntry {
    if ($1.value() == Null<double>()) {
        $result = SCM_BOOL_F;
    } else {
        Date* d = new Date($1.date());
        SCM car = SWIG_Guile_MakePtr(d, SWIGTYPE_p_Date);
        SCM cdr = gh_double2scm($1.value());
        $result = gh_cons(car,cdr);
    }
}
#endif

%rename(TimeBasket) TimeBasketHandle;
class TimeBasketHandle {
  public:
    %extend {
       TimeBasketHandle(const TimeBasketHandle& original,
			const std::vector<Date>& buckets) {
	  return new TimeBasketHandle(new TimeBasket(original, buckets));
       }
       Size __len__() {
	  return (*self)->size();
       }
#if defined (SWIGPYTHON) || defined(SWIGRUBY)
       TimeBasketEntry __getitem__(int i) {
	  return (**self)[i];
       }
#endif
    }
};

#endif
