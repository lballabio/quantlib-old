
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

#ifndef quantlib_history_i
#define quantlib_history_i

%include date.i
%include null.i
%include types.i
%include vectors.i

%{
using QuantLib::History;
typedef History::Entry HistoryEntry;
%}

class HistoryEntry {
  private:
    HistoryEntry();
};

// typemap it out to native pair in the scripting language
#if defined(SWIGPYTHON)
%typemap(out) HistoryEntry {
    $result = PyTuple_New(2);
    Date* d = new Date($1.date());
    PyTuple_SetItem($result,0,
                    SWIG_NewPointerObj(d, $descriptor(Date *), 1));
    if ($1.value() == Null<double>()) {
        Py_INCREF(Py_None);
        PyTuple_SetItem($result,1,Py_None);
    } else {
        PyTuple_SetItem($result,1,
                        PyFloat_FromDouble($1.value()));
    }
}
#elif defined(SWIGRUBY)
%typemap(out) HistoryEntry {
    $result = rb_ary_new2(2);
    Date* d = new Date($1.date());
    rb_ary_store($result,0,
                 SWIG_NewPointerObj(d, $descriptor(Date *), 1));
    if ($1.value() == Null<double>())
        rb_ary_store($result,1,Qnil);
    else 
        rb_ary_store($result,1,rb_float_new($1.value()));
}
#elif defined(SWIGMZSCHEME)
%typemap(out) HistoryEntry {
    if ($1.value() == Null<double>()) {
        $result = scheme_false;
    } else {
        Date* d = new Date($1.date());
        Scheme_Object* car = 
            SWIG_NewPointerObj(d, $descriptor(Date *), 1);
        Scheme_Object* cdr = scheme_make_double($1.value());
        $result = scheme_make_pair(car,cdr);
    }
}
#elif defined(SWIGGUILE)
%typemap(out) HistoryEntry {
    if ($1.value() == Null<double>()) {
        $result = SCM_BOOL_F;
    } else {
        Date* d = new Date($1.date());
        SCM car = SWIG_Guile_MakePtr(d, $descriptor(Date *));
        SCM cdr = gh_double2scm($1.value());
        $result = gh_cons(car,cdr);
    }
}
#endif


#if defined(SWIGPYTHON)
// Python iterators
%{
struct HistoryIterator {
    HistoryIterator(History::const_iterator i,
                    History::const_iterator end)
    : i(i), end(end) {}
    History::const_iterator i;
    History::const_iterator end;
};
 
struct HistoryValidIterator {
    HistoryValidIterator(History::const_valid_iterator i,
                         History::const_valid_iterator end)
    : i(i), end(end) {}
    History::const_valid_iterator i;
    History::const_valid_iterator end;
};
%}

%exception HistoryIterator::next {
    try {
        $action
    } catch (...) {
        %#if PY_VERSION_HEX >= 0x02020000
        PyErr_SetString(PyExc_StopIteration,"");
        %#else
        PyErr_SetString(PyExc_IndexError,"end of history reached");
        %#endif
        return NULL;
    }
}

%exception HistoryValidIterator::next {
    try {
        $action
    } catch (...) {
        %#if PY_VERSION_HEX >= 0x02020000
        PyErr_SetString(PyExc_StopIteration,"");
        %#else
        PyErr_SetString(PyExc_IndexError,"end of history reached");
        %#endif
        return NULL;
    }
}

class HistoryIterator {
  private:
    HistoryIterator();
  public:
    %extend {
        HistoryEntry next() {
            if (self->i == self->end) {
                QL_FAIL("end reached");
            } else {
                History::Entry e = *(self->i);
                (self->i)++;
                return e;
            }
        }
    }
};

class HistoryValidIterator {
  private:
    HistoryValidIterator();
  public:
    %extend {
        HistoryEntry next() {
            if (self->i == self->end) {
                QL_FAIL("end reached");
            } else {
                History::Entry e = *(self->i);
                (self->i)++;
                return e;
            }
        }
        HistoryValidIterator __iter__() {
            return *self;
        }
    }
};
#endif


#if defined(SWIGRUBY)
%mixin History "Enumerable";
#endif
class History {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("first-date") firstDate;
    %rename("last-date")  lastDate;
    %rename("length")     __len__;
    #if defined(SWIGGUILE)
    %scheme %{
        (define History-old-init new-History)
        (define (new-History dates values)
          (let ((null (null-double)))
            (History-old-init dates
                              (map (lambda (x) (or x null)) values))))
    %}
    #endif
    #endif
  public:
    History(const std::vector<Date>& dates, 
            const std::vector<double>& values);
    Date firstDate() const;
    Date lastDate() const;
    %extend {
        Size __len__() {
            return self->size();
        }
        // Getting a single datum
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        doubleOrNull __getitem__(const Date& d) {
            return (*self)[d];
        }
        #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
        HistoryEntry assoc(const Date& d) {
            if (d < self->firstDate() || d > self->lastDate())
                return History::Entry();
            else
                return *(self->begin()+(d-self->firstDate()));
        }
        #endif
        // Iterating
        #if defined(SWIGPYTHON)
        HistoryIterator __iter__() {
            return HistoryIterator(self->begin(),self->end());
        }
        HistoryValidIterator valid() {
            return HistoryValidIterator(self->vbegin(),self->vend());
        }
        #elif defined(SWIGRUBY)
        void each() {
            History::const_iterator i=self->begin(), end=self->end();
            for ( ; i!=end; ++i) {
                Date* d = new Date(i->date());
                double v = i->value();
                VALUE entry = rb_ary_new2(2);
                VALUE date = SWIG_NewPointerObj(d, $descriptor(Date *), 1);
                VALUE value = (v == Null<double>() ? Qnil : rb_float_new(v));
                rb_ary_store(entry,0,date);
                rb_ary_store(entry,1,value);
                rb_yield(entry);
            }
        }
        void each_valid() {
            History::const_valid_iterator i=self->vbegin(), end=self->vend();
            for ( ; i!=end; ++i) {
                Date* d = new Date(i->date());
                double v = i->value();
                VALUE entry = rb_ary_new2(2);
                VALUE date = SWIG_NewPointerObj(d, $descriptor(Date *), 1);
                VALUE value = rb_float_new(v);
                rb_ary_store(entry,0,date);
                rb_ary_store(entry,1,value);
                rb_yield(entry);
            }
        }
        #elif defined(SWIGMZSCHEME)
        void for_each(Scheme_Object* proc) {
            History::const_iterator i=self->begin(), end=self->end();
            for ( ; i!=end; ++i) {
                double v = i->value();
                Scheme_Object* entry;
                if (v == Null<double>()) {
                    entry = scheme_false;
                } else {
                    Date* d = new Date(i->date());
                    Scheme_Object* car = 
                        SWIG_NewPointerObj(d, $descriptor(Date *), 1);
                    Scheme_Object* cdr = scheme_make_double(v);
                    entry = scheme_make_pair(car,cdr);
                }
                scheme_apply(proc,1,&entry);
            }
        }
        void for_each_valid(Scheme_Object* proc) {
            History::const_valid_iterator i=self->vbegin(), end=self->vend();
            for ( ; i!=end; ++i) {
                double v = i->value();
                Date* d = new Date(i->date());
                Scheme_Object* car = 
                    SWIG_NewPointerObj(d, $descriptor(Date *), 1);
                Scheme_Object* cdr = scheme_make_double(v);
                Scheme_Object* cons = scheme_make_pair(car,cdr);
                scheme_apply(proc,1,&cons);
            }
        }
        #elif defined(SWIGGUILE)
        void for_each(SCM proc) {
            History::const_iterator i=self->begin(), end=self->end();
            for ( ; i!=end; ++i) {
                double v = i->value();
                SCM entry;
                if (v == Null<double>()) {
                    entry = SCM_BOOL_F;
                } else {
                    Date* d = new Date(i->date());
                    SCM car = SWIG_Guile_MakePtr(d, $descriptor(Date *));
                    SCM cdr = gh_double2scm(v);
                    entry = gh_cons(car,cdr);
                }
                gh_call1(proc,entry);
            }
        }
        void for_each_valid(SCM proc) {
            History::const_valid_iterator i=self->vbegin(), end=self->vend();
            for ( ; i!=end; ++i) {
                double v = i->value();
                Date* d = new Date(i->date());
                SCM car = SWIG_Guile_MakePtr(d, $descriptor(Date *));
                SCM cdr = gh_double2scm(v);
                SCM cons = gh_cons(car,cdr);
                gh_call1(proc,cons);
            }
        }
        %scheme%{
            (define (History-map h f)
              (let ((results '()))
                (History-for-each h (lambda (e)
                                      (if e
                                        (set! results (cons (f e) results))
                                        (set! results (cons #f results)))))
                (reverse results)))
            (define (History-map-valid h f)
              (let ((results '()))
                (History-for-each-valid h (lambda (e)
                                            (set! results 
                                              (cons (f e) results))))
                (reverse results)))
            (export History-map
                    History-map-valid)
        %}
        #endif
    }
};

#if defined(SWIGPYTHON)
%pythoncode %{
History._old___init__ = History.__init__
def History_new___init__(self,dates,values):
    values = values[:]
    for i in range(len(values)):
        values[i] = values[i] or nullDouble()
    self._old___init__(dates,values)
History.__init__ = History_new___init__
%}
#endif


#endif
