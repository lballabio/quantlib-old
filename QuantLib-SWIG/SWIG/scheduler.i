
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

#ifndef quantlib_scheduler_i
#define quantlib_scheduler_i

%include date.i
%include calendars.i
%include types.i

%{
using QuantLib::Schedule;
%}

#if defined(SWIGRUBY)
%mixin Schedule "Enumerable";
#endif
class Schedule {
    #if defined(SWIGPYTHON) || defined(SWIGRUBY)
    %rename(__len__)       size;
    %ignore                date;
    #endif
    #if defined(SWIGRUBY)
    %rename("isRegular?")  isRegular;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("is-regular?") isRegular;
    #endif
  public:
    Schedule(const Calendar& calendar, 
             const Date& startDate, const Date& endDate,
             Frequency frequency, BusinessDayConvention rollingConvention, 
             bool isAdjusted, const Date& stubDate = Date(),
             bool startFromEnd = false, bool longFinal = false);
    Schedule(const std::vector<Date>&,
             const Calendar& calendar, 
             BusinessDayConvention rollingConvention,
             bool isAdjusted);
    Size size() const;
    Date date(Size i) const;
    bool isRegular(Size i) const;
    %extend {
        #if defined(SWIGPYTHON) || defined(SWIGRUBY)
        Date __getitem__(Integer i) {
            Integer size_ = static_cast<Integer>(self->size());
            if (i>=0 && i<size_) {
                return self->date(i);
            } else if (i<0 && -i<=size_) {
                return self->date(size_+i);
            } else {
                throw std::out_of_range("schedule index out of range");
            }
            QL_DUMMY_RETURN(Date())
        }
        #endif
        #if defined(SWIGRUBY)
        void each() {
            for (Size i=0; i<self->size(); i++) {
                Date* d = new Date(self->date(i));
                rb_yield(SWIG_NewPointerObj((void *) d, 
                                            $descriptor(Date *), 1));
            }
        }
        #elif defined(SWIGMZSCHEME)
        void for_each(Scheme_Object* proc) {
            for (Size i=0; i<self->size(); i++) {
                Date* d = new Date(self->date(i));
                Scheme_Object* x = 
                    SWIG_NewPointerObj(d, $descriptor(Date *), 1);
                scheme_apply(proc,1,&x);
            }
        }
        #elif defined(SWIGGUILE)
        void for_each(SCM proc) {
            for (Size i=0; i<self->size(); i++) {
                Date* d = new Date(self->date(i));
                SCM x = SWIG_Guile_MakePtr(d, $descriptor(Date *));
                gh_call1(proc,x);
            }
        }
        %scheme%{
            (define (Schedule-map s f)
              (let ((results '()))
                (Schedule-for-each s (lambda (d)
                                      (set! results (cons (f d) results))))
                (reverse results)))
            (export Schedule-map)
        %}
        #endif
    }
};


#endif
