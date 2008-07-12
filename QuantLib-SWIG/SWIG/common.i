
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

#ifndef quantlib_common_i
#define quantlib_common_i

%include stl.i
%include exception.i

%{
// This is necessary to avoid compile failures on 
// GCC 4
// see http://svn.boost.org/trac/boost/ticket/1793

#if defined(NDEBUG)
#define BOOST_DISABLE_ASSERTS 1
#endif


#include <boost/algorithm/string/case_conv.hpp>
%}

#if defined(SWIGRUBY)
%{
#ifndef SWIG_FLOAT_P
#define SWIG_FLOAT_P(x) ((TYPE(x) == T_FLOAT) || FIXNUM_P(x))
#define SWIG_NUM2DBL(x) (FIXNUM_P(x) ? FIX2INT(x) : NUM2DBL(x))
#endif
%}
#endif

%{
// generally useful classes
using QuantLib::Error;
using QuantLib::Handle;
using QuantLib::RelinkableHandle;
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
class Handle {
    #if defined(SWIGRUBY)
    %rename("null?")   isNull;
    %rename("empty?")  empty;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("null?")    isNull;
    %rename("empty?")  empty;
    #endif
  public:
    Handle(const boost::shared_ptr<T>& = boost::shared_ptr<T>());
    boost::shared_ptr<T> operator->();
    #if defined(SWIGPYTHON)
    %extend {
        bool __nonzero__() {
            return !self->empty();
        }
    }
    #else
    bool empty();
    #endif
};

template <class T>
class RelinkableHandle : public Handle<T> {
    #if defined(SWIGRUBY)
    %rename("linkTo!")  linkTo;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("link-to!") linkTo;
    #endif
  public:
    RelinkableHandle(const boost::shared_ptr<T>& = boost::shared_ptr<T>());
    void linkTo(const boost::shared_ptr<T>&);
};

%define swigr_list_converter(ContainerRType,
                             ContainerCType, ElemCType)
#if defined(SWIGR)
%Rruntime %{
setMethod('print', 'ContainerCType',
function(x) print(as(x, "ElemCType")))

setAs("ContainerCType", "ElemCType",
function(from) {if (from$size()) from[1:from$size()] else NULL} )

setAs("ElemCType", "ContainerCType",
function(from) { a <- ContainerRType(length(from));
sapply(1:length(from), function(n) {
a[n] <- from[n] } )
a
})
%}
#endif
%enddef


#endif
