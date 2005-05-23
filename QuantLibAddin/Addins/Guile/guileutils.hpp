
/*
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_guileutils_hpp
#define qla_guileutils_hpp

#include <guile/gh.h>

#include <boost/any.hpp>
#include <ql/errors.hpp>
#include <oh/objhandler.hpp>

SCM anyToPairValue(const ObjHandler::ObjectProperty& property);
SCM anyToDottedPair(const ObjHandler::ObjectProperty& property);
SCM propertiesToAList(const ObjHandler::Properties& properties);

template <typename T>
class Convert {
  public:
    static T scalar(SCM x) {
        boost::any rtn;
        if (typeid(T) == typeid(bool)) {
            int tmp = gh_scm2bool(x);
            rtn = boost::any(tmp == 0 ? false : true);
        } else if (typeid(T) == typeid(double)) {
            rtn = boost::any(gh_scm2double(x));
        } else if (typeid(T) == typeid(long)) {
            rtn = boost::any(gh_scm2long(x));
        } else if (typeid(T) == typeid(int)) {
            rtn = boost::any(gh_scm2int(x));        
        } else if (typeid(T) == typeid(std::string)) {
            char *str = gh_scm2newstr(x, NULL);
            rtn = boost::any(std::string(str));
        } else {
            QL_FAIL("unsupported argument type");
        }
        return boost::any_cast<T>(rtn);
    }
    
    static std::vector<T> vector(SCM x) {
        std::vector<T> rtn;
        while (gh_list_p(x) && !gh_null_p(x)) {
            rtn.push_back(Convert<T>::scalar(gh_car(x)));
            x = gh_cdr(x);
        }
        return rtn;
    }
    
    static std::vector< std::vector<T> > matrix(SCM x) {
        std::vector< std::vector<T> > rtn;
        while (gh_list_p(x) && !gh_null_p(x)) {
            rtn.push_back(Convert<T>::vector(gh_car(x)));
            x = gh_cdr(x);
        }
        return rtn;
    }
};

template <typename T>
class Push {
  public:
    static SCM scalar(ObjHandler::ArgumentStack& stack, SCM argList) {
        if (gh_list_p(argList) && !gh_null_p(argList)) {
            stack.push(Convert<T>::scalar(gh_car(argList)));
            return gh_cdr(argList);
        } else {
            QL_FAIL("Error: not enough arguments");
        }
    }
    
    static SCM vector(ObjHandler::ArgumentStack& stack, SCM argList) {
        if (gh_list_p(argList) && !gh_null_p(argList)) {
            stack.push(Convert<T>::vector(gh_car(argList)));
            return gh_cdr(argList);
        } else {
            QL_FAIL("Error: not enough arguments");
        }
    }
    
    static SCM matrix(ObjHandler::ArgumentStack& stack, SCM argList) {
        if (gh_list_p(argList) && !gh_null_p(argList)) {
            stack.push(Convert<T>::matrix(gh_car(argList)));
            return gh_cdr(argList);
        } else {
            QL_FAIL("Error: not enough arguments");            
        }
    }
};

#define GET_ARGUMENT(x, args, tensor, type) Push<type>::tensor(args, x)
    
#endif

