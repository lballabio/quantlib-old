
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <ohxl/Conversions/scalartooper.hpp>
#include <ohxl/Conversions/vectortooper.hpp>
#include <ohxl/Conversions/matrixtooper.hpp>

namespace ObjectHandler {

    DLL_API void scalarToOper(const long &value, OPER &xLong, bool dllToFree) {
        xLong.xltype = xltypeNum;
        xLong.val.num = value;
    }

    DLL_API void scalarToOper(const double &value, OPER &xDouble, bool dllToFree) {
        xDouble.xltype = xltypeNum;
        xDouble.val.num = value;
    }

    DLL_API void scalarToOper(const bool &value, OPER &xBoolean, bool dllToFree) {
        xBoolean.xltype = xltypeBool;
        xBoolean.val.boolean = value;
    }

    DLL_API void scalarToOper(const std::string &value, OPER &xString, bool dllToFree) {
        int len = __min(XL_MAX_STR_LEN, value.length());
        xString.val.str = new char[ len + 1 ];
        xString.xltype = xltypeStr;
        if (dllToFree)
            xString.xltype |= xlbitDLLFree;
        xString.val.str[0] = len;
        if (len)
            strncpy(xString.val.str + 1, value.c_str(), len);
    }

    DLL_API void scalarToOper(const boost::any &value, OPER &xAny, bool dllToFree) {
        if (value.type() == typeid(OPER)) {
            OPER xTemp = boost::any_cast<OPER>(value);
            Excel(xlCoerce, &xAny, 1, &xTemp);
        } else if (value.type() == typeid(OPER*)) {
            OPER *xTemp = boost::any_cast<OPER*>(value);
            Excel(xlCoerce, &xAny, 1, xTemp);
        } else if (value.type() == typeid(unsigned int)) {
            xAny.xltype = xltypeNum;
            xAny.val.num = boost::any_cast<unsigned int>(value); 
        } else if (value.type() == typeid(int)) {
            xAny.xltype = xltypeNum;
            xAny.val.num = boost::any_cast<int>(value);
        } else if (value.type() == typeid(long)) {
            xAny.xltype = xltypeNum;
            xAny.val.num = boost::any_cast<long>(value);
        } else if (value.type() == typeid(double)) {
            xAny.xltype = xltypeNum;
            xAny.val.num = boost::any_cast<double>(value);
        } else if (value.type() == typeid(bool)) {
            xAny.xltype = xltypeBool;
            xAny.val.boolean = boost::any_cast<bool>(value);
        } else if (value.type() == typeid(std::string)) {
            std::string s = boost::any_cast<std::string>(value);
            scalarToOper(s, xAny);
        } else if (value.type() == typeid(std::vector<long>)) {
            std::vector<long> v = boost::any_cast<std::vector<long> >(value);
            vectorToOper(v, xAny);
        } else if (value.type() == typeid(std::vector<double>)) {
            std::vector<double> v = boost::any_cast<std::vector<double> >(value);
            vectorToOper(v, xAny);
        } else if (value.type() == typeid(std::vector<bool>)) {
            std::vector<bool> v = boost::any_cast<std::vector<bool> >(value);
            vectorToOper(v, xAny);
        } else if (value.type() == typeid(std::vector<std::string>)) {
            std::vector<std::string> v = boost::any_cast<std::vector<std::string> >(value);
            vectorToOper(v, xAny);
        } else if (value.type() == typeid(std::vector<boost::any>)) {
            std::vector<boost::any> v = boost::any_cast<std::vector<boost::any> >(value);
            vectorToOper(v, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<long> >)) {
            std::vector<std::vector<long> > vv = boost::any_cast<std::vector<std::vector<long> > >(value);
            vectorToOper(vv, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<double> >)) {
            std::vector<std::vector<double> > vv = boost::any_cast<std::vector<std::vector<double> > >(value);
            matrixToOper(vv, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<bool> >)) {
            std::vector<std::vector<bool> > vv = boost::any_cast<std::vector<std::vector<bool > > >(value);
            matrixToOper<bool>(vv, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<std::string> >)) {
            std::vector<std::vector<std::string> > vv = boost::any_cast<std::vector<std::vector<std::string> > >(value);
            matrixToOper(vv, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<boost::any> >)) {
            std::vector<std::vector<boost::any> > vv = boost::any_cast<std::vector<std::vector<boost::any> > >(value);
            matrixToOper(vv, xAny);
        } else {
            xAny.xltype = xltypeErr;
            xAny.val.err = xlerrValue;
        }
    }

}

