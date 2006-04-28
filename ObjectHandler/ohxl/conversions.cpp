
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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

#include <oh/exception.hpp>
#include <ohxl/conversions.hpp>
#include <sstream>

namespace ObjHandler {

    DLL_API void stringToChar(char *c, const std::string &value) {
        int len = __min(XL_MAX_STR_LEN, value.length());
        strncpy(c, value.c_str(), len);
        c[len] = 0;
    }

    DLL_API void scalarToXloper(XLOPER &xLong, const long &value) {
        xLong.xltype = xltypeNum;
        xLong.val.num = value;
    }

    DLL_API void scalarToXloper(XLOPER &xDouble, const double &value) {
        xDouble.xltype = xltypeNum;
        xDouble.val.num = value;
    }

    DLL_API void scalarToXloper(XLOPER &xBoolean, const bool &value) {
        xBoolean.xltype = xltypeBool;
        xBoolean.val.boolean = value;
    }

    DLL_API void scalarToXloper(XLOPER &xString, const std::string &value) {
        int len = __min(XL_MAX_STR_LEN, value.length());
        xString.val.str = new char[ len + 1 ];
        if (!xString.val.str) 
            throw Exception("stringToXloper: error calling new");
        xString.xltype = xltypeStr | xlbitDLLFree;
        if (len)
            strncpy(xString.val.str + 1, value.c_str(), len);
        xString.val.str[0] = len;
    }

    DLL_API void scalarToXloper(
            XLOPER &xAny, 
            const boost::any &value, 
            const bool &expandVectors) {
        if (value.type() == typeid(XLOPER)) {
            XLOPER xTemp = boost::any_cast<XLOPER>(value);
            Excel(xlCoerce, &xAny, 1, &xTemp);
        } else if (value.type() == typeid(XLOPER*)) {
            XLOPER *xTemp = boost::any_cast<XLOPER*>(value);
            Excel(xlCoerce, &xAny, 1, xTemp);
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
            scalarToXloper(xAny, s);
        } else if (value.type() == typeid(std::vector<long>)) {
            if (expandVectors) {
                std::vector<long> v= boost::any_cast< std::vector<long> >(value);
                vectorToXloper(xAny, v);
            } else {
                scalarToXloper(xAny, std::string(VECTOR));
            }
        } else if (value.type() == typeid(std::vector<double>)) {
            if (expandVectors) {
                std::vector<double> v= boost::any_cast< std::vector<double> >(value);
                vectorToXloper(xAny, v);
            } else {
                scalarToXloper(xAny, std::string(VECTOR));
            }
        } else if (value.type() == typeid(std::vector<bool>)) {
            if (expandVectors) {
                std::vector<bool> v= boost::any_cast< std::vector<bool> >(value);
                vectorToXloper(xAny, v);
            } else {
                scalarToXloper(xAny, std::string(VECTOR));
            }
        } else if (value.type() == typeid(std::vector< std::string >)) {
            if (expandVectors) {
                std::vector< std::string > v= boost::any_cast< std::vector< std::string > >(value);
                vectorToXloper(xAny, v);
            } else {
                scalarToXloper(xAny, std::string(VECTOR));
            }
        } else if (value.type() == typeid(std::vector< boost::any >)) {
            if (expandVectors) {
                std::vector< boost::any > v= boost::any_cast< std::vector< boost::any > >(value);
                vectorToXloper(xAny, v);
            } else {
                scalarToXloper(xAny, std::string(VECTOR));
            }
        } else if (value.type() == typeid(std::vector< std::vector< long > >)) {
            if (expandVectors) {
                std::vector< std::vector<long> >vv = boost::any_cast< std::vector< std::vector<long> > >(value);
                vectorToXloper(xAny, vv);
            } else {
                scalarToXloper(xAny, std::string(MATRIX));
            }
        } else if (value.type() == typeid(std::vector< std::vector< double > >)) {
            if (expandVectors) {
                std::vector< std::vector< double > >vv = boost::any_cast< std::vector< std::vector< double > > >(value);
                matrixToXloper(xAny, vv);
            } else {
                scalarToXloper(xAny, std::string(MATRIX));
            }
        } else if (value.type() == typeid(std::vector< std::vector< bool > >)) {
            if (expandVectors) {
                std::vector< std::vector< bool > >vv = boost::any_cast< std::vector< std::vector< bool > > >(value);
				// FIXME VC8 compilation fails on this line
				// support for matrix of bools not presently required
                //matrixToXloper(xAny, vv);
            } else {
                scalarToXloper(xAny, std::string(MATRIX));
            }
        } else if (value.type() == typeid(std::vector< std::vector< std::string > >)) {
            if (expandVectors) {
                std::vector< std::vector< std::string > >vv = boost::any_cast< std::vector< std::vector< std::string > > >(value);
                matrixToXloper(xAny, vv);
            } else {
                scalarToXloper(xAny, std::string(MATRIX));
            }
        } else if (value.type() == typeid(std::vector< std::vector< boost::any > >)) {
            if (expandVectors) {
                std::vector< std::vector< boost::any > >vv = boost::any_cast< std::vector< std::vector< boost::any > > >(value);
                matrixToXloper(xAny, vv);
            } else {
                scalarToXloper(xAny, std::string(MATRIX));
            }
        } else {
            xAny.xltype = xltypeErr;
            xAny.val.err = xlerrValue;
        }
    }

    DLL_API void operToScalar(long &ret, const OPER &xScalar, const long &defaultValue) {
        try {
            //if (xScalar.xltype & xltypeErr)
            //    throw Exception("input value is #NULL (xltypeErr)");
            //if (xScalar.xltype & (xltypeMissing | xltypeNil))
            if (xScalar.xltype & (xltypeMissing | xltypeNil | xltypeErr))
                ret = defaultValue;
            else if (xScalar.xltype == xltypeNum)
                ret = xScalar.val.num;
            else {
                OPER xLong;
                Excel(xlCoerce, &xLong, 2, xScalar, TempInt(xltypeInt));
                ret = xLong.val.w;
            }
        } catch (const std::exception &e) {
            std::ostringstream msg;
            msg << "operToScalar: " << e.what();
            throw Exception(msg.str().c_str());
        }
    }

    DLL_API void operToScalar(double &ret, const OPER &xScalar, const double &defaultValue) {
        try {
            //if (xScalar.xltype & xltypeErr)
            //    throw Exception("input value is #NULL (xltypeErr)");
            //if (xScalar.xltype & (xltypeMissing | xltypeNil))
            if (xScalar.xltype & (xltypeMissing | xltypeNil | xltypeErr))
                ret = defaultValue;
            else if (xScalar.xltype == xltypeNum)
                ret = xScalar.val.num;
            else {
                OPER xDouble;
                Excel(xlCoerce, &xDouble, 2, xScalar, TempInt(xltypeNum));
                ret = xDouble.val.num;
            }
        } catch (const std::exception &e) {
            std::ostringstream msg;
            msg << "operToScalar: " << e.what();
            throw Exception(msg.str().c_str());
        }
    }

    DLL_API void operToScalar(bool &ret, const OPER &xScalar, const bool &defaultValue) {
        try {
            //if (xScalar.xltype & xltypeErr)
            //    throw Exception("input value is #NULL (xltypeErr)");
            //if (xScalar.xltype & (xltypeMissing | xltypeNil))
            if (xScalar.xltype & (xltypeMissing | xltypeNil | xltypeErr))
                ret = defaultValue;
            else if (xScalar.xltype == xltypeBool)
                ret = xScalar.val.boolean != 0;
            else {
                OPER xBool;
                Excel(xlCoerce, &xBool, 2, xScalar, TempInt(xltypeBool));
                ret = xBool.val.boolean != 0;
            }
        } catch (const std::exception &e) {
            std::ostringstream msg;
            msg << "operToScalar: " << e.what();
            throw Exception(msg.str().c_str());
        }
    }

    DLL_API void operToScalar(std::string &ret, const OPER &xScalar, const std::string &defaultValue) {
        OPER xTemp;
        bool needToFree = false;
        try {
            //if (xScalar.xltype & xltypeErr)
            //    throw Exception("input value is #NULL (xltypeErr)");
            //if (xScalar.xltype & (xltypeMissing | xltypeNil))
            if (xScalar.xltype & (xltypeMissing | xltypeNil | xltypeErr)) {
                ret = defaultValue;
                return;
            }
            const OPER *xString;

            if (xScalar.xltype == xltypeStr)
                xString = &xScalar;
            else {
                Excel(xlCoerce, &xTemp, 2, xScalar, TempInt(xltypeStr));
                xString = &xTemp;
                needToFree = true;
            }

            if (xString->val.str[0])
                ret.assign(xString->val.str + 1, xString->val.str[0]);

            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);

        } catch (const std::exception &e) {
            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);
            std::ostringstream msg;
            msg << "operToScalar: " << e.what();
            throw Exception(msg.str().c_str());
        }
    }

    DLL_API void operToScalar(boost::any &ret, const OPER &xScalar) {
        //if (xScalar.xltype & xltypeErr)
        //    throw Exception("input value is #NULL (xltypeErr)");
        //if (xScalar.xltype & (xltypeMissing | xltypeNil))
        if (xScalar.xltype & (xltypeMissing | xltypeNil | xltypeErr))
            ret = boost::any();
        else if (xScalar.xltype == xltypeNum)
            ret = xScalar.val.num;
        else if (xScalar.xltype == xltypeBool)
            ret = xScalar.val.boolean != 0;
        else if (xScalar.xltype == xltypeStr) {
            std::string value;
            operToScalar(value, xScalar);
            ret = value;
        } else
            throw Exception("operToScalar: unexpected datatype");
    }

}

