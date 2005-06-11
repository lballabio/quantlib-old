
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005 Eric Ehlers

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

#include <qla/qladdin.hpp>
#include <Addins/Excel/xlutils.hpp>
#include <exception>
#include <sstream>

using namespace ObjHandler;
using namespace QuantLibAddin;

#define VECTOR "<VECTOR>"
#define MATRIX "<MATRIX>"

std::string xloperToString(const XLOPER &xOp) {
    XLOPER xStr;
    if (xlretSuccess != Excel(xlCoerce, &xStr, 2, &xOp, TempInt(xltypeStr))) 
        throw std::exception("xloperToString: error on call to xlCoerce");
    std::string s;
    if (xStr.val.str[0])
        s.assign(xStr.val.str + 1, xStr.val.str[0]);
    Excel(xlFree, 0, 1, &xStr);
    return s;
}

void stringToXloper(XLOPER &xStr, const std::string &s) {
    xStr.xltype = xltypeStr;
    xStr.xltype |= xlbitDLLFree;
    int len = __min(XL_MAX_STR_LEN, s.length());
    xStr.val.str = new char[ len + 1 ];
    if (!xStr.val.str) 
        throw std::exception("error calling new in function stringToXloper");
    if (len)
        strncpy(xStr.val.str + 1, s.c_str(), len);
    xStr.val.str[0] = len;
}

DLLEXPORT LPXLOPER getAddress(LPXLOPER xCaller) {
    XLOPER xRef;
    static XLOPER xStr;
    if (xlretSuccess != Excel(xlfVolatile, 0, 1, TempBool(0)))
        throw std::exception("error on call to xlfVolatile");
    if (xlretSuccess != Excel(xlfGetCell, &xRef, 2, TempInt(1), xCaller))
        throw std::exception("error on call to xlfGetCell");
    if (xlretSuccess != Excel(xlCoerce, &xStr, 2, &xRef, TempInt(xltypeStr))) 
        throw std::exception("error on call to xlCoerce");
    Excel(xlFree, 0, 1, &xRef);
    xStr.xltype |= xlbitXLFree;
    return &xStr;
}

std::string getHandleFull(const std::string &handle) {
    XLOPER xCaller, xStr;
    try {
        if (xlretSuccess != Excel(xlfCaller, &xCaller, 0))
            throw std::exception("error on call to xlfCaller");
        if (xlretSuccess != Excel(xlUDF, &xStr, 2, TempStrNoSize("\x0A""getAddress"), &xCaller))
            throw std::exception("error on call to getAddress");
    } catch(const std::exception &e) {
        Excel(xlFree, 0, 2, &xCaller, &xStr);
        std::ostringstream s1;
        s1 << "getHandleFull: " << e.what();
        throw std::exception(s1.str().c_str());
    }
    std::string ret(handle + '#' + xloperToString(xStr));
    Excel(xlFree, 0, 2, &xCaller, &xStr);
    return ret;
}

void stringToChar(char *c, const std::string &s) {
    int len = __min(XL_MAX_STR_LEN, s.length());
    strncpy(c, s.c_str(), len);
}

void scalarAnyToXloper(
        XLOPER &xScalar, 
        const boost::any &any, 
        const bool &expandVectors) {
    if (any.type() == typeid(XLOPER)) {
        XLOPER xTemp = boost::any_cast<XLOPER>(any);
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 1, &xTemp)) 
            xScalar.xltype = xltypeErr;
    } else if (any.type() == typeid(LPXLOPER)) {
        LPXLOPER xTemp = boost::any_cast<LPXLOPER>(any);
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 1, xTemp)) 
            xScalar.xltype = xltypeErr;
    } else if (any.type() == typeid(int)) {
//        xScalar.xltype = xltypeInt;
//        xScalar.val.w = boost::any_cast<int>(any);
        xScalar.xltype = xltypeNum;
        xScalar.val.num = boost::any_cast<int>(any);
    } else if (any.type() == typeid(long)) {
//        xScalar.xltype = xltypeInt;
//        xScalar.val.w = boost::any_cast<long>(any);
        xScalar.xltype = xltypeNum;
        xScalar.val.num = boost::any_cast<long>(any);
    } else if (any.type() == typeid(double)) {
        xScalar.xltype = xltypeNum;
        xScalar.val.num = boost::any_cast<double>(any);
    } else if (any.type() == typeid(bool)) {
        xScalar.xltype = xltypeBool;
        xScalar.val.boolean = boost::any_cast<bool>(any);
    } else if (any.type() == typeid(std::string)) {
        std::string s = boost::any_cast<std::string>(any);
        stringToXloper(xScalar, s);
    } else if (any.type() == typeid(std::vector<long>)) {
        if (expandVectors) {
            std::vector<long> v= boost::any_cast< std::vector<long> >(any);
            vectorLongToXloper(xScalar, v);
        } else {
            stringToXloper(xScalar, VECTOR);
        }
    } else if (any.type() == typeid(std::vector<double>)) {
        if (expandVectors) {
            std::vector<double> v= boost::any_cast< std::vector<double> >(any);
            vectorDoubleToXloper(xScalar, v);
        } else {
            stringToXloper(xScalar, VECTOR);
        }
    } else if (any.type() == typeid(std::vector<bool>)) {
        if (expandVectors) {
            std::vector<bool> v= boost::any_cast< std::vector<bool> >(any);
            vectorBoolToXloper(xScalar, v);
        } else {
            stringToXloper(xScalar, VECTOR);
        }
    } else if (any.type() == typeid(std::vector< std::string >)) {
        if (expandVectors) {
            std::vector< std::string > v= boost::any_cast< std::vector< std::string > >(any);
            vectorStringToXloper(xScalar, v);
        } else {
            stringToXloper(xScalar, VECTOR);
        }
    } else if (any.type() == typeid(std::vector< boost::any >)) {
        if (expandVectors) {
            std::vector< boost::any > v= boost::any_cast< std::vector< boost::any > >(any);
            vectorAnyToXloper(xScalar, v);
        } else {
            stringToXloper(xScalar, VECTOR);
        }
    } else if (any.type() == typeid(std::vector< std::vector< long > >)) {
        if (expandVectors) {
            std::vector< std::vector<long> >vv = boost::any_cast< std::vector< std::vector<long> > >(any);
            matrixLongToXloper(xScalar, vv);
        } else {
            stringToXloper(xScalar, MATRIX);
        }
    } else if (any.type() == typeid(std::vector< std::vector< double > >)) {
        if (expandVectors) {
            std::vector< std::vector< double > >vv = boost::any_cast< std::vector< std::vector< double > > >(any);
            matrixDoubleToXloper(xScalar, vv);
        } else {
            stringToXloper(xScalar, MATRIX);
        }
    } else if (any.type() == typeid(std::vector< std::vector< bool > >)) {
        if (expandVectors) {
            std::vector< std::vector< bool > >vv = boost::any_cast< std::vector< std::vector< bool > > >(any);
            matrixBoolToXloper(xScalar, vv);
        } else {
            stringToXloper(xScalar, MATRIX);
        }
    } else if (any.type() == typeid(std::vector< std::vector< std::string > >)) {
        if (expandVectors) {
            std::vector< std::vector< std::string > >vv = boost::any_cast< std::vector< std::vector< std::string > > >(any);
            matrixStringToXloper(xScalar, vv);
        } else {
            stringToXloper(xScalar, MATRIX);
        }
    } else if (any.type() == typeid(std::vector< std::vector< boost::any > >)) {
        if (expandVectors) {
            std::vector< std::vector< boost::any > >vv = boost::any_cast< std::vector< std::vector< boost::any > > >(any);
            matrixAnyToXloper(xScalar, vv);
        } else {
            stringToXloper(xScalar, MATRIX);
        }
    } else
        xScalar.xltype = xltypeErr;
}

void propertyVectorToXloper(LPXLOPER xArray, Properties properties, const std::string &handle) {
    xArray->xltype = xltypeMulti;
    xArray->xltype |= xlbitDLLFree;
    xArray->val.array.rows = 1;
    xArray->val.array.columns = properties.size() + 1;
    xArray->val.array.lparray = new XLOPER[properties.size() + 1]; 
    if (!xArray->val.array.lparray)
        throw("propertyVectorToXloper: error on call to new");
    stringToXloper(xArray->val.array.lparray[0], handle);
    for (unsigned int i=0; i<properties.size(); i++) {
        ObjectProperty property = properties[i];
        any_ptr a = property();
        scalarAnyToXloper(xArray->val.array.lparray[i + 1], *a);
    }
}

void vectorLongToXloper(XLOPER &xVector, std::vector < long > &v) {
    xVector.xltype = xltypeMulti;
    xVector.xltype |= xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw("vectorLongToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
//        xVector.val.array.lparray[i].xltype = xltypeInt;
//        xVector.val.array.lparray[i].val.w = v[i];
        xVector.val.array.lparray[i].xltype = xltypeNum;
        xVector.val.array.lparray[i].val.num = v[i];
    }
}

void vectorDoubleToXloper(XLOPER &xVector, std::vector < double > &v) {
    xVector.xltype = xltypeMulti;
    xVector.xltype |= xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw("vectorDoubleToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
        xVector.val.array.lparray[i].xltype = xltypeNum;
        xVector.val.array.lparray[i].val.num = v[i];
    }
}

void vectorBoolToXloper(XLOPER &xVector, std::vector < bool > &v) {
    xVector.xltype = xltypeMulti;
    xVector.xltype |= xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw("vectorBoolToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
        xVector.val.array.lparray[i].xltype = xltypeBool;
        xVector.val.array.lparray[i].val.boolean = v[i];
    }
}

void vectorStringToXloper(XLOPER &xVector, std::vector < std::string > &v) {
    xVector.xltype = xltypeMulti;
    xVector.xltype |= xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw("vectorBoolToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++)
        stringToXloper(xVector.val.array.lparray[i], v[i]);
}

void vectorAnyToXloper(XLOPER &xVector, std::vector < boost::any > &v) {
    xVector.xltype = xltypeMulti;
    xVector.xltype |= xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw("vectorAnyToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++)
        scalarAnyToXloper(xVector.val.array.lparray[i], v[i]);
}

void matrixLongToXloper(XLOPER &xMatrix, std::vector < std::vector < long > > &vv) {
    xMatrix.xltype = xltypeMulti;
    xMatrix.xltype |= xlbitDLLFree;
    if (vv.size() && vv[0].size()) {
        xMatrix.val.array.rows = vv.size();
        xMatrix.val.array.columns = vv[0].size();
    } else {
        xMatrix.val.array.rows    = 0;
        xMatrix.val.array.columns = 0;
        return;
    }
    xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
    if (!xMatrix.val.array.lparray)
        throw("matrixLongToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < long > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++) {
            int idx = i * v.size() + j;
//            xMatrix.val.array.lparray[idx].xltype = xltypeInt;
//            xMatrix.val.array.lparray[idx].val.w = v[j];
            xMatrix.val.array.lparray[idx].xltype = xltypeNum;
            xMatrix.val.array.lparray[idx].val.num = v[j];
        }
    }
}

void matrixDoubleToXloper(XLOPER &xMatrix, std::vector < std::vector < double > > &vv) {
    xMatrix.xltype = xltypeMulti;
    xMatrix.xltype |= xlbitDLLFree;
    if (vv.size() && vv[0].size()) {
        xMatrix.val.array.rows = vv.size();
        xMatrix.val.array.columns = vv[0].size();
    } else {
        xMatrix.val.array.rows    = 0;
        xMatrix.val.array.columns = 0;
        return;
    }
    xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
    if (!xMatrix.val.array.lparray)
        throw("matrixDoubleToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < double > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++) {
            int idx = i * v.size() + j;
            xMatrix.val.array.lparray[idx].xltype = xltypeNum;
            xMatrix.val.array.lparray[idx].val.num = v[j];
        }
    }
}

void matrixBoolToXloper(XLOPER &xMatrix, std::vector < std::vector < bool > > &vv) {
    xMatrix.xltype = xltypeMulti;
    xMatrix.xltype |= xlbitDLLFree;
    if (vv.size() && (vv[0]).size()) {
        xMatrix.val.array.rows = vv.size();
        xMatrix.val.array.columns = vv[0].size();
    } else {
        xMatrix.val.array.rows    = 0;
        xMatrix.val.array.columns = 0;
        return;
    }
    xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
    if (!xMatrix.val.array.lparray)
        throw("matrixBoolToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < bool > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++) {
            int idx = i * v.size() + j;
            xMatrix.val.array.lparray[idx].xltype = xltypeBool;
            xMatrix.val.array.lparray[idx].val.boolean = v[j];
        }
    }
}

void matrixStringToXloper(XLOPER &xMatrix, std::vector < std::vector < std::string > > &vv) {
    xMatrix.xltype = xltypeMulti;
    xMatrix.xltype |= xlbitDLLFree;
    if (vv.size() && vv[0].size()) {
        xMatrix.val.array.rows = vv.size();
        xMatrix.val.array.columns = vv[0].size();
    } else {
        xMatrix.val.array.rows    = 0;
        xMatrix.val.array.columns = 0;
        return;
    }
    xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
    if (!xMatrix.val.array.lparray)
        throw("matrixBoolToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < std::string > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++)
            stringToXloper(xMatrix.val.array.lparray[i * v.size() + j], v[j]);
    }
}

void matrixAnyToXloper(XLOPER &xMatrix, std::vector < std::vector < boost::any > > &vv) {
    xMatrix.xltype = xltypeMulti;
    xMatrix.xltype |= xlbitDLLFree;
    if (vv.size() && vv[0].size()) {
        xMatrix.val.array.rows = vv.size();
        xMatrix.val.array.columns = vv[0].size();
    } else {
        xMatrix.val.array.rows    = 0;
        xMatrix.val.array.columns = 0;
        return;
    }
    xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
    if (!xMatrix.val.array.lparray)
        throw("matrixBoolToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < boost::any > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++)
            scalarAnyToXloper(xMatrix.val.array.lparray[i * v.size() + j], v[j]);
    }
}

boost::any xloperToScalarAny(const LPXLOPER xScalar) {
    if (xScalar->xltype == xltypeInt)
        return xScalar->val.w;
    else if (xScalar->xltype == xltypeNum)
        return xScalar->val.num;
    else if (xScalar->xltype == xltypeBool)
        return xScalar->val.boolean;
    else if (xScalar->xltype == xltypeStr)
        return xloperToString(*xScalar);
    else {
        XLOPER xStr;
        if (xlretSuccess != Excel(xlCoerce, &xStr, 2, xScalar, TempInt(xltypeStr)))
            return boost::any();
        std::string ret = xloperToString(xStr);
        Excel(xlFree, 0, 1, &xStr);
        return ret;
    }
}

std::vector < long >xloperToVectorLong(const LPXLOPER xMulti) {
    XLOPER xVector;
    if (xlretSuccess != Excel(xlCoerce, &xVector, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < long > v;
    XLOPER xScalar;
    for (int i=0; i<xVector.val.array.rows * xVector.val.array.columns; i++) {
//        if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xVector.val.array.lparray[i], TempInt(xltypeInt)))
//            throw std::exception("convertArray: error on call to xlCoerce");
//        v.push_back(xScalar.val.w);
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xVector.val.array.lparray[i], TempInt(xltypeNum)))
            throw std::exception("convertArray: error on call to xlCoerce");
        v.push_back(xScalar.val.num);
    }
    Excel(xlFree, 0, 1, &xVector);
    return v;
}

std::vector < double >xloperToVectorDouble(const LPXLOPER xMulti) {
    XLOPER xVector;
    if (xlretSuccess != Excel(xlCoerce, &xVector, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < double > v;
    XLOPER xScalar;
    for (int i=0; i<xVector.val.array.rows * xVector.val.array.columns; i++) {
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xVector.val.array.lparray[i], TempInt(xltypeNum)))
            throw std::exception("convertArray: error on call to xlCoerce");
        v.push_back(xScalar.val.num);
    }
    Excel(xlFree, 0, 1, &xVector);
    return v;
}

std::vector < bool >xloperToVectorBool(const LPXLOPER xMulti) {
    XLOPER xVector;
    if (xlretSuccess != Excel(xlCoerce, &xVector, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < bool > v;
    XLOPER xScalar;
    for (int i=0; i<xVector.val.array.rows * xVector.val.array.columns; i++) {
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xVector.val.array.lparray[i], TempInt(xltypeBool)))
            throw std::exception("convertArray: error on call to xlCoerce");
        v.push_back(xScalar.val.boolean != 0);
    }
    Excel(xlFree, 0, 1, &xVector);
    return v;
}

std::vector < std::string >xloperToVectorString(const LPXLOPER xMulti) {
    XLOPER xVector;
    if (xlretSuccess != Excel(xlCoerce, &xVector, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::string > v;
    for (int i=0; i<xVector.val.array.rows * xVector.val.array.columns; i++)
        v.push_back(xloperToString(xVector.val.array.lparray[i]));
    Excel(xlFree, 0, 1, &xVector);
    return v;
}

std::vector < boost::any > xloperToVectorAny(const LPXLOPER xMulti) {
    XLOPER xVector;
    if (xlretSuccess != Excel(xlCoerce, &xVector, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < boost::any > v;
    for (int i=0; i<xVector.val.array.rows * xVector.val.array.columns; i++)
        v.push_back(xloperToScalarAny(&xVector.val.array.lparray[i]));
    Excel(xlFree, 0, 1, &xVector);
    return v;
}

std::vector < std::vector < long > > xloperToMatrixLong(const LPXLOPER xMulti) {
    XLOPER xMatrix;
    if (xlretSuccess != Excel(xlCoerce, &xMatrix, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::vector < long > > vv;
    XLOPER xScalar;
    for (int i=0; i<xMatrix.val.array.rows; i++) {
        std::vector < long > v;
        for (int j=0; j<xMatrix.val.array.columns; j++) {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, 
                    &xMatrix.val.array.lparray[i * xMatrix.val.array.columns + j], 
                    TempInt(xltypeNum)))
                throw std::exception("xloperToMatrixLong: error on call to xlCoerce");
            v.push_back(xScalar.val.num);
        }
        vv.push_back(v);
    }
    Excel(xlFree, 0, 1, &xMatrix);
    return vv;
}

std::vector < std::vector < double > > xloperToMatrixDouble(const LPXLOPER xMulti) {
    XLOPER xMatrix;
    if (xlretSuccess != Excel(xlCoerce, &xMatrix, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::vector < double > > vv;
    XLOPER xScalar;
    for (int i=0; i<xMatrix.val.array.rows; i++) {
        std::vector < double > v;
        for (int j=0; j<xMatrix.val.array.columns; j++) {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, 
                    &xMatrix.val.array.lparray[i * xMatrix.val.array.columns + j], 
                    TempInt(xltypeNum)))
                throw std::exception("xloperToMatrixLong: error on call to xlCoerce");
            v.push_back(xScalar.val.num);
        }
        vv.push_back(v);
    }
    Excel(xlFree, 0, 1, &xMatrix);
    return vv;
}

std::vector < std::vector < bool > > xloperToMatrixBool(const LPXLOPER xMulti) {
    XLOPER xMatrix;
    if (xlretSuccess != Excel(xlCoerce, &xMatrix, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::vector < bool > > vv;
    XLOPER xScalar;
    for (int i=0; i<xMatrix.val.array.rows; i++) {
        std::vector < bool > v;
        for (int j=0; j<xMatrix.val.array.columns; j++) {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, 
                    &xMatrix.val.array.lparray[i * xMatrix.val.array.columns + j], 
                    TempInt(xltypeBool)))
                throw std::exception("xloperToMatrixLong: error on call to xlCoerce");
            v.push_back(xScalar.val.boolean != 0);
        }
        vv.push_back(v);
    }
    Excel(xlFree, 0, 1, &xMatrix);
    return vv;
}

std::vector < std::vector < std::string > > xloperToMatrixString(const LPXLOPER xMulti) {
    XLOPER xMatrix;
    if (xlretSuccess != Excel(xlCoerce, &xMatrix, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::vector < std::string > > vv;
    for (int i=0; i<xMatrix.val.array.rows; i++) {
        std::vector < std::string > v;
        for (int j=0; j<xMatrix.val.array.columns; j++)
            v.push_back(xloperToString(xMatrix.val.array.lparray[i * xMatrix.val.array.columns + j]));
        vv.push_back(v);
    }
    Excel(xlFree, 0, 1, &xMatrix);
    return vv;
}

std::vector < std::vector < boost::any > > xloperToMatrixAny(const LPXLOPER xMulti) {
    XLOPER xMatrix;
    if (xlretSuccess != Excel(xlCoerce, &xMatrix, 2, xMulti, TempInt(xltypeMulti)))
        throw std::exception("convertArray: error on call to xlCoerce");
    std::vector < std::vector < boost::any > > vv;
    for (int i=0; i<xMatrix.val.array.rows; i++) {
        std::vector < boost::any > v;
        for (int j=0; j<xMatrix.val.array.columns; j++)
           v.push_back(xloperToScalarAny(&xMatrix.val.array.lparray[i * xMatrix.val.array.columns + j]));
        vv.push_back(v);
    }
    Excel(xlFree, 0, 1, &xMatrix);
    return vv;
}

