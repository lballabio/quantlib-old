
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

void stringToXloper(XLOPER &xStr, const std::string &s);

void scalarAnyToXloper(
    XLOPER &xScalar, 
    const boost::any &any, 
    const bool &expandVectors = false);

void stringToXloper(XLOPER &xStr, const std::string &s) {
    xStr.xltype = xltypeStr | xlbitDLLFree;
    int len = __min(XL_MAX_STR_LEN, s.length());
    xStr.val.str = new char[ len + 1 ];
    if (!xStr.val.str) 
        throw std::exception("stringToXloper: error calling new");
    if (len)
        strncpy(xStr.val.str + 1, s.c_str(), len);
    xStr.val.str[0] = len;
}

DLLEXPORT XLOPER* getAddress(XLOPER *xCaller) {
    XLOPER xRef;
    static XLOPER xStr;
    if (xlretSuccess != Excel(xlfVolatile, 0, 1, TempBool(0)))
        throw std::exception("getAddress: error on call to xlfVolatile");
    if (xlretSuccess != Excel(xlfGetCell, &xRef, 2, TempInt(1), xCaller))
        throw std::exception("getAddress: error on call to xlfGetCell");
    if (xlretSuccess != Excel(xlCoerce, &xStr, 2, &xRef, TempInt(xltypeStr))) 
        throw std::exception("getAddress: error on call to xlCoerce");
    Excel(xlFree, 0, 1, &xRef);
    xStr.xltype |= xlbitXLFree;
    return &xStr;
}

std::string getHandleFull(const std::string &handle) {
    XLOPER xCaller, xStr;
    try {
        if (xlretSuccess != Excel(xlfCaller, &xCaller, 0))
            throw std::exception("getHandleFull: error on call to xlfCaller");
        if (xlretSuccess != Excel(xlUDF, &xStr, 2, TempStrNoSize("\x0A""getAddress"), &xCaller))
            throw std::exception("getHandleFull: error on call to getAddress");
    } catch(const std::exception &e) {
        Excel(xlFree, 0, 2, &xCaller, &xStr);
        std::ostringstream s1;
        s1 << "getHandleFull: " << e.what();
        throw std::exception(s1.str().c_str());
    }
    std::string ret(handle + '#' + operToScalarString(&xStr));
    Excel(xlFree, 0, 2, &xCaller, &xStr);
    return ret;
}

void stringToChar(char *c, const std::string &s) {
    int len = __min(XL_MAX_STR_LEN, s.length());
    strncpy(c, s.c_str(), len);
    c[len] = 0;
}

void scalarAnyToXloper(
        XLOPER &xScalar, 
        const boost::any &any, 
        const bool &expandVectors) {
    if (any.type() == typeid(XLOPER)) {
        XLOPER xTemp = boost::any_cast<XLOPER>(any);
        if (xlretSuccess != Excel(xlCoerce, &xScalar, 1, &xTemp)) 
            xScalar.xltype = xltypeErr;
    } else if (any.type() == typeid(XLOPER*)) {
        XLOPER *xTemp = boost::any_cast<XLOPER*>(any);
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

void vectorLongToXloper(XLOPER &xVector, std::vector < long > &v) {
    xVector.xltype = xltypeMulti | xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw std::exception("vectorLongToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
        xVector.val.array.lparray[i].xltype = xltypeNum;
        xVector.val.array.lparray[i].val.num = v[i];
    }
}

void vectorDoubleToXloper(XLOPER &xVector, std::vector < double > &v) {
    xVector.xltype = xltypeMulti | xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw std::exception("vectorDoubleToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
        xVector.val.array.lparray[i].xltype = xltypeNum;
        xVector.val.array.lparray[i].val.num = v[i];
    }
}

void vectorBoolToXloper(XLOPER &xVector, std::vector < bool > &v) {
    xVector.xltype = xltypeMulti | xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw std::exception("vectorBoolToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++) {
        xVector.val.array.lparray[i].xltype = xltypeBool;
        xVector.val.array.lparray[i].val.boolean = v[i];
    }
}

void vectorStringToXloper(XLOPER &xVector, std::vector < std::string > &v) {
    xVector.xltype = xltypeMulti | xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw std::exception("vectorStringToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++)
        stringToXloper(xVector.val.array.lparray[i], v[i]);
}

void vectorAnyToXloper(XLOPER &xVector, std::vector < boost::any > &v) {
    xVector.xltype = xltypeMulti | xlbitDLLFree;
    xVector.val.array.rows = v.size();
    xVector.val.array.columns = 1;
    xVector.val.array.lparray = new XLOPER[v.size()]; 
    if (!xVector.val.array.lparray)
        throw std::exception("vectorAnyToXloper: error on call to new");
    for (unsigned int i=0; i<v.size(); i++)
        scalarAnyToXloper(xVector.val.array.lparray[i], v[i]);
}

void matrixLongToXloper(XLOPER &xMatrix, std::vector < std::vector < long > > &vv) {
    xMatrix.xltype = xltypeMulti | xlbitDLLFree;
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
        throw std::exception("matrixLongToXloper: error on call to new");
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
    xMatrix.xltype = xltypeMulti | xlbitDLLFree;
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
        throw std::exception("matrixDoubleToXloper: error on call to new");
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
    xMatrix.xltype = xltypeMulti | xlbitDLLFree;
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
        throw std::exception("matrixBoolToXloper: error on call to new");
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
    xMatrix.xltype = xltypeMulti | xlbitDLLFree;
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
        throw std::exception("matrixStringToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < std::string > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++)
            stringToXloper(xMatrix.val.array.lparray[i * v.size() + j], v[j]);
    }
}

void matrixAnyToXloper(XLOPER &xMatrix, std::vector < std::vector < boost::any > > &vv) {
    xMatrix.xltype = xltypeMulti | xlbitDLLFree;
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
        throw std::exception("matrixAnyToXloper: error on call to new");
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < boost::any > v = vv[i];
        for (unsigned int j=0; j<v.size(); j++)
            scalarAnyToXloper(xMatrix.val.array.lparray[i * v.size() + j], v[j]);
    }
}

long operToScalarLong(const OPER *xScalar, const long &defaultValue) {
    if (xScalar->xltype & (xltypeMissing | xltypeNil))
        return defaultValue;
    else if (xScalar->xltype == xltypeNum)
        return xScalar->val.num;
    else {
        OPER xLong;
        if (xlretSuccess != Excel(xlCoerce, &xLong, 2, xScalar, TempInt(xltypeInt)))
            throw std::exception("operToScalarLong: error on call to xlCoerce");
        return xLong.val.w;
    }
}

double operToScalarDouble(const OPER *xScalar, const double &defaultValue) {
    if (xScalar->xltype & (xltypeMissing | xltypeNil))
        return defaultValue;
    else if (xScalar->xltype == xltypeNum)
        return xScalar->val.num;
    else {
        OPER xDouble;
        if (xlretSuccess != Excel(xlCoerce, &xDouble, 2, xScalar, TempInt(xltypeNum)))
            throw std::exception("operToScalarDouble: error on call to xlCoerce");
        return xDouble.val.num;
    }
}

bool operToScalarBool(const OPER *xScalar, const bool &defaultValue) {
    if (xScalar->xltype & (xltypeMissing | xltypeNil))
        return defaultValue;
    else if (xScalar->xltype == xltypeBool)
        return xScalar->val.boolean != 0;
    else {
        OPER xBool;
        if (xlretSuccess != Excel(xlCoerce, &xBool, 2, xScalar, TempInt(xltypeBool)))
            throw std::exception("operToScalarBool: error on call to xlCoerce");
        return xBool.val.boolean != 0;
    }
}

std::string operToScalarString(const OPER *xScalar, const std::string &defaultValue) {
    if (xScalar->xltype & (xltypeMissing | xltypeNil))
        return defaultValue;

    OPER xTemp;
    const OPER *xString;
    bool needToFree = false;
    std::string ret;

    if (xScalar->xltype == xltypeStr)
        xString = xScalar;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xScalar, TempInt(xltypeStr)))
            throw std::exception("operToScalarString: error on call to xlCoerce");
        xString = &xTemp;
        needToFree = true;
    }

    if (xString->val.str[0])
        ret.assign(xString->val.str + 1, xString->val.str[0]);

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

boost::any operToScalarAny(const OPER *xScalar) {
    if (xScalar->xltype & (xltypeMissing | xltypeNil))
        return boost::any();
    else if (xScalar->xltype == xltypeNum)
        return xScalar->val.num;
    else if (xScalar->xltype == xltypeBool)
        return xScalar->val.boolean != 0;
    else if (xScalar->xltype == xltypeStr)
        return operToScalarString(xScalar);
    else
        throw std::exception("operToScalarAny: unexpected datatype");
}

std::vector < long >fpToVectorLong(const FP *fpVector) {
    std::vector < long > ret;
    for (int i=0; i<fpVector->rows * fpVector->columns; i++)
        ret.push_back(fpVector->array[i]);
    return ret;
}

std::vector < double >fpToVectorDouble(const FP *fpVector) {
    std::vector < double > ret;
    for (int i=0; i<fpVector->rows * fpVector->columns; i++)
        ret.push_back(fpVector->array[i]);
    return ret;
}

std::vector < long > operToVectorLong(const OPER *xVector) {
    std::vector < long > ret;

    if (xVector->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xVector->xltype == xltypeMulti)
        xMulti = xVector;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti)))
            throw std::exception("operToVectorLong: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++) {
        if (xMulti->val.array.lparray[i].xltype == xltypeNum)
            ret.push_back(xMulti->val.array.lparray[i].val.num);
        else {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeInt)))
                throw std::exception("operToVectorLong: error on call to xlCoerce");
            ret.push_back(xScalar.val.w);
        }
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < double > operToVectorDouble(const OPER *xVector) {
    std::vector < double > ret;

    if (xVector->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xVector->xltype == xltypeMulti)
        xMulti = xVector;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti)))
            throw std::exception("operToVectorDouble: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++) {
        if (xMulti->val.array.lparray[i].xltype == xltypeNum)
            ret.push_back(xMulti->val.array.lparray[i].val.num);
        else {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeNum)))
                throw std::exception("operToVectorDouble: error on call to xlCoerce");
            ret.push_back(xScalar.val.num);
        }
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < bool > operToVectorBool(const OPER *xVector) {
    std::vector < bool > ret;

    if (xVector->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xVector->xltype == xltypeMulti)
        xMulti = xVector;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti)))
            throw std::exception("operToVectorBool: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++) {
        if (xMulti->val.array.lparray[i].xltype == xltypeBool)
            ret.push_back(xMulti->val.array.lparray[i].val.boolean != 0);
        else {
            if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeBool)))
                throw std::exception("operToVectorBool: error on call to xlCoerce");
            ret.push_back(xScalar.val.boolean != 0);
        }
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::string > operToVectorString(const OPER *xVector) {
    std::vector < std::string > ret;

    if (xVector->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp;
    const OPER *xMulti;
    bool needToFree = false;

    if (xVector->xltype == xltypeMulti)
        xMulti = xVector;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti)))
            throw std::exception("operToVectorString: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++)
        ret.push_back(operToScalarString(&xMulti->val.array.lparray[i]));

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < boost::any > operToVectorAny(const OPER *xVector) {
    std::vector < boost::any > ret;

    if (xVector->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp;
    const OPER *xMulti;
    bool needToFree = false;

    if (xVector->xltype == xltypeMulti)
        xMulti = xVector;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti)))
            throw std::exception("operToVectorAny: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++)
        ret.push_back(operToScalarAny(&xMulti->val.array.lparray[i]));

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::vector < long > > fpToMatrixLong(const FP *fpMatrix) {
    std::vector < std::vector < long > > ret;
    for (int i=0; i<fpMatrix->rows; i++) {
        std::vector < long > row;
        for (int j=0; j<fpMatrix->columns; j++)
            row.push_back(fpMatrix->array[i * fpMatrix->rows + j]);
        ret.push_back(row);
    }
    return ret;
}

std::vector < std::vector < double > > fpToMatrixDouble(const FP *fpMatrix) {
    std::vector < std::vector < double > > ret;
    for (int i=0; i<fpMatrix->rows; i++) {
        std::vector < double > row;
        for (int j=0; j<fpMatrix->columns; j++)
            row.push_back(fpMatrix->array[i * fpMatrix->rows + j]);
        ret.push_back(row);
    }
    return ret;
}

std::vector < std::vector < long > > operToMatrixLong(const OPER *xMatrix) {
    std::vector < std::vector < long > > ret;

    if (xMatrix->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xMatrix->xltype == xltypeMulti)
        xMulti = xMatrix;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti)))
            throw std::exception("operToMatrixLong: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows; i++) {
        std::vector < long > row;
        for (int j=0; j<xMulti->val.array.columns; j++) {
            if (xMulti->val.array.lparray[i].xltype == xltypeNum)
                row.push_back(xMulti->val.array.lparray[i].val.num);
            else {
                if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeInt)))
                    throw std::exception("operToMatrixLong: error on call to xlCoerce");
                row.push_back(xScalar.val.w);
            }
        }
        ret.push_back(row);
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::vector < double > > operToMatrixDouble(const OPER *xMatrix) {
    std::vector < std::vector < double > > ret;

    if (xMatrix->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xMatrix->xltype == xltypeMulti)
        xMulti = xMatrix;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti)))
            throw std::exception("operToMatrixDouble: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows; i++) {
        std::vector < double > row;
        for (int j=0; j<xMulti->val.array.columns; j++) {
            if (xMulti->val.array.lparray[i].xltype == xltypeNum)
                row.push_back(xMulti->val.array.lparray[i].val.num);
            else {
                if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeNum)))
                    throw std::exception("operToMatrixDouble: error on call to xlCoerce");
                row.push_back(xScalar.val.num);
            }
        }
        ret.push_back(row);
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::vector < bool > > operToMatrixBool(const OPER *xMatrix) {
    std::vector < std::vector < bool > > ret;

    if (xMatrix->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp, xScalar;
    const OPER *xMulti;
    bool needToFree = false;

    if (xMatrix->xltype == xltypeMulti)
        xMulti = xMatrix;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti)))
            throw std::exception("operToMatrixBool: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows; i++) {
        std::vector < bool > row;
        for (int j=0; j<xMulti->val.array.columns; j++) {
            if (xMulti->val.array.lparray[i].xltype == xltypeBool)
                row.push_back(xMulti->val.array.lparray[i].val.boolean != 0);
            else {
                if (xlretSuccess != Excel(xlCoerce, &xScalar, 2, &xMulti->val.array.lparray[i], TempInt(xltypeBool)))
                    throw std::exception("operToMatrixBool: error on call to xlCoerce");
                row.push_back(xScalar.val.boolean != 0);
            }
        }
        ret.push_back(row);
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::vector < std::string > > operToMatrixString(const OPER *xMatrix) {
    std::vector < std::vector < std::string > > ret;

    if (xMatrix->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp;
    const OPER *xMulti;
    bool needToFree = false;

    if (xMatrix->xltype == xltypeMulti)
        xMulti = xMatrix;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti)))
            throw std::exception("operToMatrixString: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows; i++) {
        std::vector < std::string > row;
        for (int j=0; j<xMulti->val.array.columns; j++)
            row.push_back(operToScalarString(&xMulti->val.array.lparray[i * xMulti->val.array.columns + j]));
        ret.push_back(row);
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

std::vector < std::vector < boost::any > > operToMatrixAny(const OPER *xMatrix) {
    std::vector < std::vector < boost::any > > ret;

    if (xMatrix->xltype & (xltypeMissing | xltypeNil))
        return ret;

    OPER xTemp;
    const OPER *xMulti;
    bool needToFree = false;

    if (xMatrix->xltype == xltypeMulti)
        xMulti = xMatrix;
    else {
        if (xlretSuccess != Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti)))
            throw std::exception("operToMatrixAny: error on call to xlCoerce");
        xMulti = &xTemp;
        needToFree = true;
    }

    for (int i=0; i<xMulti->val.array.rows; i++) {
        std::vector < boost::any > row;
        for (int j=0; j<xMulti->val.array.columns; j++)
           row.push_back(operToScalarAny(&xMulti->val.array.lparray[i * xMulti->val.array.columns + j]));
        ret.push_back(row);
    }

    if (needToFree)
        Excel(xlFree, 0, 1, &xTemp);

    return ret;
}

