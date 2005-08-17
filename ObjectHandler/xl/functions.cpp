
/*
 Copyright (C) 2005 Eric Ehlers

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

#include <oh/utilities.hpp>
#include <oh/exception.hpp>
#include <string>
#include <cctype>
#include <vector>
#include <windows.h>
#include <xl/xlcall.h>
#include <xl/framewrk.hpp>

using namespace ObjHandler;

#define VECTOR "<VECTOR>"
#define MATRIX "<MATRIX>"
#define XL_MAX_STR_LEN 255

// suppress VC8 'strncpy deprecated' warning
#if defined BOOST_MSVC
#pragma warning(disable : 4996)
#endif

void stringToXloper(XLOPER &xStr, const std::string &s);

void scalarAnyToXloper(
    XLOPER &xScalar, 
    const boost::any &any, 
    const bool &expandVectors = false);

void vectorLongToXloper(XLOPER &xVector, std::vector < long > &v);
void vectorDoubleToXloper(XLOPER &xVector, std::vector < double > &v);
void vectorBoolToXloper(XLOPER &xVector, std::vector < bool > &v);
void vectorStringToXloper(XLOPER &xVector, std::vector < std::string > &v);
void vectorAnyToXloper(XLOPER &xVector, std::vector < boost::any > &v);

void matrixLongToXloper(XLOPER &xMatrix, std::vector < std::vector < long > > &vv);
void matrixDoubleToXloper(XLOPER &xMatrix, std::vector < std::vector < double > > &vv);
void matrixBoolToXloper(XLOPER &xMatrix, std::vector < std::vector < bool > > &vv);
void matrixStringToXloper(XLOPER &xMatrix, std::vector < std::vector < std::string > > &vv);
void matrixAnyToXloper(XLOPER &xMatrix, std::vector < std::vector < boost::any > > &vv);

DLLEXPORT char* ohVersion() {
    try {
        static char ret[XL_MAX_STR_LEN];
        std::string ver = OBJHANDLER_VERSION;
        int len = __min(XL_MAX_STR_LEN - 1, ver.length());
        strncpy(ret, ver.c_str(), len);
        ret[len] = 0;
        return ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_VERSION: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT LPXLOPER fieldNames(char *handleObject) {
    static XLOPER xRet;
    xRet.val.array.lparray = 0;
    try {
        Properties properties = queryObject(std::string(handleObject));
        xRet.xltype = xltypeMulti;
        xRet.xltype |= xlbitDLLFree;
        xRet.val.array.rows = properties.size();
        xRet.val.array.columns = 1;
        xRet.val.array.lparray = new XLOPER[properties.size()];
        if (!xRet.val.array.lparray)
            throw Exception("error on call to new");
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            stringToXloper(xRet.val.array.lparray[i], property.name().c_str());
        }
        return &xRet;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_FIELD_NAMES: ") + e.what(), 2);
        if (xRet.val.array.lparray)
            delete [] xRet.val.array.lparray;
        return 0;
    }
}

DLLEXPORT LPXLOPER fieldValue(char *handleObject, char *fieldName) {
    try {
        Properties properties = queryObject(std::string(handleObject));
        static XLOPER xRet;
        std::string fieldNameUpper = fieldName;
        std::transform(fieldNameUpper.begin(), fieldNameUpper.end(),
               fieldNameUpper.begin(), (int(*)(int)) toupper);
        for (unsigned int i=0; i<properties.size(); i++) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            if (property.name().compare(fieldNameUpper) == 0) {
                scalarAnyToXloper(xRet, *a, true);
                return &xRet;
            }
        }
        throw Exception(std::string("no field with name ") + fieldNameUpper);
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_FIELD_VALUE: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* deleteObject(char *handleObject) {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().deleteObject(handleObject);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_DELETE_OBJECT: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* deleteAllObjects() {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().deleteAllObjects();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_DELETE_ALL_OBJECTS: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT char* setLogFile(char *logFileName, long *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        int lvl = *logLevel ? *logLevel : 4;
        ObjHandler::setLogFile(std::string(logFileName), lvl);
        int len = __min(XL_MAX_STR_LEN - 1, strlen(logFileName));
        strncpy(ret, logFileName, len);
        ret[len] = 0;
        return ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT char* logMessage(char *message, long *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        int lvl = *logLevel ? *logLevel : 4;
        ObjHandler::logMessage(std::string(message), lvl);
        int len = __min(XL_MAX_STR_LEN - 1, strlen(message));
        strncpy(ret, message, len);
        ret[len] = 0;
        return ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT long* setLogLevel(long *logLevel) {
    try {
        static long ret;
        ObjHandler::setLogLevel(*logLevel);
        ret = *logLevel;
        return &ret;
    } catch (...) {
        return 0;
    }
}

DLLEXPORT short int* logObject(char *handleObject) {
    try {
        static short int ret = TRUE;
        ObjHandler::logObject(handleObject);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_LOG_OBJECT: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int* logAllObjects() {
    try {
        static short int ret = TRUE;
        ObjHandler::logAllObjects();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: OH_LOG_ALL_OBJECTS: ") + e.what(), 2);
        return 0;
    }
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
