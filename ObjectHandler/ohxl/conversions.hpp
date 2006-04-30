
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef ohxl_conversions_hpp
#define ohxl_conversions_hpp

#include <oh/objhandlerdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <vector>
#include <string>
#include <boost/any.hpp>

namespace ObjHandler {

    DLL_API std::string getHandleFull(const std::string &handle);
    DLL_API void stringToChar(char *c, const std::string &s);

    // conversions from native C++ datatypes to Excel datatypes

    DLL_API void scalarToXloper(XLOPER &xLong, const long &value);
    DLL_API void scalarToXloper(XLOPER &xDouble, const double &value);
    DLL_API void scalarToXloper(XLOPER &xBoolean, const bool &value);
    DLL_API void scalarToXloper(XLOPER &xString, const std::string &value);
    DLL_API void scalarToXloper(
        XLOPER &xAny, 
        const boost::any &value, 
        const bool &expandVectors = true);

    template < class T >
    void vectorToXloper(XLOPER &xVector, const std::vector < T > &v) {
        if (v.empty()) {
            xVector.xltype = xltypeNum;
            xVector.val.num = 0;
            return;
        }
        xVector.xltype = xltypeMulti | xlbitDLLFree;
        xVector.val.array.rows    = v.size();
        xVector.val.array.columns = 1;
        xVector.val.array.lparray = new XLOPER[v.size()]; 
        if (!xVector.val.array.lparray)
            throw std::exception("vectorToXloper: error on call to new");
        for (unsigned int i=0; i<v.size(); i++)
            scalarToXloper(xVector.val.array.lparray[i], v[i]);
    }

    template < class T >
    void matrixToXloper(XLOPER &xMatrix, const std::vector < std::vector < T > > &vv) {
        if (vv.empty() || vv[0].empty()) {
            xMatrix.xltype = xltypeNum;
            xMatrix.val.num = 0;
            return;
        }
        xMatrix.xltype = xltypeMulti | xlbitDLLFree;
        xMatrix.val.array.rows    = vv.size();
        xMatrix.val.array.columns = vv[0].size();
        xMatrix.val.array.lparray = new XLOPER[xMatrix.val.array.rows * xMatrix.val.array.columns]; 
        if (!xMatrix.val.array.lparray)
            throw std::exception("matrixToXloper: error on call to new");
        for (unsigned int i=0; i<vv.size(); i++) {
            std::vector < T > v = vv[i];
            for (unsigned int j=0; j<v.size(); j++)
                scalarToXloper(xMatrix.val.array.lparray[i * v.size() + j], v[j]);
        }
    }

    // conversions from Excel datatypes to native C++ datatypes

    DLL_API void operToScalar(long &ret, const OPER &xScalar, const long &defaultValue = 0);
    DLL_API void operToScalar(double &ret, const OPER &xScalar, const double &defaultValue = 0);
    DLL_API void operToScalar(bool &ret, const OPER &xScalar, const bool &defaultValue = false);
    DLL_API void operToScalar(std::string &ret, const OPER &xScalar, const std::string &defaultValue = "");
    DLL_API void operToScalar(boost::any &ret, const OPER &xScalar);

    template < class T >
    void fpToVector(std::vector < T > &ret, const FP *fpVector) {
        for (int i=0; i<fpVector->rows * fpVector->columns; i++)
            ret.push_back(fpVector->array[i]);
    }

    template < class T >
    void operToVector(std::vector < T > &ret, const OPER *xVector) {
        OPER xTemp;
        bool needToFree = false;
        try {
            if (xVector->xltype & xltypeErr)
                throw std::exception("input value is #NULL (xltypeErr)");
            if (xVector->xltype & (xltypeMissing | xltypeNil))
                return;

            const OPER *xMulti;

            if (xVector->xltype == xltypeMulti)
                xMulti = xVector;
            else {
                Excel(xlCoerce, &xTemp, 2, xVector, TempInt(xltypeMulti));
                xMulti = &xTemp;
                needToFree = true;
            }

            for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; i++) {
                T value;
                operToScalar(value, xMulti->val.array.lparray[i]);
                ret.push_back(value);
            }

            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);

        } catch (const std::exception &e) {
            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);
            std::ostringstream msg;
            msg << "operToVector: " << e.what();
            throw std::exception(msg.str().c_str());
        }
    }

    template < class T >
    void fpToMatrix( 
    std::vector < std::vector < T > > &ret, 
    const FP *fpMatrix) {
        for (int i=0; i<fpMatrix->rows; i++) {
            std::vector < T > row;
            for (int j=0; j<fpMatrix->columns; j++)
                row.push_back(fpMatrix->array[i * fpMatrix->columns + j]);
            ret.push_back(row);
        }
    }

    template < class T >
    void operToMatrix(
    std::vector < std::vector < T > > &ret, 
    const OPER *xMatrix) {
        OPER xTemp;
        bool needToFree = false;
        try {
            if (xMatrix->xltype & xltypeErr)
                throw std::exception("input value is #NULL (xltypeErr)");
            if (xMatrix->xltype & (xltypeMissing | xltypeNil))
                return;

            const OPER *xMulti;

            if (xMatrix->xltype == xltypeMulti)
                xMulti = xMatrix;
            else {
                Excel(xlCoerce, &xTemp, 2, xMatrix, TempInt(xltypeMulti));
                xMulti = &xTemp;
                needToFree = true;
            }

            for (int i=0; i<xMulti->val.array.rows; i++) {
                std::vector < T > row;
                for (int j=0; j<xMulti->val.array.columns; j++) {
                    T value;
                    operToScalar(value, xMulti->val.array.lparray[i * xMulti->val.array.columns + j]);
                    row.push_back(value);
                }
                ret.push_back(row);
            }

            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);

        } catch (const std::exception &e) {
            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);
            std::ostringstream msg;
            msg << "operToMatrix: " << e.what();
            throw std::exception(msg.str().c_str());
        }
    }

}

#endif
