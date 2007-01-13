
/*
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

#ifndef qla_calc_calcutils_hpp
#define qla_calc_calcutils_hpp

#include <boost/any.hpp>

std::string ouStringToStlString(const STRING &s);
ANY stlStringToCalcAny(const std::string &s);

// conversions from native C++ datatypes to Calc datatypes

template < class T >
void scalarToCalc(T &ret, const T &value) {
    ret = value;
}

void scalarToCalc(sal_Int32 &ret, const bool &value);
void scalarToCalc(STRING &ret, const std::string &value);
void scalarToCalc(ANY &ret, const boost::any &value);

template < class T_FROM, class T_TO >
void vectorToCalc(SEQSEQ( T_TO ) &ret, const std::vector < T_FROM > &v) {
    ret.realloc(v.size());
    for (unsigned int i=0; i<v.size(); i++) {
        SEQ( T_TO ) s(1);
        scalarToCalc(s[0], v[i]);
        ret[i] = s;
    }
}

template < class T_FROM, class T_TO >
void matrixToCalc(SEQSEQ( T_TO ) &ret, const std::vector < std::vector < T_FROM > >&vv) {
    ret.realloc(vv.size());
    for (unsigned int i=0; i<vv.size(); i++) {
        std::vector < T_FROM > v = vv[i];
        SEQ( T_TO ) s(v.size());
        for (unsigned int j=0; j<v.size(); j++)
            scalarToCalc(s[j], v[j]);
        ret[i] = s;
    }
}

// conversions from Calc datatypes to native C++ datatypes

template < class T >
void calcToScalar(T &ret, const T &value) {
    ret = value;
}

void calcToScalar(bool &ret, const sal_Int32 &value);
void calcToScalar(boost::any &ret, const ANY &value);
void calcToScalar(long &ret, const ANY &value, const long &defaultValue = 0);
void calcToScalar(double &ret, const ANY &value, const double &defaultValue = 0);
void calcToScalar(bool &ret, const ANY &value, const bool &defaultValue = false);
void calcToScalar(std::string &ret, const ANY &value, const std::string &defaultValue = "");

template < class T >
void calcToVector(std::vector < T > &ret, const ANY &value) {
    STRING t = value.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        value >>= ss;
        for (int i=0; i<ss.getLength(); i++) {
            for (int j=0; j<ss[i].getLength(); j++) {
                T temp;
                calcToScalar(temp, ss[i][j]);
                ret.push_back(temp);
            }
        }
    } else {
        T temp;
        calcToScalar(temp, value);
        ret.push_back(temp);
    }
}

template < class T_FROM, class T_TO >
void calcToVector(std::vector < T_TO > &ret, const SEQSEQ( T_FROM )& ss) {
    for (int i=0; i<ss.getLength(); i++) {
        for (int j=0; j<ss[i].getLength(); j++) {
            T_TO temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

template < class T_FROM, class T_TO >
void calcToMatrix(std::vector < std::vector < T_TO > > &ret, const SEQSEQ( T_FROM )& ss) {
    for (int i=0; i<ss.getLength(); i++) {
        std::vector < T_TO >v;
        for (int j=0; j<ss[i].getLength(); j++) {
            T_TO temp;
            calcToScalar(temp, ss[i][j]);
            v.push_back(temp);
        }
        ret.push_back(v);
    }
}

#endif

