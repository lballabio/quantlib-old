
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 Copyright (C) 2010 Roland Lichters

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

#ifndef qla_calc_calcutils_hpp
#define qla_calc_calcutils_hpp

#include <boost/any.hpp>

#include <ql/types.hpp>
#include <ql/time/date.hpp>
#include <ql/time/calendar.hpp>
#include <ql/interestrate.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/time/period.hpp>
#include <ql/compounding.hpp>
#include <ql/instruments/swaption.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/instruments/zerocouponinflationswap.hpp>
#include <ql/math/matrix.hpp>
#include <oh/property.hpp>

#include <iostream>

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
//void scalarToCalc(ANY &ret, const ObjectHandler::property_t &value);
void scalarToCalc(long &, const QuantLib::Date &);
void scalarToCalc(int &, const QuantLib::Date &);
void scalarToCalc(STRING &, const QuantLib::Calendar &);
void scalarToCalc(STRING &, const QuantLib::InterestRate &);
void scalarToCalc(STRING &, const QuantLib::BusinessDayConvention &);
void scalarToCalc(STRING &, const QuantLib::DateGeneration::Rule &);
void scalarToCalc(STRING &, const QuantLib::Period &);
void scalarToCalc(STRING &, const QuantLib::Compounding &);
void scalarToCalc(STRING &, const QuantLib::DayCounter &);
void scalarToCalc(STRING &, const QuantLib::Frequency &);
void scalarToCalc(STRING &, const QuantLib::Settlement::Type &);
void scalarToCalc(STRING &, const QuantLib::VanillaSwap::Type &);
void scalarToCalc(STRING &, const QuantLib::ZeroCouponInflationSwap::Type &);
void scalarToCalc(double &, const double &);
#ifndef WIN32
void scalarToCalc(long &, const QuantLib::Natural &);
void scalarToCalc(long &, const QuantLib::Size &);
#endif 

template < class T_FROM, class T_TO >
void vectorToCalc(SEQSEQ( T_TO ) &ret, const std::vector < T_FROM > &v) {
    ret.realloc(v.size());
    for (unsigned int i=0; i<v.size(); ++i) {
        SEQ( T_TO ) s(1);
        scalarToCalc(s[0], v[i]);
        ret[i] = s;
    }
}

template < class T_FROM, class T_TO >
void matrixToCalc(SEQSEQ( T_TO ) &ret, const std::vector < std::vector < T_FROM > >&vv) {
    ret.realloc(vv.size());
    for (unsigned int i=0; i<vv.size(); ++i) {
        std::vector < T_FROM > v = vv[i];
        SEQ( T_TO ) s(v.size());
        for (unsigned int j=0; j<v.size(); ++j)
            scalarToCalc(s[j], v[j]);
        ret[i] = s;
    }
}

template < class T_TO >
void matrixToCalc(SEQSEQ( T_TO ) &ret, const QuantLib::Matrix &vv) {
    ret.realloc(vv.columns());
    for (unsigned int i=0; i<vv.columns(); ++i) {
        SEQ( T_TO ) s(vv.rows());
        for (unsigned int j=0; j<vv.rows(); ++j)
            scalarToCalc(s[j], vv[i][j]);
        ret[i] = s;
    }
}


// conversions from Calc datatypes to native C++ datatypes


#ifndef WIN32
void calcToScalar(QuantLib::Size &ret, const sal_Int32 &value);
void calcToScalar(QuantLib::Size &, const ANY &);
#endif

void calcToScalar(long &, const int &);
void calcToScalar(long &, const ANY &);
void calcToScalar(int &, sal_Int32 &);
void calcToScalar(unsigned int &, const long &);
void calcToScalar(unsigned int &, const int &);

void calcToScalar(double &, const ANY &);

void calcToScalar(bool &, const sal_Int32 &);
void calcToScalar(bool &, const ANY &);
void calcToScalar(double &, const ANY &);

void calcToScalar(std::string &, const ANY &);

void calcToScalar(boost::any &, const ANY &);

void calcToScalar(QuantLib::Natural &, const ANY &);
void calcToScalar(QuantLib::Natural &, const sal_Int32 &);
void calcToScalar(QuantLib::Real &, const double);
void calcToScalar(QuantLib::Period &, const STRING &);
void calcToScalar(QuantLib::Period &ret, const ANY &value);
void calcToScalar(QuantLib::Date &, const sal_Int32&);
void calcToScalar(QuantLib::Date &, const ANY&);

void calcToScalar(ObjectHandler::property_t&, const com::sun::star::uno::Any&);



void calcToVector(std::vector<ObjectHandler::property_t> &ret, 
		  const SEQSEQ(ANY) &in);

template < class T >
void calcToVector(std::vector < T > &ret, const ANY &value) {
  STRING t = value.getValueTypeName();
    if (t.equalsIgnoreAsciiCase(STRFROMANSI("[][]ANY"))) {
        SEQSEQ( ANY ) ss;
        value >>= ss;
        for (int i=0; i<ss.getLength(); ++i) {
            for (int j=0; j<ss[i].getLength(); ++j) {
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
    for (int i=0; i<ss.getLength(); ++i) {
        for (int j=0; j<ss[i].getLength(); ++j) {
            T_TO temp;
            calcToScalar(temp, ss[i][j]);
            ret.push_back(temp);
        }
    }
}

template < class T_FROM, class T_TO >
void calcToMatrix(std::vector < std::vector < T_TO > > &ret, const SEQSEQ( T_FROM )& ss) {
    for (int i=0; i<ss.getLength(); ++i) {
        std::vector < T_TO >v;
        for (int j=0; j<ss[i].getLength(); ++j) {
            T_TO temp;
            calcToScalar(temp, ss[i][j]);
            v.push_back(temp);
        }
        ret.push_back(v);
    }
}


inline 
QuantLib::Matrix calcToQlMatrix(SEQSEQ( double ) in) {
	int size_i = in.getLength(), size_j=0;
	
	if(size_i>0)
		size_j = in[0].getLength();
		
	QuantLib::Matrix ret(size_i, size_j, 0.0);
	for(unsigned int i=0; i<size_i; i++)
		for(unsigned int j=0; j<size_j; j++)
			ret[i][j] = in[i][j];
	
	return ret;
}

inline
QuantLib::Matrix calcToQlMatrix(SEQSEQ( ANY ) in) {
	int size_i = in.getLength(), size_j=0;

	if(size_i>0)
		size_j = in[0].getLength();

	QuantLib::Matrix ret(size_i, size_j, 0.0);
	for(unsigned int i=0; i<size_i; i++)
		for(unsigned int j=0; j<size_j; j++) {
			double dd;
			calcToScalar(dd, in[i][j]);
			ret[i][j] = dd;
		}

	return ret;
}

#endif
