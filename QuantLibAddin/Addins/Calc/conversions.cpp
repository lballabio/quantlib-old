
/*
 Copyright (C) 2006 Eric Ehlers

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

#include <ql/Utilities/dataparsers.hpp>
#include <Addins/Calc/conversions.hpp>
#include <Addins/Calc/calcutils.hpp>
//#include <qlo/calendar.hpp>
#include <qlo/typefactory.hpp>
#include <oh/objecthandler.hpp>

void calcToScalar(QuantLib::Date &ret, const sal_Int32 &date) {
	ret = QuantLib::Date(date);
}

void calcToScalar(QuantLib::Date &ret, const ANY &date) {
	long dateLong;
	calcToScalar(dateLong, date);
	ret = QuantLib::Date(dateLong);
}

//void calcToScalar(QuantLib::Calendar &ret, const STRING &id2) {
//    std::string id = ouStringToStlString(id2);
//    if (QuantLibAddin::Create<QuantLib::Calendar>().checkType(id)) {
//        ret = QuantLibAddin::Create<QuantLib::Calendar>()(id);
//    } else {
//        OH_GET_REFERENCE(calendarPointer, id,
//            QuantLibAddin::JointCalendar, QuantLib::Calendar)
//        ret = *calendarPointer.get();
//    }
//}

void calcToScalar(QuantLib::Period &ret, const STRING &id) {
    std::string idCpp = ouStringToStlString(id);
    ret = QuantLib::PeriodParser::parse(idCpp);
}

void calcToVector(std::vector<QuantLib::Date> &ret, 
        const SEQSEQ(sal_Int32) &in) {
    for (int i=0; i<in.getLength(); ++i)
        for (int j=0; j<in[i].getLength(); ++j)
            ret.push_back(QuantLib::Date(in[i][j]));

}

void calcToVector(QuantLib::Array &ret, const SEQSEQ(double) &in) {
}

void calcToVector(std::vector<std::string> &ret, const SEQSEQ(ANY) &in) {
	for (int i=0; i<in.getLength(); ++i) {
		for (int j=0; j<in[i].getLength(); ++j) {
			std::string s;
			calcToScalar(s, in[i][j]);
            ret.push_back(s);
		}
	}
}

void calcToVector(std::vector<long> &ret, const SEQSEQ(sal_Int32) &in) {
}

void calcToVector(std::vector<bool> &ret, const SEQSEQ(sal_Int32) &in) {
}

void calcToVector(std::vector<QuantLib::Period> &ret, const SEQSEQ(ANY) &in) {
}

QuantLib::Matrix calcToQlMatrix(const SEQSEQ(double) &in) {
    int rows = in.getLength();
    int cols;
    if (rows)
        cols = in[0].getLength();
    else
        cols = 0;
    QuantLib::Matrix m(rows, cols);
    for (int i=0; i<rows; ++i) {
        SEQ(double) row = in[i];
        for (int j=0; j<cols; ++j) {
            m[i][j] = row[j];
        }
    }
    return m;
}


void scalarToCalc(sal_Int32 &ret, const QuantLib::Date &in) {
    ret = in.serialNumber();
}

void scalarToCalc(double &ret, const QuantLib::Real &in) {
    ret = in;
}

void vectorToCalc(SEQSEQ(sal_Int32) &ret, const std::vector<QuantLib::Date> &v) {
    ret.realloc(v.size());
    for (unsigned int i=0; i<v.size(); ++i) {
        SEQ(sal_Int32) s(1);
        s[0] = v[i].serialNumber();
        ret[i] = s;
    }
}

void vectorToCalc(SEQSEQ(double) &ret, const QuantLib::Array &in) {
    ret.realloc(in.size());
    for (unsigned int i=0; i<in.size(); ++i) {
        SEQ(double) s(1);
        s[0] = in[i];
        ret[i] = s;
    }
}

void matrixToCalc(SEQSEQ(double) &ret, const QuantLib::Matrix &in) {
    ret.realloc(in.rows());
    for (unsigned int i=0; i<in.rows(); ++i) {
        SEQ(double) s(in.columns());
        for (unsigned int j=0; j<in.columns(); ++j) {
            s[j] = in[i][j];
        }
        ret[i] = s;
    }
}

