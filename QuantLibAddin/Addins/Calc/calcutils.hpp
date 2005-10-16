
/*
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

#ifndef qla_calc_calcutils_hpp
#define qla_calc_calcutils_hpp

ANY boostAnyToCalcAny(const boost::any &a);
SEQSEQ( ANY ) propertyVectorToSeqSeq(
    ObjHandler::Properties properties, 
    const STRING &handle);
std::string ouStringToStlString(const STRING& s);
STRING stlStringToOuString(const std::string &s);
ANY stlStringToCalcAny(const std::string &s);
SEQSEQ(ANY) boostAnyToSeqSeq(const ObjHandler::any_ptr &a);

std::vector < long > seqSeqToVectorLong(const SEQSEQ( sal_Int32 )& ss);
std::vector < double > seqSeqToVectorDouble(const SEQSEQ( double )& ss);
std::vector < bool > seqSeqToVectorBool(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::string > seqSeqToVectorString(const SEQSEQ( ANY )& ss);
std::vector < boost::any > seqSeqToVectorAny(const SEQSEQ( ANY )& ss);

std::vector < std::vector < long > > seqSeqToMatrixLong(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::vector < double > > seqSeqToMatrixDouble(const SEQSEQ( double )& ss);
std::vector < std::vector < bool > > seqSeqToMatrixBool(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::vector < std::string > > seqSeqToMatrixString(const SEQSEQ( ANY )& ss);
std::vector < std::vector < boost::any > > seqSeqToMatrixAny(const SEQSEQ( ANY )& ss);

SEQSEQ( sal_Int32 ) vectorLongToSeqSeq(const std::vector < long > &v);
SEQSEQ( double ) vectorDoubleToSeqSeq(const std::vector < double > &v);
SEQSEQ( sal_Int32 ) vectorBoolToSeqSeq(const std::vector < bool > &v);
SEQSEQ( STRING ) vectorStringToSeqSeq(const std::vector < std::string > &v);
SEQSEQ( ANY ) vectorAnyToSeqSeq(const std::vector < boost::any > &v);

SEQSEQ( sal_Int32 ) matrixLongToSeqSeq(const std::vector < std::vector < long > >&v);
SEQSEQ( double ) matrixDoubleToSeqSeq(const std::vector < std::vector < double > >&v);
SEQSEQ( sal_Int32 ) matrixBoolToSeqSeq(const std::vector < std::vector < bool > >&v);
SEQSEQ( STRING ) matrixStringToSeqSeq(const std::vector < std::vector < std::string > >&v);
SEQSEQ( ANY ) matrixAnyToSeqSeq(const std::vector < std::vector < boost::any > >&v);

long anyToScalarLong(const ANY &a, const long &defaultValue = 0);
double anyToScalarDouble(const ANY &a, const double &defaultValue = 0);
bool anyToScalarBool(const ANY &a, const bool &defaultValue = false);
std::string anyToScalarString(const ANY &s, const std::string &defaultValue = "");
boost::any anyToScalarAny(const ANY &a);

std::vector < long > anyToVectorLong(const ANY &a);
std::vector < double > anyToVectorDouble(const ANY &a);
std::vector < bool > anyToVectorBool(const ANY &a);
std::vector < std::string > anyToVectorString(const ANY& s);
std::vector < boost::any > anyToVectorAny(const ANY &a);

std::vector < std::vector < long > > anyToMatrixLong(const ANY &a);
std::vector < std::vector < double > > anyToMatrixDouble(const ANY &a);
std::vector < std::vector < bool > > anyToMatrixBool(const ANY &a);
std::vector < std::vector < std::string > > anyToMatrixString(const ANY& s);
std::vector < std::vector < boost::any > > anyToMatrixAny(const ANY &a);

#endif

