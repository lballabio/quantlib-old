
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
boost::any calcAnyToBoostAny(const ANY &a);
SEQSEQ( ANY ) propertyVectorToSeqSeq(ObjHandler::Properties properties, const STRING &handle);
std::string OUStringToStlString(const STRING& s);
STRING stlStringToOuString(const std::string &s);
ANY stringToANY(const std::string &s);
std::string calcAnyToStlString(const ANY& s);

std::vector < long >SeqSeqToVectorLong(const SEQSEQ( sal_Int32 )& ss);
std::vector < double >SeqSeqToVectorDouble(const SEQSEQ( double )& ss);
std::vector < bool >SeqSeqToVectorBool(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::string >SeqSeqToVectorString(const SEQSEQ( ANY )& ss);
std::vector < boost::any >SeqSeqToVectorAny(const SEQSEQ( ANY )& ss);

std::vector < std::vector < long > >SeqSeqToMatrixLong(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::vector < double > >SeqSeqToMatrixDouble(const SEQSEQ( double )& ss);
std::vector < std::vector < bool > >SeqSeqToMatrixBool(const SEQSEQ( sal_Int32 )& ss);
std::vector < std::vector < std::string > >SeqSeqToMatrixString(const SEQSEQ( ANY )& ss);
std::vector < std::vector < boost::any > >SeqSeqToMatrixAny(const SEQSEQ( ANY )& ss);

SEQSEQ( sal_Int32 ) VectorLongToSeqSeq(const std::vector < long > &v);
SEQSEQ( double ) VectorDoubleToSeqSeq(const std::vector < double > &v);
SEQSEQ( sal_Int32 ) VectorBoolToSeqSeq(const std::vector < bool > &v);
SEQSEQ( STRING ) VectorStringToSeqSeq(const std::vector < std::string > &v);
SEQSEQ( ANY ) VectorAnyToSeqSeq(const std::vector < boost::any > &v);

SEQSEQ( sal_Int32 ) MatrixLongToSeqSeq(const std::vector < std::vector < long > >&v);
SEQSEQ( double ) MatrixDoubleToSeqSeq(const std::vector < std::vector < double > >&v);
SEQSEQ( sal_Int32 ) MatrixBoolToSeqSeq(const std::vector < std::vector < bool > >&v);
SEQSEQ( STRING ) MatrixStringToSeqSeq(const std::vector < std::vector < std::string > >&v);
SEQSEQ( ANY ) MatrixAnyToSeqSeq(const std::vector < std::vector < boost::any > >&v);

#endif

