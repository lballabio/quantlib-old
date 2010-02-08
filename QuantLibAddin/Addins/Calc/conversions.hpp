
/*
 Copyright (C) 2006, 2007, 2008 Eric Ehlers
 Copyright (C) 2009 Roland Lichters

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

#ifndef qla_calc_conversions_hpp
#define qla_calc_conversions_hpp

#include <oh/property.hpp>
#include <ql/time/date.hpp>
#include <ql/time/calendar.hpp>
#include <ql/math/matrix.hpp>
#include <qldefs.hpp>
#include <sal/types.h>
#include <vector>

// temp fix pending Calc implementation of CoerceQuoteHandle
#include <qlo/quotes.hpp>

void calcToScalar(QuantLib::Date &, const sal_Int32&);
void calcToScalar(QuantLib::Date &, const ANY&);
void calcToScalar(ObjectHandler::property_t &, const ANY&);
// RL: added
void calcToScalar(ObjectHandler::property_t &, const STRING&);
void calcToScalar(QuantLib::Calendar &, const STRING &id);
void calcToScalar(QuantLib::Period &, const STRING &id);
void calcToVector(std::vector<QuantLib::Date> &, const SEQSEQ(sal_Int32) &);
void calcToVector(QuantLib::Array &, const SEQSEQ(double) &);
void calcToVector(std::vector<std::string> &, const SEQSEQ(ANY) &);
void calcToVector(std::vector<long> &, const SEQSEQ(sal_Int32) &);
void calcToVector(std::vector<bool> &, const SEQSEQ(sal_Int32) &);
void calcToVector(std::vector<QuantLib::Period> &, const SEQSEQ(ANY) &);
void calcToVector(std::vector<boost::any> &, const SEQSEQ(ANY) &);
QuantLib::Matrix calcToQlMatrix(const SEQSEQ(double) &);

void scalarToCalc(sal_Int32 &, const QuantLib::Date &);
// RL: added
void scalarToCalc(STRING &, const QuantLib::Calendar &);

// Function below required on 64-bit systems but on 32-bit systems it
// conflicts with sal_Int32 override.
// FIXME Need a #define that specifically distinguishes 32/64-bit
#if defined(__GNUC__) && defined(__x86_64__)
void scalarToCalc(long &, const QuantLib::Date &);
#endif

void scalarToCalc(double &, const QuantLib::Real &);
void vectorToCalc(SEQSEQ(sal_Int32) &, const std::vector<QuantLib::Date> &);
void vectorToCalc(SEQSEQ(double) &, const QuantLib::Array &);
void matrixToCalc(SEQSEQ(double) &, const QuantLib::Matrix &);

#endif

