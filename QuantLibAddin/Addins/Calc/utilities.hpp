
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

#ifndef calc_utilities_h
#define calc_utilities_h

ANY anyToANY(const ObjHandler::any_ptr &a); // convert boost::any to Calc Any
SEQSEQ( ANY ) getArray(ObjHandler::Properties properties,
                       STRING handle);
std::string OUStringToString(const STRING& s1);
ANY stringToANY(const std::string &s);

std::vector < long > longSequenceToVector(const SEQSEQ(long)& s);
std::vector < double > doubleSequenceToVector(const SEQSEQ(double)& s);
std::vector < std::string > stringSequenceToVector(const SEQSEQ(STRING)& s);
std::vector < std::vector < double > >doubleSequenceToMatrix(const SEQSEQ(double)& s);

#endif
