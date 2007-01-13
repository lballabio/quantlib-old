
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

#ifndef qla_varies_hpp
#define qla_varies_hpp

#include <string>
#include <vector>

void variesToScalar(long &ret, const Varies &value, const long &defaultValue = 0);
void variesToScalar(double &ret, const Varies &value, const double &defaultValue = 0);
void variesToScalar(bool &ret, const Varies &value, const bool &defaultValue = false);
void variesToScalar(std::string &ret, const Varies &value, const std::string &defaultValue = "");

template < class T >
void variesToVector(std::vector < T > &ret, const Varies &value) {
}

template < class T >
void variesToMatrix(std::vector < std::vector < T > > &ret, const Varies &value) {
}

template < class T >
void scalarToVaries(Varies *ret, const T &value) {
}

template < class T >
void vectorToVaries(Varies *ret, const std::vector < T > &value) {
}

template < class T >
void matrixToVaries(Varies *ret, const std::vector < std::vector < T > > &value) {
}

#endif

