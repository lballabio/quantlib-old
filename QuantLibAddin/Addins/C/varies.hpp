
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

long variesToScalarLong(const Varies &v, const long &defaultValue);
double variesToScalarDouble(const Varies &v, const double &defaultValue);
bool variesToScalarBool(const Varies &v, const bool &defaultValue);
std::string variesToScalarString(const Varies &v, const std::string &defaultValue);

std::vector < long > variesToVectorLong(const Varies &v);
std::vector < double > variesToVectorDouble(const Varies &v);
std::vector < bool > variesToVectorBool(const Varies &v);
std::vector < std::string > variesToVectorString(const Varies &v);
std::vector < boost::any > variesToVectorAny(const Varies &v);

std::vector < std::vector < long > > variesToMatrixLong(const Varies &v);
std::vector < std::vector < double > > variesToMatrixDouble(const Varies &v);
std::vector < std::vector < bool > > variesToMatrixBool(const Varies &v);
std::vector < std::vector < std::string > > variesToMatrixString(const Varies &v);
std::vector < std::vector < boost::any > > variesToMatrixAny(const Varies &v);

//void propertyVectorToVariesList(const ObjHandler::Properties &properties, 
//        VariesList *variesList);
//boost::any variesToBoostAny(const Varies &v);

#endif

