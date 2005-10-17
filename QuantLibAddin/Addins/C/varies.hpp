
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
boost::any variesToScalarAny(const Varies &v, const boost::any &defaultValue);

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

void scalarLongToVaries(const long &in, Varies *v);
void scalarDoubleToVaries(const double &in, Varies *v);
void scalarBoolToVaries(const bool &in, Varies *v);
void scalarStringToVaries(const std::string &in, Varies *v);

void vectorLongToVaries(const std::vector < long > &in, Varies *v);
void vectorDoubleToVaries(const std::vector < double > &in, Varies *v);
void vectorBoolToVaries(const std::vector < bool > &in, Varies *v);
void vectorStringToVaries(const std::vector < std::string > &in, Varies *v);
void vectorAnyToVaries(const std::vector < boost::any > &in, Varies *v);

void matrixLongToVaries(const std::vector < std::vector < long > > &in, Varies *v);
void matrixDoubleToVaries(const std::vector < std::vector < double > > &in, Varies *v);
void matrixBoolToVaries(const std::vector < std::vector < bool > > &in, Varies *v);
void matrixStringToVaries(const std::vector < std::vector < std::string > > &in, Varies *v);
void matrixAnyToVaries(const std::vector < std::vector < boost::any > > &in, Varies *v);

//void propertyVectorToVariesList(const ObjHandler::Properties &properties, 
//        VariesList *variesList);

#endif

