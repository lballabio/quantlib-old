
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

#include <oh/objhandler.hpp>
extern "C" {
#include <Addins/C/varies.h>
}
#include <sstream>

using namespace ObjHandler;

long variesToScalarLong(const Varies &v, const long &defaultValue) {
    long ret;
    return ret;
}

double variesToScalarDouble(const Varies &v, const double &defaultValue){
    double ret;
    return ret;
}

bool variesToScalarBool(const Varies &v, const bool &defaultValue){
    bool ret;
    return ret;
}

std::string variesToScalarString(const Varies &v, const std::string &defaultValue){
    std::string ret;
    return ret;
}

boost::any variesToScalarAny(const Varies &v, const boost::any &defaultValue){
    boost::any ret;
    return ret;
}


std::vector < long > variesToVectorLong(const Varies &v){
    std::vector < long > ret;
    return ret;
}

std::vector < double > variesToVectorDouble(const Varies &v){
    std::vector < double > ret;
    return ret;
}

std::vector < bool > variesToVectorBool(const Varies &v){
    std::vector < bool > ret;
    return ret;
}

std::vector < std::string > variesToVectorString(const Varies &v){
    std::vector < std::string > ret;
    return ret;
}

std::vector < boost::any > variesToVectorAny(const Varies &v){
    std::vector < boost::any > ret;
    return ret;
}


std::vector < std::vector < long > > variesToMatrixLong(const Varies &v){
    std::vector < std::vector < long > > ret;
    return ret;
}

std::vector < std::vector < double > > variesToMatrixDouble(const Varies &v){
    std::vector < std::vector < double > > ret;
    return ret;
}

std::vector < std::vector < bool > > variesToMatrixBool(const Varies &v){
    std::vector < std::vector < bool > > ret;
    return ret;
}

std::vector < std::vector < std::string > > variesToMatrixString(const Varies &v){
    std::vector < std::vector < std::string > > ret;
    return ret;
}

std::vector < std::vector < boost::any > > variesToMatrixAny(const Varies &v){
    std::vector < std::vector < boost::any > > ret;
    return ret;
}


void scalarLongToVaries(const long &in, Varies *v) {
}

void scalarDoubleToVaries(const double &in, Varies *v) {
}

void scalarBoolToVaries(const bool &in, Varies *v) {
}

void scalarStringToVaries(const std::string &in, Varies *v) {
}


void vectorLongToVaries(const std::vector < long > &in, Varies *v) {
}

void vectorDoubleToVaries(const std::vector < double > &in, Varies *v) {
}

void vectorBoolToVaries(const std::vector < bool > &in, Varies *v) {
}

void vectorStringToVaries(const std::vector < std::string > &in, Varies *v) {
}

void vectorAnyToVaries(const std::vector < boost::any > &in, Varies *v) {
}


void matrixLongToVaries(const std::vector < std::vector < long > > &in, Varies *v) {
}

void matrixDoubleToVaries(const std::vector < std::vector < double > > &in, Varies *v) {
}

void matrixBoolToVaries(const std::vector < std::vector < bool > > &in, Varies *v) {
}

void matrixStringToVaries(const std::vector < std::vector < std::string > > &in, Varies *v) {
}

void matrixAnyToVaries(const std::vector < std::vector < boost::any > > &in, Varies *v) {
}



//void propertyVectorToVariesList(
//        const Properties &properties,
//        VariesList *variesList) {
//    variesList->count = properties.size();
//    variesList->varies = new Varies[properties.size()];
//    for (size_t i = 0; i < properties.size(); i++) {
//        ObjectProperty property = properties[i];
//        variesList->varies[i].Label = new char[property.name().size() + 1];
//        sprintf(variesList->varies[i].Label, property.name().c_str());
//        any_ptr a = property();
//        if (a->type() == typeid(long)) {
//            variesList->varies[i].type = LONG;
//            variesList->varies[i].data.AsLong = boost::any_cast<long>(*a);
//        } else if (a->type() == typeid(double)) {
//            variesList->varies[i].type = DOUBLE;
//            variesList->varies[i].data.AsDouble = boost::any_cast<double>(*a);
//        } else if (a->type() == typeid(bool)) {
//            variesList->varies[i].type = BOOL;
//            variesList->varies[i].data.AsBool = (Boolean) boost::any_cast<bool>(*a);
//        } else if (a->type() == typeid(std::string)) {
//            variesList->varies[i].type = CHARP;
//            const char *c = boost::any_cast<std::string>(*a).c_str();
//            variesList->varies[i].data.AsCharP = new char[strlen(c) + 1];
//            sprintf(variesList->varies[i].data.AsCharP, c);
//        } else {
//            freeVariesList(variesList);
//            throw Exception("propertiesToVaries: unrecognized type");
//        }
//    }
//}
//
//const char *variesToString(const Varies *v) {
//    static std::string ret;
//    std::ostringstream s;
//    if (v->type == LONG)
//        s << v->data.AsLong;
//    else if (v->type == DOUBLE)
//        s << v->data.AsDouble;
//    else if (v->type == BOOL)
//        s << v->data.AsBool;
//    else if (v->type == CHARP)
//        s << v->data.AsCharP;
//    else
//        throw Exception("variesToString: unrecognized type");
//    ret = s.str();
//    return ret.c_str();
//}
//
//void freeVariesList(VariesList *vl) {
//    for (int i = 0; i < vl->count; i++) {
//        if (vl->varies[i].type == CHARP && vl->varies[i].data.AsCharP)
//            delete [] vl->varies[i].data.AsCharP;
//        if (vl->varies[i].Label)
//            delete [] vl->varies[i].Label;
//    }
//    delete [] vl->varies;
//}
//
//boost::any variesToBoostAny(const Varies &v) {
//    if (v.type == LONG)
//        return boost::any(v.data.AsLong);
//    else if (v.type == DOUBLE)
//        return boost::any(v.data.AsDouble);
//    else if (v.type == BOOL)
//        return boost::any(static_cast < bool > (v.data.AsBool == TRUE));
//    else if (v.type == CHARP)
//        return boost::any(v.data.AsCharP);
//    else
//        throw Exception("variesToBoostAny: unrecognized type");
//}

