
/*
 Copyright (C) 2004 Eric Ehlers

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

#include <ObjectHandler/objhandler.hpp>
extern "C" {
#include <Addins/C/varies.h>
}

using namespace ObjHandler;

void propertiesToVaries(const Properties &properties, 
        VariesList *variesList) {
    variesList->count = properties.size();
    variesList->varies = new Varies[properties.size()];
    for (size_t i = 0; i < properties.size(); i++) {
        ObjectProperty property = properties[i];
        variesList->varies[i].Label = new char[property.name().size() + 1];
        sprintf(variesList->varies[i].Label, property.name().c_str());
        any_ptr a = property();
        if (a->type() == typeid(int)) {
            variesList->varies[i].type = INT;
            variesList->varies[i].AsInt = boost::any_cast<int>(*a);
        } else if (a->type() == typeid(long)) {
            variesList->varies[i].type = LONG;
            variesList->varies[i].AsLong = boost::any_cast<long>(*a);
        } else if (a->type() == typeid(double)) {
            variesList->varies[i].type = DOUBLE;
            variesList->varies[i].AsDouble = boost::any_cast<double>(*a);
        } else if (a->type() == typeid(std::string)) {
            variesList->varies[i].type = CHARP;
            const char *c = boost::any_cast<std::string>(*a).c_str();
            variesList->varies[i].AsCharP = new char[strlen(c) + 1];
            sprintf(variesList->varies[i].AsCharP, c);
        } else {
            freeVariesList(variesList);
            throw Exception("propertiesToVaries: unrecognized type");
        }
    }
}

char c[100];    // FIXME
const char *variesToString(const Varies *v) {
    if (v->type == INT)
        sprintf(c, "%d", v->AsInt);
    else if (v->type == LONG)
        sprintf(c, "%d", v->AsLong);
    else if (v->type == DOUBLE)
        sprintf(c, "%f", v->AsDouble);
    else if (v->type == CHARP)
        sprintf(c, "%s", v->AsCharP);
    else
        throw Exception("variesToString: unrecognized type");
    return c;
}

void freeVariesList(VariesList *vl) {
    for (int i = 0; i < vl->count; i++) {
        if (vl->varies[i].type == CHARP && vl->varies[i].AsCharP)
            delete [] vl->varies[i].AsCharP;
        if (vl->varies[i].Label)
            delete [] vl->varies[i].Label;
    }
    delete [] vl->varies;
}
