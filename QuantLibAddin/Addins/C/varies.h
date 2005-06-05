
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

#ifndef qla_varies_h
#define qla_varies_h

typedef enum { LONG, DOUBLE, BOOL, CHARP } Type;

typedef union {
    long AsLong;
    double AsDouble;
    bool AsBool;
    char* AsCharP;
} DataUnion;

typedef struct {
    DataUnion data;
    Type type;
    char* Label;
} Varies;

typedef struct {
    int count;
    Varies *varies;
} VariesList;

const char *variesToString(const Varies *v);
void freeVariesList(VariesList *vl);

#endif

