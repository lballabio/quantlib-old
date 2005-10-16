
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

#ifndef qla_utilities_h
#define qla_utilities_h

#undef ql_VERSION

const char *qlVersion();

const char *ohVersion();

const char *ohSetLogfile(
    const char *logFileName);

void ohConsole(
    const long console);

void ohLogMessage(
    const char *fmt,
    ...);

void ohLogObject(
    const char *handle);

int qlListRegisteredEnums(
    char **result);

int qlListEnum(
    char *enumId,
    char **result);

int qlListRegisteredTypes(
    char **result);

int qlListType(
    char *enumId,
    char **result);

#endif

