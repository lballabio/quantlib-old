
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

#ifndef xl_utilities_hpp
#define xl_utilities_hpp

#include <windows.h>
#include <string>
#include <Addins/Excel/xlcall.h>

#define DLLEXPORT extern "C" __declspec(dllexport)

void setXLOPERString(XLOPER &xStr,
                     const char *s);
void anyToXLOPER(const ObjHandler::any_ptr &any,
                 XLOPER &xOp);
std::string getCaller();
void setValues(LPXLOPER xArray,
               ObjHandler::Properties properties,
               const std::string &handle);
std::vector <long> longXLOPERToVector(LPXLOPER xVec);

#endif
