
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef ohxl_export_hpp
#define ohxl_export_hpp

#pragma comment (linker, "/export:_ohVersion") 
#pragma comment (linker, "/export:_ohObjectCount") 
#pragma comment (linker, "/export:_ohHandleList") 
#pragma comment (linker, "/export:_ohFieldNames") 
#pragma comment (linker, "/export:_ohFieldValue") 
#pragma comment (linker, "/export:_ohDeleteObject") 
#pragma comment (linker, "/export:_ohDeleteAllObjects") 
#pragma comment (linker, "/export:_ohDependsOn") 
#pragma comment (linker, "/export:_ohGetGcEnabled") 
#pragma comment (linker, "/export:_ohSetGcEnabled") 
#pragma comment (linker, "/export:_ohCallGC") 
#pragma comment (linker, "/export:_ohSetLogFile") 
#pragma comment (linker, "/export:_ohSetLogLevel") 
#pragma comment (linker, "/export:_ohLogMessage") 
#pragma comment (linker, "/export:_ohLogObject") 
#pragma comment (linker, "/export:_ohLogAllObjects") 

#endif
