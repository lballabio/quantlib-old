
/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

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

/*! \file qlxl.hpp
    \brief QuantLib Excel add-in
*/

#ifndef qlxl_h
#define qlxl_h

#include <xlw/xlw.h>

#include <ql/qldefines.hpp>


//! version hexadecimal number
#define QLXL_HEX_VERSION 0x000308f0

//! version string
#ifdef QL_DEBUG
    #define QLXL_VERSION "0.3.8-debug"
#else
    #define QLXL_VERSION "0.3.8"
#endif

#if QL_HEX_VERSION < 0x000308a0
    #error using an old version of QuantLib, please update
#endif

#if XLW_HEX_VERSION < 0x010203a0
    #error using an old version of xlw, please update
#endif

// If called from the function wizard returns immediately
#define WIZARD_NO_CALC if (XlfExcel::Instance().IsCalledByFuncWiz()) \
            return XlfOper("Wizard no-calc");

#define QLEXCEL_END \
} catch (QuantLib::Error& e) { \
	return XlfOper(e.what()); \
} catch (XlfException&) { \
	return 0; \
} catch (...) { \
	return XlfOper::Error(xlerrValue); \
}

#endif
