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
/*! \file utilities.cpp
    \brief QuantLib Excel utilities

    \fullpath
    qlxl/%utilities.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>

extern "C"
{
    LPXLOPER EXCEL_EXPORT xlQLversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(QL_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(QL_HEX_VERSION));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlXLWversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(XLW_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlXLWhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(XLW_HEX_VERSION));
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLXLversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(QLXL_VERSION);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlQLXLhexversion(void)
    {
        EXCEL_BEGIN;
        return XlfOper(double(QLXL_HEX_VERSION));
        EXCEL_END;
    }

}
