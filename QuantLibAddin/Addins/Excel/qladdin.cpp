
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

#include <windows.h>
#include <Addins/Excel/xlcall.h>
#include <Addins/Excel/framewrk.hpp>
#include <Addins/Excel/funcdef.hpp>

extern "C" __declspec(dllexport) int xlAutoOpen() {
    static XLOPER xDll;
    Excel(xlGetName, &xDll, 0);
    for (int i=0; i<NUM_FUNCS; i++)
        Excel(xlfRegister, 0, NUM_ATTS + 1, &xDll,
            TempStr(func[i][0]),
            TempStr(func[i][1]),
            TempStr(func[i][2]),
            TempStr(func[i][3]),
            TempStr(func[i][4]),
            TempStr(func[i][5]),
            TempStr(func[i][6]),
            TempStr(func[i][7]),
            TempStr(func[i][8]),
            TempStr(func[i][9]),
            TempStr(func[i][10]),
            TempStr(func[i][11]),
            TempStr(func[i][12]),
            TempStr(func[i][13]),
            TempStr(func[i][14]),
            TempStr(func[i][15]),
            TempStr(func[i][16]),
            TempStr(func[i][17]),
            TempStr(func[i][18]),
            TempStr(func[i][19]),
            TempStr(func[i][20]),
            TempStr(func[i][21]),
            TempStr(func[i][22]),
            TempStr(func[i][23]));
    Excel(xlFree, 0, 1, &xDll);
    return 1;
}

extern "C" __declspec(dllexport) void xlAutoFree(LPXLOPER px) {
    if (px->xltype == xltypeMulti && px->val.array.lparray) {
        unsigned short size = px->val.array.rows * px->val.array.columns;
        for (unsigned short i = 0; i < size; i++)
            if (px->val.array.lparray[i].xltype == xltypeStr
            &&  px->val.array.lparray[i].val.str)
                delete [] px->val.array.lparray[i].val.str;
        delete [] px->val.array.lparray;
    }
}
