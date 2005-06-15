
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

#include <Addins/C/qladdin.h>
#include <stdio.h>
#include <malloc.h>

void printVariesList(const char *s, const VariesList vo) {
    int i;
    QL_LOG_MESSAGE(s);
    for (i=0; i<vo.count; i++)
        QL_LOG_MESSAGE("field = %s, value = %s", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));
}

int main() {
    VariesList vfcb;                        // fixed coupon bond
    // inputs
    long issueDate              = 38424;    // issue date (1 January 2000)
    long datedDate              = 38424;    // dated date (15 July 2001)
    long maturityDate           = 42076;    // maturity date (1 January 2010)
    long settlementDays         = 3;        // settlement days
    double coupons[] = { 0.04 };            // coupons
    double yields[] = { 0.05 };             // yields
    double redemption          = 0.05;      // redemption
    char *frequencyID          = "Annual";  // frequency
    char *dayCounterID         = "Thirty360";// day count
    char *businessDayConvention= "Following";// business day convention
    char *calendarID           = "Germany"; // calendar

    QL_LOGFILE("quantlib.log");
    QL_CONSOLE(1);
    QL_LOG_MESSAGE("begin options test");

    if (QL_FIXED_COUPON_BOND(
            "bond1", 
            issueDate, 
            datedDate,
            maturityDate, 
            settlementDays, 
            1,                      // #/coupons
            coupons, 
            1,                      // #/yields
            yields, 
            redemption, 
            frequencyID, 
            dayCounterID, 
            businessDayConvention,
            calendarID, 
            1,                      // startFromEnd
            1,                      // longFinal
            "",                     // discCurveId
            &vfcb) != SUCCESS) {
        QL_LOG_MESSAGE("Error on call to QL_FIXED_COUPON_BOND");
        goto fail;
    }

    printVariesList("QL_FIXED_COUPON_BOND", vfcb);

    freeVariesList(&vfcb);

    QL_LOG_MESSAGE("end instruments test");

    return 0;

fail:

    return 1;
}

