/*
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano

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

/*! \file qlxlfoper.hpp
    \brief XlfOper specialization for QuantLib classes
*/

#ifndef qlxl_qlxlfoper_h
#define qlxl_qlxlfoper_h

#include <qlxl/qlxl.hpp>
#include <ql/Math/matrix.hpp>
#include <ql/date.hpp>
#include <ql/daycounter.hpp>
#include <ql/DayCounters/actual365fixed.hpp>
#include <ql/termstructure.hpp>
#include <ql/option.hpp>
#include <ql/voltermstructure.hpp>
#include <ql/Math/Matrix.hpp>

class QlXlfOper {
public:
    QlXlfOper(const XlfOper& xlfOper);
    QuantLib::DayCounter AsDayCounter() const;
    QuantLib::Calendar AsCalendar() const;
    QuantLib::Date AsDate() const;
    std::vector<QuantLib::Date> AsDateVector() const;
    QuantLib::Matrix AsMatrix() const;
    QuantLib::Option::Type AsOptionType() const;
    QuantLib::Handle<QuantLib::BlackVolTermStructure> AsBlackVolTermStructure(
        const QuantLib::Date& referenceDate,
        int interpolationType,
        const QuantLib::DayCounter& dc = QuantLib::Actual365Fixed()) const;
    QuantLib::Handle<QuantLib::YieldTermStructure> AsTermStructure(
        const QuantLib::Date& referenceDate,
        const QuantLib::DayCounter& dc = QuantLib::Actual365Fixed()) const;
private:
    XlfOper xlfOper_;
};


#endif
