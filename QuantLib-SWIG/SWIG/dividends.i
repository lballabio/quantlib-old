
/*
 Copyright (C) 2006 StatPro Italia srl

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

#ifndef quantlib_dividends_i
#define quantlib_dividends_i

%include cashflows.i

%{
using QuantLib::FixedDividend;
using QuantLib::FractionalDividend;
using QuantLib::DividendSchedule;

typedef boost::shared_ptr<CashFlow> FixedDividendPtr;
typedef boost::shared_ptr<CashFlow> FractionalDividendPtr;
%}

%rename(FixedDividend) FixedDividendPtr;
class FixedDividendPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        FixedDividendPtr(Real amount, const Date& date) {
            return new FixedDividendPtr(new FixedDividend(amount,date));
        }
    }
};

%rename(FractionalDividend) FractionalDividendPtr;
class FractionalDividendPtr : public boost::shared_ptr<CashFlow> {
  public:
    %extend {
        FractionalDividendPtr(Rate rate, const Date& date) {
            return new FractionalDividendPtr(
                                     new FractionalDividend(rate,date));
        }
    }
};


class DividendSchedule {
  public:
    DividendSchedule();
    DividendSchedule(std::vector<boost::shared_ptr<CashFlow> >);
};


#endif
