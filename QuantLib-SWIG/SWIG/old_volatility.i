
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef quantlib_old_volatility_i
#define quantlib_old_volatility_i

%include common.i
%include date.i
%include calendars.i
%include daycounters.i
%include vectors.i
%include linearalgebra.i
%include interpolation.i


// eventually the classes exported here will be redesigned or deprecated

// cap/floor volatilities

%{
using QuantLib::CapFloorTermVolatilityStructure;
%}

%ignore CapFloorTermVolatilityStructure;
class CapFloorTermVolatilityStructure : public Extrapolator {
  public:
    Volatility volatility(const Period& length, Rate strike,
                          bool extrapolate = false);
    Volatility volatility(const Date& end, Rate strike,
                          bool extrapolate = false);
    Volatility volatility(Time end, Rate strike,
                          bool extrapolate = false);
};

%template(CapFloorTermVolatilityStructure)
boost::shared_ptr<CapFloorTermVolatilityStructure>;
IsObservable(boost::shared_ptr<CapFloorTermVolatilityStructure>);

%template(CapFloorTermVolatilityStructureHandle)
Handle<CapFloorTermVolatilityStructure>;
IsObservable(Handle<CapFloorTermVolatilityStructure>);

%template(RelinkableCapFloorTermVolatilityStructureHandle)
RelinkableHandle<CapFloorTermVolatilityStructure>;

%{
using QuantLib::CapFloorTermVolCurve;
typedef boost::shared_ptr<CapFloorTermVolatilityStructure> CapFloorTermVolCurvePtr;
%}

%rename(CapFloorTermVolCurve) CapFloorTermVolCurvePtr;
class CapFloorTermVolCurvePtr
: public boost::shared_ptr<CapFloorTermVolatilityStructure> {
  public:
    %extend {
       CapFloorTermVolCurvePtr(const Date& referenceDate,
                               const Calendar& calendar,
                               BusinessDayConvention bdc,
                               const std::vector<Period>& lengths,
                               const std::vector<Volatility>& vols,
                               const DayCounter& dc =
                                           QuantLib::Actual365Fixed()) {
            return new CapFloorTermVolCurvePtr(
                new CapFloorTermVolCurve(referenceDate,calendar,bdc,
                                         lengths,vols,dc));
        }
        CapFloorTermVolCurvePtr(Natural settlementDays,
                                const Calendar& calendar,
                                BusinessDayConvention bdc,
                                const std::vector<Period>& lengths,
                                const std::vector<Volatility>& vols,
                                const DayCounter& dc =
                                            QuantLib::Actual365Fixed()) {
            return new CapFloorTermVolCurvePtr(
                new CapFloorTermVolCurve(settlementDays,calendar,bdc,
                                         lengths,vols,dc));
        }
    }
};


#endif
