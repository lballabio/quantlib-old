
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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


// eventually the classes exported here will be redesigned or deprecated

// cap/floor volatilities

%{
using QuantLib::CapVolatilityStructure;
%}

%ignore CapVolatilityStructure;
class CapVolatilityStructure {
  public:
    Volatility volatility(const Date& end, Rate strike);
    Volatility volatility(Time end, Rate strike);
};

%template(CapVolatilityStructure) boost::shared_ptr<CapVolatilityStructure>;
IsObservable(boost::shared_ptr<CapVolatilityStructure>);

%template(CapVolatilityStructureHandle) Handle<CapVolatilityStructure>;
IsObservable(Handle<CapVolatilityStructure>);
%template(RelinkableCapVolatilityStructureHandle)
RelinkableHandle<CapVolatilityStructure>;

%{
using QuantLib::CapVolatilityVector;
typedef boost::shared_ptr<CapVolatilityStructure> CapVolatilityVectorPtr;
%}

%rename(CapVolatilityVector) CapVolatilityVectorPtr;
class CapVolatilityVectorPtr
: public boost::shared_ptr<CapVolatilityStructure> {
  public:
    %extend {
       CapVolatilityVectorPtr(const Date& referenceDate,
                              const Calendar& calendar,
                              const std::vector<Period>& lengths,
                              const std::vector<Volatility>& vols,
                              const DayCounter& dayCounter) {
            return new CapVolatilityVectorPtr(
                new CapVolatilityVector(referenceDate,calendar,
                                        lengths,vols,dayCounter));
        }
        CapVolatilityVectorPtr(Integer settlementDays,
                               const Calendar& calendar,
                               const std::vector<Period>& lengths,
                               const std::vector<Volatility>& vols,
                               const DayCounter& dayCounter) {
            return new CapVolatilityVectorPtr(
                new CapVolatilityVector(settlementDays,calendar,
                                        lengths,vols,dayCounter));
        }
    }
};


#endif
