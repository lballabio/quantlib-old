
/*
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// $Id$

#ifndef quantlib_old_volatility_i
#define quantlib_old_volatility_i

%include common.i
%include date.i
%include calendars.i
%include daycounters.i
%include vectors.i
%include linearalgebra.i


// eventually the classes exported here will be redesigned and deprecated


// swaption volatilities

%{
using QuantLib::SwaptionVolatilityStructure;
%}

%ignore SwaptionVolatilityStructure;
class SwaptionVolatilityStructure {
  public:
	double volatility(const Date& exercise, const Period& length, 
                      Rate strike);
	double volatility(Time exercise, double length, Rate strike);
};

%template(SwaptionVolatilityStructure) Handle<SwaptionVolatilityStructure>;
IsObservable(Handle<SwaptionVolatilityStructure>);

%template(SwaptionVolatilityStructureHandle) 
    RelinkableHandle<SwaptionVolatilityStructure>;
IsObservable(RelinkableHandle<SwaptionVolatilityStructure>);

%{
using QuantLib::Volatilities::SwaptionVolatilityMatrix;
typedef Handle<SwaptionVolatilityStructure> SwaptionVolatilityMatrixHandle;
%}

%rename(SwaptionVolatilityMatrix) SwaptionVolatilityMatrixHandle;
class SwaptionVolatilityMatrixHandle 
: public Handle<SwaptionVolatilityStructure> {
  public:
    %extend {
        SwaptionVolatilityMatrixHandle(const Date& today, 
                                       const std::vector<Date>& dates, 
                                       const std::vector<Period>& lengths, 
                                       const Matrix& vols, 
                                       const DayCounter& dayCounter) {
            return new SwaptionVolatilityMatrixHandle(
                new SwaptionVolatilityMatrix(today,dates,lengths,
                                             vols,dayCounter));
        }
    }
};



// cap/floor volatilities

%{
using QuantLib::CapFlatVolatilityStructure;
%}

%ignore CapFlatVolatilityStructure;
class CapFlatVolatilityStructure {
  public:
	double volatility(const Date& end, Rate strike);
	double volatility(Time end, Rate strike);
};

%template(CapFlatVolatilityStructure) Handle<CapFlatVolatilityStructure>;
IsObservable(Handle<CapFlatVolatilityStructure>);

%template(CapFlatVolatilityStructureHandle)
    RelinkableHandle<CapFlatVolatilityStructure>;
IsObservable(RelinkableHandle<CapFlatVolatilityStructure>);

%{
using QuantLib::Volatilities::CapFlatVolatilityVector;
typedef Handle<CapFlatVolatilityStructure> CapFlatVolatilityVectorHandle;
%}

%rename(CapFlatVolatilityVector) CapFlatVolatilityVectorHandle;
class CapFlatVolatilityVectorHandle 
: public Handle<CapFlatVolatilityStructure> {
  public:
    %extend {
        CapFlatVolatilityVectorHandle(const Date& today, 
                                      const Calendar& calendar,
                                      int settlementDays, 
                                      const std::vector<Period>& lengths,
                                      const std::vector<double>& vols, 
                                      const DayCounter& dayCounter) {
            return new CapFlatVolatilityVectorHandle(
                new CapFlatVolatilityVector(today,calendar,settlementDays,
                                            lengths,vols,dayCounter));
        }
    }
};


#endif
