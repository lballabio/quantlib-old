
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

%template(SwaptionVolatilityStructure) 
    boost::shared_ptr<SwaptionVolatilityStructure>;
IsObservable(boost::shared_ptr<SwaptionVolatilityStructure>);

%template(SwaptionVolatilityStructureHandle) 
    RelinkableHandle<SwaptionVolatilityStructure>;
IsObservable(RelinkableHandle<SwaptionVolatilityStructure>);

%{
using QuantLib::SwaptionVolatilityMatrix;
typedef boost::shared_ptr<SwaptionVolatilityStructure> 
    SwaptionVolatilityMatrixPtr;
%}

%rename(SwaptionVolatilityMatrix) SwaptionVolatilityMatrixPtr;
class SwaptionVolatilityMatrixPtr 
: public boost::shared_ptr<SwaptionVolatilityStructure> {
  public:
    %extend {
        SwaptionVolatilityMatrixPtr(const Date& today, 
                                    const std::vector<Date>& dates, 
                                    const std::vector<Period>& lengths, 
                                    const Matrix& vols, 
                                    const DayCounter& dayCounter) {
            return new SwaptionVolatilityMatrixPtr(
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

%template(CapFlatVolatilityStructure) 
    boost::shared_ptr<CapFlatVolatilityStructure>;
IsObservable(boost::shared_ptr<CapFlatVolatilityStructure>);

%template(CapFlatVolatilityStructureHandle)
    RelinkableHandle<CapFlatVolatilityStructure>;
IsObservable(RelinkableHandle<CapFlatVolatilityStructure>);

%{
using QuantLib::CapFlatVolatilityVector;
typedef boost::shared_ptr<CapFlatVolatilityStructure> 
    CapFlatVolatilityVectorPtr;
%}

%rename(CapFlatVolatilityVector) CapFlatVolatilityVectorPtr;
class CapFlatVolatilityVectorPtr 
: public boost::shared_ptr<CapFlatVolatilityStructure> {
  public:
    %extend {
        CapFlatVolatilityVectorPtr(const Date& today, 
                                   const Calendar& calendar,
                                   int settlementDays, 
                                   const std::vector<Period>& lengths,
                                   const std::vector<double>& vols, 
                                   const DayCounter& dayCounter) {
            return new CapFlatVolatilityVectorPtr(
                new CapFlatVolatilityVector(today,calendar,settlementDays,
                                            lengths,vols,dayCounter));
        }
    }
};


#endif
