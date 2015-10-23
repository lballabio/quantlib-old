
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl
 Copyright (C) 2015 Matthias Groncki

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

%{
using QuantLib::CapFloorTermVolSurface;
typedef boost::shared_ptr<CapFloorTermVolatilityStructure> CapFloorTermVolSurfacePtr;
%}

%rename(CapFloorTermVolSurface) CapFloorTermVolSurfacePtr;
class CapFloorTermVolSurfacePtr : public boost::shared_ptr<CapFloorTermVolatilityStructure> {
public:
    %extend{
        CapFloorTermVolSurfacePtr(Natural settlementDays,
                               const Calendar& calendar,
                               BusinessDayConvention bdc,
                               const std::vector<Period>& optionTenors,
                               const std::vector<Rate>& strikes,
                               const std::vector<std::vector<Handle<Quote> > >& quotes,
                               const DayCounter& dc = QuantLib::Actual365Fixed()){
            return new CapFloorTermVolSurfacePtr(new CapFloorTermVolSurface(settlementDays, 
            calendar,
            bdc,
            optionTenors,
            strikes,
            quotes,
            dc));
        }
        CapFloorTermVolSurfacePtr(const Date& settlementDate,
                               const Calendar& calendar,
                               BusinessDayConvention bdc,
                               const std::vector<Period>& optionTenors,
                               const std::vector<Rate>& strikes,
                               const std::vector<std::vector<Handle<Quote> > >& quotes,
                               const DayCounter& dc = QuantLib::Actual365Fixed()){
            return new CapFloorTermVolSurfacePtr(new CapFloorTermVolSurface(settlementDate, 
            calendar,
            bdc,
            optionTenors,
            strikes,
            quotes,
            dc));
        }
        CapFloorTermVolSurfacePtr(const Date& settlementDate,
                               const Calendar& calendar,
                               BusinessDayConvention bdc,
                               const std::vector<Period>& optionTenors,
                               const std::vector<Rate>& strikes,
                               const Matrix& volatilities,
                               const DayCounter& dc = QuantLib::Actual365Fixed()){
            return new CapFloorTermVolSurfacePtr(new CapFloorTermVolSurface(settlementDate, 
            calendar,
            bdc,
            optionTenors,
            strikes,
            volatilities,
            dc));
        }
        CapFloorTermVolSurfacePtr(Natural settlementDays,
                               const Calendar& calendar,
                               BusinessDayConvention bdc,
                               const std::vector<Period>& optionTenors,
                               const std::vector<Rate>& strikes,
                               const Matrix& volatilities,
                               const DayCounter& dc = QuantLib::Actual365Fixed()){ 
            return new CapFloorTermVolSurfacePtr(new CapFloorTermVolSurface(settlementDays, 
            calendar,
            bdc,
            optionTenors,
            strikes,
            volatilities,
            dc));
        }
    }
};

%{
using QuantLib::StrippedOptionletBase;
using QuantLib::VolatilityType;
%}

%ignore StrippedOptionletBase;
class StrippedOptionletBase {
public:
    const std::vector<Rate>& optionletStrikes(Size i);
    const std::vector<Volatility>& optionletVolatilities(Size i);
    const std::vector<Date>& optionletFixingDates();
    const std::vector<Time>& optionletFixingTimes();
    Size optionletMaturities();
    const std::vector<Rate>& atmOptionletRates();
    DayCounter dayCounter();
    Calendar calendar();
    Natural settlementDays();
    BusinessDayConvention businessDayConvention();
};

%template(StrippedOptionletBase) boost::shared_ptr<StrippedOptionletBase>;

%{
using QuantLib::OptionletStripper1;
typedef boost::shared_ptr<StrippedOptionletBase> OptionletStripper1Ptr;
%}

%rename(OptionletStripper1) OptionletStripper1Ptr;
class OptionletStripper1Ptr : public boost::shared_ptr<StrippedOptionletBase> {
public:
    %extend{
        OptionletStripper1Ptr(const CapFloorTermVolSurfacePtr & parVolSurface,
                              const IborIndexPtr &index,
                              Rate switchStrikes = Null<Rate>(),
                              Real accuracy = 1.0e-6, Natural maxIter = 100,
                              const Handle<YieldTermStructure> &discount =
                              Handle<YieldTermStructure>(),
                              VolatilityType type = ShiftedLognormal,
                              Real displacement = 0.0,
                              bool dontThrow = false){
            boost::shared_ptr<CapFloorTermVolSurface> surface = 
                boost::dynamic_pointer_cast<CapFloorTermVolSurface>(parVolSurface);
            boost::shared_ptr<IborIndex> idx =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new OptionletStripper1Ptr(new OptionletStripper1(surface,
                                                                    idx,
                                                                    switchStrikes,
                                                                    accuracy,
                                                                    maxIter,
                                                                    discount,
                                                                    type,
                                                                    displacement,
                                                                    dontThrow));
        }
        const Matrix& capFloorPrices() const {
            return boost::dynamic_pointer_cast<OptionletStripper1>(*self)
                ->capFloorPrices();
        }
        const Matrix& capFloorVolatilities() const {
            return boost::dynamic_pointer_cast<OptionletStripper1>(*self)
                ->capFloorVolatilities();
        }
        const Matrix& optionletPrices() const{
            return boost::dynamic_pointer_cast<OptionletStripper1>(*self)
                ->optionletPrices();
        }
        Rate switchStrike() const{
            return boost::dynamic_pointer_cast<OptionletStripper1>(*self)
                ->switchStrike();
        }
    }
};

%{
using QuantLib::StrippedOptionletAdapter;
typedef boost::shared_ptr<OptionletVolatilityStructure>
   StrippedOptionletAdapterPtr;
%}

%rename(StrippedOptionletAdapter) StrippedOptionletAdapterPtr;
class StrippedOptionletAdapterPtr
    : public boost::shared_ptr<OptionletVolatilityStructure> {
  public:
    %extend {
        StrippedOptionletAdapterPtr(const boost::shared_ptr<StrippedOptionletBase> & stripper){
            return new StrippedOptionletAdapterPtr(
                new StrippedOptionletAdapter(stripper));
        }
    }
};
 
#endif
