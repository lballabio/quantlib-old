/*
 Copyright (C) 2008, 2009 StatPro Italia srl

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

#ifndef quantlib_default_probability_structures_i
#define quantlib_default_probability_structures_i

%include common.i
%include types.i
%include date.i
%include calendars.i
%include daycounters.i
%include scheduler.i
%include observer.i
%include marketelements.i
%include interpolation.i

%{
using QuantLib::DefaultProbabilityTermStructure;
%}

%ignore DefaultProbabilityTermStructure;
class DefaultProbabilityTermStructure : public Extrapolator {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("day-counter")     dayCounter;
    %rename("reference-date")  referenceDate;
    %rename("max-date")        maxDate;
    %rename("max-time")        maxTime;
    %rename("default-probability")  defaultProbability;
    %rename("survival-probability") survivalProbability;
    %rename("hazard-rate")          hazardRate;
    %rename("default-density")      defaultDensity;
    #endif
  public:
    DayCounter dayCounter() const;
    Calendar calendar() const;
    Date referenceDate() const;
    Date maxDate() const;
    Time maxTime() const;

    Probability defaultProbability(const Date&, bool extrapolate = false);
    Probability defaultProbability(Time, bool extrapolate = false);
    Probability defaultProbability(const Date&, const Date&,
                                   bool extrapolate = false);
    Probability defaultProbability(Time, Time, bool extrapolate = false);

    Probability survivalProbability(const Date&, bool extrapolate = false);
    Probability survivalProbability(Time, bool extrapolate = false);

    Real defaultDensity(const Date&, bool extrapolate = false);
    Real defaultDensity(Time, bool extrapolate = false);

    Real hazardRate(const Date&, bool extrapolate = false);
    Real hazardRate(Time, bool extrapolate = false);
};

%template(DefaultProbabilityTermStructure)
boost::shared_ptr<DefaultProbabilityTermStructure>;
IsObservable(boost::shared_ptr<DefaultProbabilityTermStructure>);

%template(DefaultProbabilityTermStructureHandle)
Handle<DefaultProbabilityTermStructure>;
IsObservable(Handle<DefaultProbabilityTermStructure>);
%template(RelinkableDefaultProbabilityTermStructureHandle)
RelinkableHandle<DefaultProbabilityTermStructure>;


// concrete curves


// flat forward curve

%{
using QuantLib::FlatHazardRate;
typedef boost::shared_ptr<DefaultProbabilityTermStructure> FlatHazardRatePtr;
%}

%rename(FlatHazardRate) FlatHazardRatePtr;
class FlatHazardRatePtr
    : public boost::shared_ptr<DefaultProbabilityTermStructure> {
  public:
    %extend {
        FlatHazardRatePtr(Integer settlementDays,
                          const Calendar& calendar,
                          const Handle<Quote>& hazardRate,
                          const DayCounter& dayCounter) {
            return new FlatHazardRatePtr(
                           new FlatHazardRate(settlementDays,calendar,
                                              hazardRate,dayCounter));
        }
        FlatHazardRatePtr(const Date& todaysDate,
                          const Handle<Quote>& hazardRate,
                          const DayCounter& dayCounter) {
            return new FlatHazardRatePtr(
                        new FlatHazardRate(todaysDate,hazardRate,dayCounter));
        }
    }
};



%{
using QuantLib::InterpolatedHazardRateCurve;
%}

%define export_hazard_curve(Name,Interpolator)

%{
typedef boost::shared_ptr<DefaultProbabilityTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<DefaultProbabilityTermStructure> {
  public:
    %extend {
        Name##Ptr(const std::vector<Date>& dates,
                  const std::vector<Real>& hazardRates,
                  const DayCounter& dayCounter,
                  const Calendar& calendar = Calendar(),
                  const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new InterpolatedHazardRateCurve<Interpolator>(dates,hazardRates,
                                                              dayCounter,
                                                              calendar,i));
        }
        const std::vector<Date>& dates() {
            typedef InterpolatedHazardRateCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Real>& hazardRates() {
            typedef InterpolatedHazardRateCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->hazardRates();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef InterpolatedHazardRateCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef

export_hazard_curve(HazardRateCurve,BackwardFlat);

// add interpolations as you wish, e.g.,
// export_hazard_curve(LinearHazardRateCurve,Linear);



%{
using QuantLib::InterpolatedDefaultDensityCurve;
%}

%define export_default_density_curve(Name,Interpolator)

%{
typedef boost::shared_ptr<DefaultProbabilityTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<DefaultProbabilityTermStructure> {
  public:
    %extend {
        Name##Ptr(const std::vector<Date>& dates,
                  const std::vector<Real>& densities,
                  const DayCounter& dayCounter,
                  const Calendar& calendar = Calendar(),
                  const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new InterpolatedDefaultDensityCurve<Interpolator>(dates,
                                                                  densities,
                                                                  dayCounter,
                                                                  calendar,i));
        }
        const std::vector<Date>& dates() {
            typedef InterpolatedDefaultDensityCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Real>& defaultDensities() {
            typedef InterpolatedDefaultDensityCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->defaultDensities();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef InterpolatedDefaultDensityCurve<Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef

export_default_density_curve(DefaultDensityCurve,Linear);

// add interpolations as you wish, e.g.,
// export_default_density_curve(CubicDefaultDensityCurve,Cubic);



%{
using QuantLib::DefaultProbabilityHelper;
using QuantLib::SpreadCdsHelper;
typedef boost::shared_ptr<DefaultProbabilityHelper> SpreadCdsHelperPtr;
using QuantLib::UpfrontCdsHelper;
typedef boost::shared_ptr<DefaultProbabilityHelper> UpfrontCdsHelperPtr;
%}

// rate helpers for curve bootstrapping
%template(DefaultProbabilityHelper) boost::shared_ptr<DefaultProbabilityHelper>;

#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<DefaultProbabilityHelper> )
#endif
namespace std {
    %template(DefaultProbabilityHelperVector)
    vector<boost::shared_ptr<DefaultProbabilityHelper> >;
}


%rename(SpreadCdsHelper) SpreadCdsHelperPtr;
class SpreadCdsHelperPtr : public boost::shared_ptr<DefaultProbabilityHelper> {
  public:
    %extend {
        SpreadCdsHelperPtr(
                const Handle<Quote>& spread,
                const Period& tenor,
                Integer settlementDays,
                const Calendar& calendar,
                Frequency frequency,
                BusinessDayConvention convention,
                DateGeneration::Rule rule,
                const DayCounter& dayCounter,
                Real recoveryRate,
                const Handle<YieldTermStructure>& discountCurve,
                bool settlesAccrual = true,
                bool paysAtDefaultTime = true) {
            return new SpreadCdsHelperPtr(
                new SpreadCdsHelper(spread,tenor,settlementDays,calendar,
                                    frequency,convention,rule,dayCounter,
                                    recoveryRate,discountCurve,
                                    settlesAccrual,paysAtDefaultTime));
        }
        SpreadCdsHelperPtr(
                Rate spread,
                const Period& tenor,
                Integer settlementDays,
                const Calendar& calendar,
                Frequency frequency,
                BusinessDayConvention convention,
                DateGeneration::Rule rule,
                const DayCounter& dayCounter,
                Real recoveryRate,
                const Handle<YieldTermStructure>& discountCurve,
                bool settlesAccrual = true,
                bool paysAtDefaultTime = true) {
            return new SpreadCdsHelperPtr(
                new SpreadCdsHelper(spread,tenor,settlementDays,calendar,
                                    frequency,convention,rule,dayCounter,
                                    recoveryRate,discountCurve,
                                    settlesAccrual,paysAtDefaultTime));
        }
    }
};


%rename(UpfrontCdsHelper) UpfrontCdsHelperPtr;
class UpfrontCdsHelperPtr : public boost::shared_ptr<DefaultProbabilityHelper> {
  public:
    %extend {
        UpfrontCdsHelperPtr(
                const Handle<Quote>& upfront,
                Rate spread,
                const Period& tenor,
                Integer settlementDays,
                const Calendar& calendar,
                Frequency frequency,
                BusinessDayConvention convention,
                DateGeneration::Rule rule,
                const DayCounter& dayCounter,
                Real recoveryRate,
                const Handle<YieldTermStructure>& discountCurve,
                bool settlesAccrual = true,
                bool paysAtDefaultTime = true) {
            return new UpfrontCdsHelperPtr(
                new UpfrontCdsHelper(upfront,spread,tenor,
                                     settlementDays,calendar,
                                     frequency,convention,rule,dayCounter,
                                     recoveryRate,discountCurve,
                                     settlesAccrual,paysAtDefaultTime));
        }
        UpfrontCdsHelperPtr(
                Rate upfront,
                Rate spread,
                const Period& tenor,
                Integer settlementDays,
                const Calendar& calendar,
                Frequency frequency,
                BusinessDayConvention convention,
                DateGeneration::Rule rule,
                const DayCounter& dayCounter,
                Real recoveryRate,
                const Handle<YieldTermStructure>& discountCurve,
                bool settlesAccrual = true,
                bool paysAtDefaultTime = true) {
            return new UpfrontCdsHelperPtr(
                new UpfrontCdsHelper(upfront,spread,tenor,
                                     settlementDays,calendar,
                                     frequency,convention,rule,dayCounter,
                                     recoveryRate,discountCurve,
                                     settlesAccrual,paysAtDefaultTime));
        }
    }
};



// bootstrap traits

%{
using QuantLib::HazardRate;
using QuantLib::DefaultDensity;
%}

struct HazardRate {};
struct DefaultDensity {};

// curve

%{
using QuantLib::PiecewiseDefaultCurve;
%}

%define export_piecewise_default_curve(Name,Base,Interpolator)

%{
typedef boost::shared_ptr<DefaultProbabilityTermStructure> Name##Ptr;
%}

%rename(Name) Name##Ptr;
class Name##Ptr : public boost::shared_ptr<DefaultProbabilityTermStructure> {
  public:
    %extend {
        Name##Ptr(
              const Date& referenceDate,
              const std::vector<boost::shared_ptr<DefaultProbabilityHelper> >&
                                                                  instruments,
              const DayCounter& dayCounter,
              Real accuracy = 1.0e-12,
              const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseDefaultCurve<Base,Interpolator>(
                                                 referenceDate,instruments,
                                                 dayCounter, accuracy, i));
        }
        Name##Ptr(
              Integer settlementDays, const Calendar& calendar,
              const std::vector<boost::shared_ptr<DefaultProbabilityHelper> >&
                                                                  instruments,
              const DayCounter& dayCounter,
              Real accuracy = 1.0e-12,
              const Interpolator& i = Interpolator()) {
            return new Name##Ptr(
                new PiecewiseDefaultCurve<Base,Interpolator>(
                                        settlementDays, calendar, instruments,
                                        dayCounter, accuracy, i));
        }
        const std::vector<Date>& dates() {
            typedef PiecewiseDefaultCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->dates();
        }
        const std::vector<Time>& times() {
            typedef PiecewiseDefaultCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->times();
        }
        #if !defined(SWIGR) && !defined(SWIGGUILE) && !defined(SWIGMZSCHEME)
        std::vector<std::pair<Date,Real> > nodes() {
            typedef PiecewiseDefaultCurve<Base,Interpolator> Name;
            return boost::dynamic_pointer_cast<Name>(*self)->nodes();
        }
        #endif
    }
};

%enddef


export_piecewise_default_curve(PiecewiseFlatHazardRate,HazardRate,BackwardFlat);

// combine traits as you wish, e.g.,
// export_piecewise_default_curve(PiecewiseLinearDensity,DefaultDensity,Linear);


#endif
