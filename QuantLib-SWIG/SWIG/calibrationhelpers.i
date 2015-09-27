/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2007, 2009 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2007 Luis Cota

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

#ifndef quantlib_calibration_helpers_i
#define quantlib_calibration_helpers_i

%include date.i
%include calendars.i
%include daycounters.i
%include cashflows.i
%include marketelements.i
%include termstructures.i
%include optimizers.i
%include options.i
%include linearalgebra.i
%include types.i
%include vectors.i

%{
using QuantLib::CalibrationHelper;
using QuantLib::SwaptionHelper;
using QuantLib::CapHelper;
using QuantLib::HestonModelHelper;
typedef boost::shared_ptr<CalibrationHelper> SwaptionHelperPtr;
typedef boost::shared_ptr<CalibrationHelper> CapHelperPtr;
typedef boost::shared_ptr<CalibrationHelper> HestonModelHelperPtr;
%}

// calibration helpers
#if defined(SWIGJAVA) || defined(SWIGCSHARP)
%rename(_CalibrationHelper) CalibrationHelper;
#else
%ignore CalibrationHelper;
#endif
class CalibrationHelper {
    #if defined(SWIGRUBY)
    %rename("pricingEngine=")      setPricingEngine;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("pricing-engine-set!") setPricingEngine;
    %rename("market-value")        marketValue;
    %rename("model-value")         modelValue;
    %rename("implied-volatility")  impliedVolatility;
    %rename("black-price")         blackPrice;
    #endif
  private:
    CalibrationHelper();
  public:
    enum CalibrationErrorType {
                            RelativePriceError, PriceError, ImpliedVolError };
    void setPricingEngine(const boost::shared_ptr<PricingEngine>& engine);
    Real marketValue() const;
    Real modelValue() const;
	Real calibrationError();
    Volatility impliedVolatility(Real targetValue,
                                 Real accuracy, Size maxEvaluations,
                                 Volatility minVol, Volatility maxVol) const;
    Real blackPrice(Volatility volatility) const;
};
%template(CalibrationHelper) boost::shared_ptr<CalibrationHelper>;
%extend boost::shared_ptr<CalibrationHelper> {
    static const CalibrationHelper::CalibrationErrorType RelativePriceError =
        CalibrationHelper::RelativePriceError;
    static const CalibrationHelper::CalibrationErrorType PriceError =
        CalibrationHelper::PriceError;
    static const CalibrationHelper::CalibrationErrorType ImpliedVolError =
        CalibrationHelper::ImpliedVolError;
}

%rename(SwaptionHelper) SwaptionHelperPtr;
class SwaptionHelperPtr : public boost::shared_ptr<CalibrationHelper> {
  public:
    %extend {
        SwaptionHelperPtr(const Period& maturity, const Period& length,
                          const Handle<Quote>& volatility,
                          const IborIndexPtr& index,
                          const Period& fixedLegTenor,
                          const DayCounter& fixedLegDayCounter,
                          const DayCounter& floatingLegDayCounter,
                          const Handle<YieldTermStructure>& termStructure,
                          CalibrationHelper::CalibrationErrorType errorType
                                    = CalibrationHelper::RelativePriceError) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new SwaptionHelperPtr(
                new SwaptionHelper(maturity,length,volatility,
                                   libor,fixedLegTenor,
                                   fixedLegDayCounter,
                                   floatingLegDayCounter,
                                   termStructure,
                                   errorType));
        }
        std::vector<Time> times() {
            std::list<Time> l;
            (*self)->addTimesTo(l);
            std::vector<Time> v;
            std::copy(l.begin(),l.end(),std::back_inserter(v));
            return v;
        }
    }
};

%rename(CapHelper) CapHelperPtr;
class CapHelperPtr : public boost::shared_ptr<CalibrationHelper> {
  public:
    %extend {
        CapHelperPtr(const Period& length,
                     const Handle<Quote>& volatility,
                     const IborIndexPtr& index,
                     Frequency fixedLegFrequency,
                     const DayCounter& fixedLegDayCounter,
                     bool includeFirstSwaplet,
                     const Handle<YieldTermStructure>& termStructure,
                     CalibrationHelper::CalibrationErrorType errorType
                                    = CalibrationHelper::RelativePriceError) {
            boost::shared_ptr<IborIndex> libor =
                boost::dynamic_pointer_cast<IborIndex>(index);
            return new CapHelperPtr(
                new CapHelper(length,volatility,libor,fixedLegFrequency,
                              fixedLegDayCounter,includeFirstSwaplet,
                              termStructure));
        }
        std::vector<Time> times() {
            std::list<Time> l;
            (*self)->addTimesTo(l);
            std::vector<Time> v;
            std::copy(l.begin(),l.end(),std::back_inserter(v));
            return v;
        }
    }
};

%rename(HestonModelHelper) HestonModelHelperPtr;
class HestonModelHelperPtr : public boost::shared_ptr<CalibrationHelper> {
  public:
	%extend {
		HestonModelHelperPtr(const Period& maturity,
                             const Calendar& calendar,
                             const Real s0,
                             const Real strikePrice,
                             const Handle<Quote>& volatility,
                             const Handle<YieldTermStructure>& riskFreeRate,
                             const Handle<YieldTermStructure>& dividendYield,
                             CalibrationHelper::CalibrationErrorType errorType
								 = CalibrationHelper::RelativePriceError) {
			return new HestonModelHelperPtr(
				new HestonModelHelper(maturity, calendar, s0, strikePrice,
									  volatility, riskFreeRate, dividendYield,
									  errorType)); 
		}
	}
};

// allow use of CalibrationHelper vectors
#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<CalibrationHelper> )
#endif
namespace std {
    %template(CalibrationHelperVector)
        vector<boost::shared_ptr<CalibrationHelper> >;
}

// the base class for calibrated models
%{
using QuantLib::CalibratedModel;
%}

%ignore CalibratedModel;
class CalibratedModel {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) || defined(SWIGRUBY)
    %rename("calibrate!") calibrate;
    #elif defined(SWIGCSHARP)
    %rename("parameters") params;
    #endif
  public:
    Array params() const;
    void calibrate(
        const std::vector<boost::shared_ptr<CalibrationHelper> >&,
        OptimizationMethod&, const EndCriteria &,
        const Constraint& constraint = Constraint(),
        const std::vector<Real>& weights = std::vector<Real>());
};


%template(CalibratedModel) boost::shared_ptr<CalibratedModel>;
IsObservable(boost::shared_ptr<CalibratedModel>);

%template(CalibratedModelHandle) Handle<CalibratedModel>;
IsObservable(Handle<CalibratedModel>);
%template(RelinkableCalibratedModelHandle)
RelinkableHandle<CalibratedModel>;

#endif
