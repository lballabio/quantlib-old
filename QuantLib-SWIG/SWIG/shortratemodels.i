
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

#ifndef quantlib_short_rate_models_i
#define quantlib_short_rate_models_i

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
typedef Handle<CalibrationHelper> SwaptionHelperHandle;
typedef Handle<CalibrationHelper> CapHelperHandle;
%}

// calibration helpers
%ignore CalibrationHelper;
class CalibrationHelper {
    #if defined(SWIGRUBY)
    %rename("pricingEngine=")      setPricingEngine;
    #elif defined(SWIGMZSCHEME) || defined(SWIGGUILE)
    %rename("pricing-engine-set!") setPricingEngine;
    #endif
  public:
    void setPricingEngine(const Handle<PricingEngine>& engine);
};
%template(CalibrationHelper) Handle<CalibrationHelper>;

%rename(SwaptionHelper) SwaptionHelperHandle;
class SwaptionHelperHandle : public Handle<CalibrationHelper> {
  public:
    %extend {
        SwaptionHelperHandle(
                const Period& maturity, const Period& length,
                const RelinkableHandle<Quote>& volatility,
                const XiborHandle& index,
                const RelinkableHandle<TermStructure>& termStructure) {
            Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
            return new SwaptionHelperHandle(
                new SwaptionHelper(maturity,length,volatility,
                                   libor,termStructure));
        }
    }
};

%rename(CapHelper) CapHelperHandle;
class CapHelperHandle : public Handle<CalibrationHelper> {
  public:
    %extend {
        CapHelperHandle(
                const Period& length,
                const RelinkableHandle<Quote>& volatility,
                const XiborHandle& index,
                const RelinkableHandle<TermStructure>& termStructure) {
            Handle<Xibor> libor = boost::dynamic_pointer_cast<Xibor>(index);
            return new CapHelperHandle(
                new CapHelper(length,volatility,libor,termStructure));
        }
    }
};


// allow use of CalibrationHelper vectors
namespace std {
    %template(CalibrationHelperVector) vector<Handle<CalibrationHelper> >;
}


// the base class for models
%{
using QuantLib::ShortRateModel;
%}

%ignore ShortRateModel;
class ShortRateModel {
    #if defined(SWIGMZSCHEME) || defined(SWIGGUILE) || defined(SWIGRUBY)
    %rename("calibrate!") calibrate;
    #endif
  public:
    Array params() const;
    void calibrate(
        const std::vector<Handle<CalibrationHelper> >& instruments,
        OptimizationMethod& method);
};

%template(ShortRateModel) Handle<ShortRateModel>;
IsObservable(Handle<ShortRateModel>);

// actual models

%{
using QuantLib::HullWhite;
using QuantLib::BlackKarasinski;
typedef Handle<ShortRateModel> HullWhiteHandle;
typedef Handle<ShortRateModel> BlackKarasinskiHandle;
%}

%rename(HullWhite) HullWhiteHandle;
class HullWhiteHandle : public Handle<ShortRateModel> {
  public:
    %extend {
        HullWhiteHandle(
                const RelinkableHandle<TermStructure>& termStructure, 
                double a = 0.1, double sigma = 0.01) {
	        return new HullWhiteHandle(
	            new HullWhite(termStructure, a, sigma));
        }
    }
};

%rename(BlackKarasinski) BlackKarasinskiHandle;
class BlackKarasinskiHandle : public Handle<ShortRateModel> {
  public:
    %extend {
        BlackKarasinskiHandle(
                const RelinkableHandle<TermStructure>& termStructure, 
                double a = 0.1, double sigma = 0.1) {
	        return new BlackKarasinskiHandle(
	            new BlackKarasinski(termStructure, a, sigma));
        }
    }
};


// pricing engines for calibration helpers
%{
using QuantLib::JamshidianSwaption;
using QuantLib::TreeSwaption;
using QuantLib::TreeCapFloor;
typedef Handle<PricingEngine> JamshidianSwaptionHandle;
typedef Handle<PricingEngine> TreeSwaptionHandle;
typedef Handle<PricingEngine> TreeCapFloorHandle;
%}

%rename(JamshidianSwaption) JamshidianSwaptionHandle;
class JamshidianSwaptionHandle : public Handle<PricingEngine> {
  public:
    %extend {
        JamshidianSwaptionHandle(const Handle<ShortRateModel>& model) {
            using QuantLib::OneFactorAffineModel;
            Handle<OneFactorAffineModel> m = 
                 boost::dynamic_pointer_cast<OneFactorAffineModel>(model);
            QL_REQUIRE(!IsNull(model),
                       "JamshidianSwaption: affine model required");
            return new JamshidianSwaptionHandle(new JamshidianSwaption(m));
        }
    }
};

%rename(TreeSwaption) TreeSwaptionHandle;
class TreeSwaptionHandle : public Handle<PricingEngine> {
  public:
    %extend {
        TreeSwaptionHandle(const Handle<ShortRateModel>& model,
                           Size timeSteps) {
            return new TreeSwaptionHandle(
                new TreeSwaption(model,timeSteps));
        }
    }
};

%rename(TreeCapFloor) TreeCapFloorHandle;
class TreeCapFloorHandle : public Handle<PricingEngine> {
  public:
    %extend {
        TreeCapFloorHandle(const Handle<ShortRateModel>& model,
                           Size timeSteps) {
            return new TreeCapFloorHandle(
                new TreeCapFloor(model,timeSteps));
        }
    }
};


#endif
