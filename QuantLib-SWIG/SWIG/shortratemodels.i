
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
%include grid.i
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
typedef boost::shared_ptr<CalibrationHelper> SwaptionHelperPtr;
typedef boost::shared_ptr<CalibrationHelper> CapHelperPtr;
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
    void setPricingEngine(const boost::shared_ptr<PricingEngine>& engine);
    Real marketValue() const;
    Real modelValue() const;
    Volatility impliedVolatility(Real targetValue,
                                 Real accuracy, Size maxEvaluations,
                                 Volatility minVol, Volatility maxVol) const;
    Real blackPrice(Volatility volatility) const;
};
%template(CalibrationHelper) boost::shared_ptr<CalibrationHelper>;

%rename(SwaptionHelper) SwaptionHelperPtr;
class SwaptionHelperPtr : public boost::shared_ptr<CalibrationHelper> {
  public:
    %extend {
        SwaptionHelperPtr(
                const Period& maturity, const Period& length,
                const RelinkableHandle<Quote>& volatility,
                const XiborPtr& index,
                const RelinkableHandle<TermStructure>& termStructure) {
            boost::shared_ptr<Xibor> libor = 
                boost::dynamic_pointer_cast<Xibor>(index);
            return new SwaptionHelperPtr(
                new SwaptionHelper(maturity,length,volatility,
                                   libor,termStructure));
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
        CapHelperPtr(
                const Period& length,
                const RelinkableHandle<Quote>& volatility,
                const XiborPtr& index,
                const RelinkableHandle<TermStructure>& termStructure) {
            boost::shared_ptr<Xibor> libor = 
                boost::dynamic_pointer_cast<Xibor>(index);
            return new CapHelperPtr(
                new CapHelper(length,volatility,libor,termStructure));
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


// allow use of CalibrationHelper vectors
namespace std {
    %template(CalibrationHelperVector) 
        vector<boost::shared_ptr<CalibrationHelper> >;
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
        const std::vector<boost::shared_ptr<CalibrationHelper> >&,
        OptimizationMethod&);
};

%template(ShortRateModel) boost::shared_ptr<ShortRateModel>;
IsObservable(boost::shared_ptr<ShortRateModel>);

// actual models

%{
using QuantLib::HullWhite;
using QuantLib::BlackKarasinski;
typedef boost::shared_ptr<ShortRateModel> HullWhitePtr;
typedef boost::shared_ptr<ShortRateModel> BlackKarasinskiPtr;
%}

%rename(HullWhite) HullWhitePtr;
class HullWhitePtr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        HullWhitePtr(
                const RelinkableHandle<TermStructure>& termStructure, 
                Real a = 0.1, Real sigma = 0.01) {
	        return new HullWhitePtr(
	            new HullWhite(termStructure, a, sigma));
        }
    }
};

%rename(BlackKarasinski) BlackKarasinskiPtr;
class BlackKarasinskiPtr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        BlackKarasinskiPtr(
                const RelinkableHandle<TermStructure>& termStructure, 
                Real a = 0.1, Real sigma = 0.1) {
	        return new BlackKarasinskiPtr(
	            new BlackKarasinski(termStructure, a, sigma));
        }
    }
};


// pricing engines for calibration helpers
%{
using QuantLib::JamshidianSwaption;
using QuantLib::TreeSwaption;
using QuantLib::TreeCapFloor;
typedef boost::shared_ptr<PricingEngine> JamshidianSwaptionPtr;
typedef boost::shared_ptr<PricingEngine> TreeSwaptionPtr;
typedef boost::shared_ptr<PricingEngine> TreeCapFloorPtr;
%}

%rename(JamshidianSwaption) JamshidianSwaptionPtr;
class JamshidianSwaptionPtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        JamshidianSwaptionPtr(const boost::shared_ptr<ShortRateModel>& model) {
            using QuantLib::OneFactorAffineModel;
            boost::shared_ptr<OneFactorAffineModel> m = 
                 boost::dynamic_pointer_cast<OneFactorAffineModel>(model);
            QL_REQUIRE(model, "affine model required");
            return new JamshidianSwaptionPtr(new JamshidianSwaption(m));
        }
    }
};

%rename(TreeSwaption) TreeSwaptionPtr;
class TreeSwaptionPtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        TreeSwaptionPtr(const boost::shared_ptr<ShortRateModel>& model,
                        Size timeSteps) {
            return new TreeSwaptionPtr(
                new TreeSwaption(model,timeSteps));
        }
        TreeSwaptionPtr(const boost::shared_ptr<ShortRateModel>& model,
                        const TimeGrid& grid) {
            return new TreeSwaptionPtr(
                new TreeSwaption(model,grid));
        }
    }
};

%rename(TreeCapFloor) TreeCapFloorPtr;
class TreeCapFloorPtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        TreeCapFloorPtr(const boost::shared_ptr<ShortRateModel>& model,
                        Size timeSteps) {
            return new TreeCapFloorPtr(
                new TreeCapFloor(model,timeSteps));
        }
        TreeCapFloorPtr(const boost::shared_ptr<ShortRateModel>& model,
                        const TimeGrid& grid) {
            return new TreeCapFloorPtr(
                new TreeCapFloor(model,grid));
        }
    }
};


#endif
