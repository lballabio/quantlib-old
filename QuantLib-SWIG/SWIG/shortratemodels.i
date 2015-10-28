
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2007, 2009 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier
 Copyright (C) 2007 Luis Cota
 Copyright (C) 2015 Gouthaman Balaraman

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

#ifndef quantlib_short_rate_models_i
#define quantlib_short_rate_models_i

%include calibrationhelpers.i
%include grid.i

// the base class for models
%{
using QuantLib::ShortRateModel;
%}

%ignore ShortRateModel;
class ShortRateModel {
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
        const std::vector<Real>& weights = std::vector<Real>(),
        const std::vector<bool> & fixParameters = std::vector<bool>());
};

%template(ShortRateModel) boost::shared_ptr<ShortRateModel>;
IsObservable(boost::shared_ptr<ShortRateModel>);

%template(ShortRateModelHandle) Handle<ShortRateModel>;
IsObservable(Handle<ShortRateModel>);
%template(RelinkableShortRateModelHandle)
RelinkableHandle<ShortRateModel>;

// actual models

%{
    using QuantLib::Vasicek;
    using QuantLib::HullWhite;
    using QuantLib::BlackKarasinski;
    using QuantLib::G2;
    typedef boost::shared_ptr<ShortRateModel> VasicekPtr;
    typedef boost::shared_ptr<ShortRateModel> HullWhitePtr;
    typedef boost::shared_ptr<ShortRateModel> BlackKarasinskiPtr;
    typedef boost::shared_ptr<ShortRateModel> G2Ptr;
%}

%rename(Vasicek) VasicekPtr;
class VasicekPtr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        VasicekPtr(Rate r0 = 0.05,
                   Real a = 0.1,
                   Real b = 0.05,
                   Real sigma = 0.01,
                   Real lambda = 0.0) {
            return new VasicekPtr(new Vasicek(r0, a, b, sigma, lambda));
        }
        DiscountFactor discount(Time t) const {
            return boost::dynamic_pointer_cast<Vasicek>(*self)->discount(t);
        }
    }
};


%rename(HullWhite) HullWhitePtr;
class HullWhitePtr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        HullWhitePtr(const Handle<YieldTermStructure>& termStructure,
                     Real a = 0.1, Real sigma = 0.01) {
            return new HullWhitePtr(
                new HullWhite(termStructure, a, sigma));
        }
        DiscountFactor discount(Time t) const {
            return boost::dynamic_pointer_cast<HullWhite>(*self)->discount(t);
        }
    }
};

%rename(BlackKarasinski) BlackKarasinskiPtr;
class BlackKarasinskiPtr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        BlackKarasinskiPtr(const Handle<YieldTermStructure>& termStructure,
                           Real a = 0.1, Real sigma = 0.1) {
            return new BlackKarasinskiPtr(
                new BlackKarasinski(termStructure, a, sigma));
        }
    }
};

%rename(G2) G2Ptr;
class G2Ptr : public boost::shared_ptr<ShortRateModel> {
  public:
    %extend {
        G2Ptr(const Handle<YieldTermStructure>& termStructure,
              Real a = 0.1, Real sigma = 0.01, Real b = 0.1,
              Real eta = 0.01, Real rho = -0.75) {
            return new G2Ptr(new G2(termStructure, a, sigma, b, eta, rho));
        }
        DiscountFactor discount(Time t) const {
            return boost::dynamic_pointer_cast<G2>(*self)->discount(t);
        }
    }
};


// pricing engines for calibration helpers
%{
using QuantLib::JamshidianSwaptionEngine;
using QuantLib::TreeSwaptionEngine;
using QuantLib::AnalyticCapFloorEngine;
using QuantLib::TreeCapFloorEngine;
using QuantLib::G2SwaptionEngine;
typedef boost::shared_ptr<PricingEngine> JamshidianSwaptionEnginePtr;
typedef boost::shared_ptr<PricingEngine> TreeSwaptionEnginePtr;
typedef boost::shared_ptr<PricingEngine> AnalyticCapFloorEnginePtr;
typedef boost::shared_ptr<PricingEngine> TreeCapFloorEnginePtr;
typedef boost::shared_ptr<PricingEngine> G2SwaptionEnginePtr;
%}

%rename(JamshidianSwaptionEngine) JamshidianSwaptionEnginePtr;
class JamshidianSwaptionEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        JamshidianSwaptionEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            using QuantLib::OneFactorAffineModel;
            boost::shared_ptr<OneFactorAffineModel> m =
                 boost::dynamic_pointer_cast<OneFactorAffineModel>(model);
            QL_REQUIRE(model, "affine model required");
            return new JamshidianSwaptionEnginePtr(
                               new JamshidianSwaptionEngine(m,termStructure));
        }
    }
};

%rename(TreeSwaptionEngine) TreeSwaptionEnginePtr;
class TreeSwaptionEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        TreeSwaptionEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         Size timeSteps,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            return new TreeSwaptionEnginePtr(
                       new TreeSwaptionEngine(model,timeSteps,termStructure));
        }
        TreeSwaptionEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         const TimeGrid& grid,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            return new TreeSwaptionEnginePtr(
                            new TreeSwaptionEngine(model,grid,termStructure));
        }
        TreeSwaptionEnginePtr(
                         const Handle<ShortRateModel>& model,
                         Size timeSteps,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            return new TreeSwaptionEnginePtr(
                       new TreeSwaptionEngine(model,timeSteps,termStructure));
        }
    }
};

%rename(AnalyticCapFloorEngine) AnalyticCapFloorEnginePtr;
class AnalyticCapFloorEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        AnalyticCapFloorEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            using QuantLib::OneFactorAffineModel;
            boost::shared_ptr<OneFactorAffineModel> m =
                 boost::dynamic_pointer_cast<OneFactorAffineModel>(model);
            QL_REQUIRE(model, "affine model required");
            return new AnalyticCapFloorEnginePtr(
                                 new AnalyticCapFloorEngine(m,termStructure));
        }
    }
};

%rename(TreeCapFloorEngine) TreeCapFloorEnginePtr;
class TreeCapFloorEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        TreeCapFloorEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         Size timeSteps,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            return new TreeCapFloorEnginePtr(
                       new TreeCapFloorEngine(model,timeSteps,termStructure));
        }
        TreeCapFloorEnginePtr(
                         const boost::shared_ptr<ShortRateModel>& model,
                         const TimeGrid& grid,
                         const Handle<YieldTermStructure>& termStructure =
                                                Handle<YieldTermStructure>()) {
            return new TreeCapFloorEnginePtr(
                            new TreeCapFloorEngine(model,grid,termStructure));
        }
    }
};

%rename(G2SwaptionEngine) G2SwaptionEnginePtr;
class G2SwaptionEnginePtr : public boost::shared_ptr<PricingEngine> {
  public:
    %extend {
        G2SwaptionEnginePtr(const boost::shared_ptr<ShortRateModel>& model,
                            Real range, Size intervals) {
            boost::shared_ptr<G2> g2 =
                boost::dynamic_pointer_cast<G2>(model);
            return new G2SwaptionEnginePtr(
                                    new G2SwaptionEngine(g2,range,intervals));
        }
    }
};


#endif
