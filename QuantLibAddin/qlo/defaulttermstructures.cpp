/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2009 Ferdinando Ametrano

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif

#include <qlo/defaulttermstructures.hpp>

#include <ql/termstructures/credit/flathazardrate.hpp>
#include <ql/termstructures/credit/interpolatedhazardratecurve.hpp>

#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/experimental/credit/basecorrelationstructure.hpp>

#include <ql/math/solver1d.hpp>
#include <ql/math/solvers1d/brent.hpp>

namespace QuantLibAddin {

    FlatHazardRate::FlatHazardRate(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                             QuantLib::Natural nDays,
                             const QuantLib::Calendar& calendar,
                             const QuantLib::Handle<QuantLib::Quote>& hazardRate,
                             const QuantLib::DayCounter& dayCounter,
                             bool permanent)
    : HazardRateStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::FlatHazardRate(nDays, calendar, hazardRate, dayCounter));
    }


    BaseCorrelationTermStructure::BaseCorrelationTermStructure(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::string& interpolType,
        QuantLib::Natural nDays,
        const QuantLib::Calendar& calendar,
        QuantLib::BusinessDayConvention bdc,
        const std::vector<QuantLib::Period>& tenors,
        const std::vector<QuantLib::Real>& lossLevel,
        const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& correls,
        const QuantLib::DayCounter& dayCounter,
        bool permanent)
    : interpolType_(interpolType), CorrelationTermStructure(properties, permanent)
    {
        // select 2D interpolator:
        //  Another option is to write one class per interpolator.
        if(interpolType == std::string("BILIN")) {
            libraryObject_ = boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<QuantLib::BilinearInterpolation> >(new
                QuantLib::BaseCorrelationTermStructure<QuantLib::BilinearInterpolation>(nDays, calendar, bdc, tenors, lossLevel, correls, dayCounter));
        //}else if(interpolType == std::string("")) {
        }else if(interpolType == std::string("BICUBIC")) {
            libraryObject_ = boost::shared_ptr<QuantLib::BaseCorrelationTermStructure<QuantLib::BicubicSpline> >(new
                QuantLib::BaseCorrelationTermStructure<QuantLib::BicubicSpline>(nDays, calendar, bdc, tenors, lossLevel, correls, dayCounter));
        }else{
            QL_FAIL("Can't determine base correlation surface interpolator.");
        }
    }

    QuantLib::Real BaseCorrelationTermStructure::correlation(const QuantLib::Date& d, QuantLib::Real lossLevel) {
        if(interpolType_ == std::string("BILIN")) {
            return boost::dynamic_pointer_cast<QuantLib::BaseCorrelationTermStructure<QuantLib::BilinearInterpolation> >
                (libraryObject_)->correlation(d, lossLevel);
        }else if(interpolType_ == std::string("BICUBIC")) {
            return boost::dynamic_pointer_cast<QuantLib::BaseCorrelationTermStructure<QuantLib::BicubicSpline> >
                (libraryObject_)->correlation(d, lossLevel);
        }else{
            QL_FAIL("unknown 2D interpolator");
        }
    }


    namespace {
        class HRObjectiveF : public std::unary_function<QuantLib::Real, QuantLib::Real> {
            const QuantLib::Real targetValue_;
            const std::vector<QuantLib::Date> d_;
            const QuantLib::DayCounter dc_;
        public:
            HRObjectiveF(const QuantLib::Real Prob,
                         const std::vector<QuantLib::Date>& t,
                         const QuantLib::DayCounter& dc)
            : targetValue_(Prob), d_(t), dc_(dc) { /*QL_REQUIRE(d_.size() == 2, "Needs two dates exactly.");*/ }
            QuantLib::Real operator()(const QuantLib::Real x)const {
                std::vector<QuantLib::Real> hrs(2, x);
                QuantLib::InterpolatedHazardRateCurve<QuantLib::BackwardFlat> tstCurve(d_, hrs, dc_);
                return tstCurve.defaultProbability(d_[0], d_[1], true) - targetValue_;
            }
        };
    }

    QuantLib::Real probabilityToHazardRate(
        QuantLib::Probability pdef, 
        const QuantLib::Date& d,
        const QuantLib::DayCounter& dc
        ) 
    {
        std::vector<QuantLib::Date> tenors;
        tenors.push_back(QuantLib::Settings::instance().evaluationDate());
        tenors.push_back(d);
            
        HRObjectiveF f(pdef, tenors, dc);
        QuantLib::Brent solver;
        solver.setMaxEvaluations(100);
        QuantLib::Real guess  = 0.5;
        QuantLib::Real minVal = QL_EPSILON;
        QuantLib::Real maxVal = 100.;
        QuantLib::Real accuracy = 1.0e-6;
        return solver.solve(f, accuracy, guess, minVal, maxVal);
    }


}
