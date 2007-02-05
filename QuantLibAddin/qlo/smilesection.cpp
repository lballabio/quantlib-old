/*
 Copyright (C) 2006 François du Vignaud

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/smilesection.hpp>
#include <ql/Math/sabrinterpolation.hpp>
#include <ql/Volatilities/smilesection.hpp>


namespace QuantLibAddin {

    FlatSmileSection::FlatSmileSection(
                                const QuantLib::Date& optionDate,
                                QuantLib::Volatility v,
                                const QuantLib::DayCounter& dc,
                                const QuantLib::Date& refDate)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SmileSection>(new
            QuantLib::FlatSmileSection(optionDate, v, dc, refDate));
    }

    InterpolatedSmileSection::InterpolatedSmileSection(
            const QuantLib::Date& optionDate,
            const std::vector<QuantLib::Rate>& s,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            const QuantLib::DayCounter& dc)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SmileSection>(new
            QuantLib::InterpolatedSmileSection<>(optionDate, s,
                                                 stdDevs, dc));
    }

    SabrSmileSection::SabrSmileSection(
            const QuantLib::Time expiry,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            QuantLib::Real forward, 
            QuantLib::Real alpha,
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method) {

        boost::shared_ptr<QuantLib::OptimizationMethod> method_ = method;
        if (!method) {
            QuantLib::Array guess(4); 
            method_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
                            QuantLib::Simplex(1e-6, guess));
        }

        boost::shared_ptr<QuantLib::EndCriteria> endCriteria_ = endCriteria;
        if (!endCriteria) {
            QuantLib::EndCriteria endCriteria(120000, 1e-12);
        }

        QuantLib::SABR sabrInterpolationFactory(expiry, forward, alpha, beta,
            nu, rho, isAlphaFixed, isBetaFixed, isNuFixed, 
            isRhoFixed, vegaWeighted, endCriteria, method);

        QuantLib::InterpolatedSmileSection<QuantLib::SABR>* 
            genericInterpolatedSmileSection = new
                QuantLib::InterpolatedSmileSection<QuantLib::SABR>(
                    expiry, strikes, stdDevs, sabrInterpolationFactory);

        libraryObject_ = boost::shared_ptr<
            QuantLib::InterpolatedSmileSection<QuantLib::SABR> >(
                genericInterpolatedSmileSection);
    }

    SabrInterpolatedSmileSection::SabrInterpolatedSmileSection(
            const QuantLib::Date& optionDate,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevHandles,
            QuantLib::Handle<QuantLib::Quote> forward, 
            QuantLib::Real alpha, 
            QuantLib::Real beta,
            QuantLib::Real nu,
            QuantLib::Real rho,
            bool isAlphaFixed,
            bool isBetaFixed,
            bool isNuFixed,
            bool isRhoFixed,
            bool vegaWeighted,
            const boost::shared_ptr<QuantLib::EndCriteria> endCriteria,
            const boost::shared_ptr<QuantLib::OptimizationMethod> method,
            const QuantLib::DayCounter& dc)
    {
       libraryObject_ = 
           boost::shared_ptr<QuantLib::SabrInterpolatedSmileSection>(
            new QuantLib::SabrInterpolatedSmileSection(optionDate,
                                                   strikes,
                                                   stdDevHandles,
                                                   forward,
                                                   alpha,
                                                   beta,
                                                   nu,
                                                   rho,
                                                   isAlphaFixed,
                                                   isBetaFixed,
                                                   isNuFixed,
                                                   isRhoFixed,
                                                   vegaWeighted,
                                                   endCriteria,
                                                   method,
                                                   dc));
    }
}
