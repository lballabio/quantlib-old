/*
 Copyright (C) 2006 François du Vignaud

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/smilesection.hpp>

#include <ql/termstructures/volatility/interpolatedsmilesection.hpp>
#include <ql/termstructures/volatility/sabrinterpolatedsmilesectionNew.hpp>
#include <ql/termstructures/volatility/sabrvolsurface.hpp>
#include <ql/math/interpolations/sabrinterpolation.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/quotes/simplequote.hpp>

namespace QuantLibAddin {

    FlatSmileSection::FlatSmileSection(
                                const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                                const QuantLib::Date& optionDate,
                                QuantLib::Volatility v,
                                const QuantLib::DayCounter& dc,
                                const QuantLib::Date& refDate,
                                QuantLib::Real atmValue,
                                bool permanent) : SmileSection(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SmileSection>(new
            QuantLib::FlatSmileSection(optionDate, v, dc, refDate, atmValue));
    }

    InterpolatedSmileSection::InterpolatedSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& optionDate,
            const std::vector<QuantLib::Rate>& s,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            const QuantLib::Handle<QuantLib::Quote>& atmLevel,
            const QuantLib::DayCounter& dc,
            bool permanent) : SmileSection(properties, permanent)
    {
        std::vector<QuantLib::Handle<QuantLib::Quote> > temp(stdDevs.size());
        for(QuantLib::Size i = 0; i<temp.size(); ++i)
            temp[i] = stdDevs[i];
        libraryObject_ = boost::shared_ptr<QuantLib::SmileSection>(new
            QuantLib::InterpolatedSmileSection<QuantLib::Linear>(
                                    optionDate, s, temp, atmLevel, dc));
    }

    SabrSmileSection::SabrSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Time expiry,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& stdDevs,
            const QuantLib::Handle<QuantLib::Quote>& forward,
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
            bool permanent) : SmileSection(properties, permanent) {

        boost::shared_ptr<QuantLib::OptimizationMethod> method_ = method;
        if (!method)
            method_ = boost::shared_ptr<QuantLib::OptimizationMethod>(new
                            QuantLib::Simplex(0.01));

        boost::shared_ptr<QuantLib::EndCriteria> endCriteria_ = endCriteria;
        if (!endCriteria)
            QuantLib::EndCriteria endCriteria(60000, 100, 1e-8, 1e-8, 1e-8);

        QuantLib::SABR sabr(
                                expiry, forward->value(), alpha, beta,
                                nu, rho, isAlphaFixed, isBetaFixed, isNuFixed, 
                                isRhoFixed, vegaWeighted, endCriteria, method);
        
        std::vector<QuantLib::Handle<QuantLib::Quote> > temp(stdDevs.size());
        for(QuantLib::Size i = 0; i<temp.size(); ++i)
            temp[i] = stdDevs[i];
        QuantLib::InterpolatedSmileSection<QuantLib::SABR>* 
            genericInterpolatedSmileSection = new
                QuantLib::InterpolatedSmileSection<QuantLib::SABR>(
                    expiry, strikes, temp, forward, sabr);

        libraryObject_ = boost::shared_ptr<
            QuantLib::InterpolatedSmileSection<QuantLib::SABR> >(
                genericInterpolatedSmileSection);
    }

    
    SabrInterpolatedSmileSection::SabrInterpolatedSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                           const QuantLib::Date& optionDate,
                           const QuantLib::Rate& forward,
                           const std::vector<QuantLib::Rate>& strikes,
                           bool hasFloatingStrikes,
                           const QuantLib::Volatility& atmVolatility,
                           const std::vector<QuantLib::Volatility>& vols,
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
                           const QuantLib::DayCounter& dc,
                           bool permanent): SmileSection(properties, permanent)
    {
           libraryObject_ = 
           boost::shared_ptr<QuantLib::SabrInterpolatedSmileSection>(
            new QuantLib::SabrInterpolatedSmileSection(optionDate,
                                                   forward,
                                                   strikes,
                                                   hasFloatingStrikes,
                                                   atmVolatility,
                                                   vols,
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

    SabrInterpolatedSmileSection::SabrInterpolatedSmileSection(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& optionDate,
            const QuantLib::Handle<QuantLib::Quote>& forward,
            const std::vector<QuantLib::Rate>& strikes,
            bool hasFloatingStrikes,
            const QuantLib::Handle<QuantLib::Quote>& atmVolatility,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& volHandles,
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
            const QuantLib::DayCounter& dc,
            bool permanent): SmileSection(properties, permanent) {

           std::vector<QuantLib::Handle<QuantLib::Quote> > temp(volHandles.size());
           for(QuantLib::Size i = 0; i<temp.size(); ++i)
                temp[i] = volHandles[i];

           libraryObject_ = 
           boost::shared_ptr<QuantLib::SabrInterpolatedSmileSection>(
            new QuantLib::SabrInterpolatedSmileSection(optionDate,
                                                   forward,
                                                   strikes,
                                                   hasFloatingStrikes,
                                                   atmVolatility,
                                                   temp,
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

    SmileSectionFromSabrVolSurface::SmileSectionFromSabrVolSurface(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::SabrVolSurface>& sabrVol,
            const QuantLib::Time& time,
            bool permanent): SmileSection(properties, permanent) {
           
             libraryObject_ = sabrVol->smileSection(time,true);  
    }
}

