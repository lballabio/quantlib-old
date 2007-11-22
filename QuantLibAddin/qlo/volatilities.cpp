
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/volatilities.hpp>

#include <ql/termstructures/volatility/equityfx/blackconstantvol.hpp>
#include <ql/termstructures/volatility/equityfx/blackvariancesurface.hpp>
#include <ql/experimental/abcdatmvolcurve.hpp>
#include <ql/experimental/sabrvolsurface.hpp>

namespace QuantLibAddin {

    BlackConstantVol::BlackConstantVol(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            const QuantLib::Calendar& cal,
            QuantLib::Volatility volatility,
            const QuantLib::DayCounter& dayCounter,
            bool permanent) : BlackVolTermStructure(properties, permanent)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::BlackVolTermStructure>(new
                QuantLib::BlackConstantVol(settlementDate,
                                           cal,
                                           volatility,
                                           dayCounter));
    }

    BlackVarianceSurface::BlackVarianceSurface(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            const QuantLib::Calendar& cal,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Rate>& strikes,
            const QuantLib::Matrix& vols,
            const QuantLib::DayCounter& dayCounter,
            bool permanent) : BlackVolTermStructure(properties, permanent)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::BlackVolTermStructure>(new
                QuantLib::BlackVarianceSurface(settlementDate,
                                               cal,
                                               dates,
                                               strikes,
                                               vols,
                                               dayCounter));
    }

    AbcdAtmVolCurve::AbcdAtmVolCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& cal,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::Handle<QuantLib::Quote> > & vols,
            const std::vector<bool> inclusionInInterpolationFlag,
            QuantLib::BusinessDayConvention bdc,
            const QuantLib::DayCounter& dc,
            bool permanent)
    : BlackAtmVolCurve(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::AbcdAtmVolCurve>(new
            QuantLib::AbcdAtmVolCurve(settlementDays,
                                      cal,
                                      optionTenors,
                                      vols,
                                      inclusionInInterpolationFlag,
                                      bdc,
                                      dc));
    }

    SabrVolSurface::SabrVolSurface(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::InterestRateIndex>& index,
            const QuantLib::Handle<QuantLib::BlackAtmVolCurve>& blackAtmCurve,
            const std::vector<QuantLib::Period>& optionTenors,
            const std::vector<QuantLib::Spread>& atmRateSpreads,
            const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& volSpreads,
            bool permanent)
    : InterestRateVolSurface(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SabrVolSurface>(new
            QuantLib::SabrVolSurface(index,
                                      blackAtmCurve,
                                      optionTenors,
                                      atmRateSpreads,
                                      volSpreads));
    }

    std::string SabrVolSurface::atmCurve() {
        return boost::any_cast<std::string>(propertyValue("BLACKATMVOLCURVE"));
    }

    SabrSmileSectionImpl::SabrSmileSectionImpl(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::SabrVolSurface>& sabrVol,
            const QuantLib::Time& time,
            bool permanent) : SmileSection(properties, permanent) {
             libraryObject_ = sabrVol->smileSectionImpl(time);
    }
}
