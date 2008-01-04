/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Ferdinando Ametrano

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

#include <qlo/capletvolstructure.hpp>

#include <ql/termstructures/volatility/optionlet/constantoptionletvol.hpp>
#include <ql/termstructures/volatility/optionlet/spreadedoptionletvol.hpp>
#include <ql/termstructures/volatility/capfloor/capfloortermvolcurve.hpp>
#include <ql/termstructures/volatility/capfloor/capfloortermvolsurface.hpp>
#include <ql/termstructures/volatility/optionlet/strippedoptionletadapter.hpp>
#include <ql/termstructures/volatility/optionlet/optionletstripper1.hpp>
#include <ql/termstructures/volatility/optionlet/optionletstripper2.hpp>
#include <ql/termstructures/volatility/optionlet/strippedoptionlet.hpp>

namespace QuantLibAddin {

    ConstantOptionletVol::ConstantOptionletVol(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::Quote>& volatility,
        const QuantLib::Calendar& cal,
        const QuantLib::DayCounter& dayCounter,
        bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        QuantLib::Natural settlDays = 0;  // FIXME
        QuantLib::BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ConstantOptionletVol(settlDays,
                                           volatility,
                                           cal,
                                           bdc,
                                           dayCounter));
    }

    StrippedOptionletAdapter::StrippedOptionletAdapter(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::StrippedOptionletBase>& strippedOptionlet,
        bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::StrippedOptionletAdapter(strippedOptionlet));
    }

    SpreadedOptionletVol::SpreadedOptionletVol(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>& baseVol,
        const QuantLib::Handle<QuantLib::Quote>& spread,
        bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SpreadedOptionletVol(baseVol,
                                           spread));
    }

    CapFloorTermVolCurve::CapFloorTermVolCurve(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionTenors,
          const std::vector<QuantLib::Handle<QuantLib::Quote> >& vols,
          const QuantLib::DayCounter& dayCounter,
          bool permanent)
    : CapFloorTermVolatilityStructure(properties, permanent)
    {
        QuantLib::BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolCurve(settlementDays,
                                           calendar,
                                           optionTenors,
                                           vols,
                                           bdc,
                                           dayCounter));
    }

    CapFloorTermVolSurface::CapFloorTermVolSurface(
          const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
          QuantLib::Natural settlementDays,
          const QuantLib::Calendar& calendar,
          const std::vector<QuantLib::Period>& optionLengths,
          const std::vector<QuantLib::Rate>& strikes,
          const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& vols,
          const QuantLib::DayCounter& dc,
          bool permanent)
    : CapFloorTermVolatilityStructure(properties, permanent)
    {
        QuantLib::BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolSurface(settlementDays,
                                             calendar,
                                             optionLengths,
                                             strikes,
                                             vols,
                                             bdc,
                                             dc));
    }

    StrippedOptionlet::StrippedOptionlet(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& referenceDate,
            const QuantLib::Calendar& calendar,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention businessDayConvention,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Period>& optionletTenors,
            const std::vector<QuantLib::Rate>& strikes,
            const std::vector<std::vector<QuantLib::Handle<QuantLib::Quote> > >& optionletVolQuotes,
            const QuantLib::DayCounter& dc,
            bool permanent)
    : StrippedOptionletBase(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::StrippedOptionlet>(new
            QuantLib::StrippedOptionlet(referenceDate,
                                        settlementDays,
                                        index,
                                        optionletTenors,
                                        strikes,
                                        optionletVolQuotes,
                                        calendar,
                                        businessDayConvention,
                                        dc));
    }

    StrippedOptionletBase::StrippedOptionletBase(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            bool permanent)
    : ObjectHandler::LibraryObject<QuantLib::StrippedOptionletBase>(properties, permanent)
    {
    }

    OptionletStripper1::OptionletStripper1(
                    const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    const boost::shared_ptr<QuantLib::CapFloorTermVolSurface>& surface,
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    QuantLib::Rate switchStrike,
                    QuantLib::Real accuracy,
                    bool permanent)
    : OptionletStripper(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OptionletStripper1>(new
            QuantLib::OptionletStripper1(surface,
                                         index,
                                         switchStrike,
                                         accuracy));
    }

    OptionletStripper2::OptionletStripper2(
                    const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    const boost::shared_ptr<QuantLib::OptionletStripper1>& optionletStripper1,
                    const QuantLib::Handle<QuantLib::CapFloorTermVolCurve>& atmCapFloorTermVolCurve,
                    bool permanent)
    : OptionletStripper(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OptionletStripper2>(new
            QuantLib::OptionletStripper2(optionletStripper1,
                                         atmCapFloorTermVolCurve));
    }

}
