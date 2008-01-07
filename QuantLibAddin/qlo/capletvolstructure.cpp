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

using std::vector;
using QuantLib::Handle;
using QuantLib::Quote;
using BusinessDayConvention;
using boost::shared_ptr;
using Objecthandler::ValueObject;
using LibraryObject;

namespace QuantLibAddin {

    ConstantOptionletVolatility::ConstantOptionletVolatility(
                                    const shared_ptr<ValueObject>& properties,
                                    QuantLib::Natural settlementDays,
                                    const Handle<Quote>& volatility,
                                    const QuantLib::DayCounter& dayCounter,
                                    const QuantLib::Calendar& cal,
                                    BusinessDayConvention bdc,
                                    bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        QuantLib::Natural settlDays = 0;  // FIXME
        BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ConstantOptionletVolatility(settlDays,
                                                  volatility,
                                                  dayCounter,
                                                  cal,
                                                  bdc));
    }

    SpreadedOptionletVolatility::SpreadedOptionletVolatility(
                const shared_ptr<ValueObject>& properties,
                const Handle<QuantLib::OptionletVolatilityStructure>& baseVol,
                const Handle<Quote>& spread,
                bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::SpreadedOptionletVolatility(baseVol,
                                                  spread));
    }

    StrippedOptionletAdapter::StrippedOptionletAdapter(
                        const shared_ptr<ValueObject>& properties,
                        const shared_ptr<QuantLib::StrippedOptionletBase>& v,
                        bool permanent)
    : OptionletVolatilityStructure(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::StrippedOptionletAdapter(v));
    }

    CapFloorTermVolCurve::CapFloorTermVolCurve(
                                const shared_ptr<ValueObject>& properties,
                                QuantLib::Natural settlementDays,
                                const QuantLib::Calendar& calendar,
                                const vector<QuantLib::Period>& optionTenors,
                                const vector<Handle<Quote> >& vols,
                                const QuantLib::DayCounter& dayCounter,
                                bool permanent)
    : CapFloorTermVolatilityStructure(properties, permanent)
    {
        BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolCurve(settlementDays,
                                           calendar,
                                           optionTenors,
                                           vols,
                                           bdc,
                                           dayCounter));
    }

    CapFloorTermVolSurface::CapFloorTermVolSurface(
                                const shared_ptr<ValueObject>& properties,
                                QuantLib::Natural settlementDays,
                                const QuantLib::Calendar& calendar,
                                const vector<QuantLib::Period>& optionLengths,
                                const vector<QuantLib::Rate>& strikes,
                                const vector<vector<Handle<Quote> > >& vols,
                                const QuantLib::DayCounter& dc,
                                bool permanent)
    : CapFloorTermVolatilityStructure(properties, permanent)
    {
        BusinessDayConvention bdc = QuantLib::Following;  // FIXME
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::CapFloorTermVolSurface(settlementDays,
                                             calendar,
                                             optionLengths,
                                             strikes,
                                             vols,
                                             bdc,
                                             dc));
    }

    StrippedOptionlet::StrippedOptionlet(
                                const shared_ptr<ValueObject>& properties,
                                const QuantLib::Date& referenceDate,
                                const QuantLib::Calendar& calendar,
                                QuantLib::Natural settlementDays,
                                BusinessDayConvention businessDayConvention,
                                const shared_ptr<QuantLib::IborIndex>& index,
                                const vector<QuantLib::Period>& optionletTenors,
                                const vector<QuantLib::Rate>& strikes,
                                const vector<vector<Handle<Quote> > >& vols,
                                const QuantLib::DayCounter& dc,
                                bool permanent)
    : StrippedOptionletBase(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::StrippedOptionlet>(new
            QuantLib::StrippedOptionlet(referenceDate,
                                        settlementDays,
                                        index,
                                        optionletTenors,
                                        strikes,
                                        vols,
                                        calendar,
                                        businessDayConvention,
                                        dc));
    }

    StrippedOptionletBase::StrippedOptionletBase(
                                const shared_ptr<ValueObject>& properties,
                                bool permanent)
    : LibraryObject<QuantLib::StrippedOptionletBase>(properties, permanent) {}

    OptionletStripper1::OptionletStripper1(
                        const shared_ptr<ValueObject>& properties,
                        const shared_ptr<QuantLib::CapFloorTermVolSurface>& s,
                        const shared_ptr<QuantLib::IborIndex>& index,
                        QuantLib::Rate switchStrike,
                        QuantLib::Real accuracy,
                        bool permanent)
    : OptionletStripper(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::OptionletStripper1>(new
            QuantLib::OptionletStripper1(s,
                                         index,
                                         switchStrike,
                                         accuracy));
    }

    OptionletStripper2::OptionletStripper2(
                    const shared_ptr<ValueObject>& properties,
                    const shared_ptr<QuantLib::OptionletStripper1>& o,
                    const Handle<QuantLib::CapFloorTermVolCurve>& atmV,
                    bool permanent)
    : OptionletStripper(properties, permanent)
    {
        libraryObject_ = shared_ptr<QuantLib::OptionletStripper2>(new
            QuantLib::OptionletStripper2(o,
                                         atmV));
    }

}
