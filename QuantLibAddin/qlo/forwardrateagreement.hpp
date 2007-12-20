/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Katiuscia Manzoni
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

#ifndef qla_forwardrateagreement_hpp
#define qla_forwardrateagreement_hpp

#include <qlo/baseinstruments.hpp>
#include <ql/instruments/forwardrateagreement.hpp>

namespace QuantLib {
    class Date;
    class IborIndex;
}

namespace QuantLibAddin {

    class ForwardRateAgreement : public Instrument {
    public:
        ForwardRateAgreement(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& valueDate,
            const QuantLib::Date& maturityDate,
            QuantLib::Position::Type type,
            QuantLib::Rate strike,
            double notional,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            bool permanent);
    };

}

#endif

