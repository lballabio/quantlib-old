/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2010 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_leg_hpp
#define qla_leg_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>
#include <ql/compounding.hpp>
#include <ql/cashflows/cashflows.hpp>

namespace QuantLib {
    class FloatingRateCouponPricer;
}

namespace QuantLibAddin {
    class FloatingRateCouponPricer;
}

namespace QuantLibAddin {

    class Leg : public ObjectHandler::LibraryObject<QuantLib::Leg> {
      public:
        Leg(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Real>& amounts,
            const std::vector<QuantLib::Date>& dates,
            bool toBeSorted,
            bool permanent);
        void setCouponPricers(const std::vector<boost::shared_ptr<QuantLibAddin::FloatingRateCouponPricer> >& p);
        std::vector<std::vector<ObjectHandler::property_t> > flowAnalysis() const;
      protected:
        OH_LIB_CTOR(Leg, QuantLib::Leg)
    };

    class MultiPhaseLeg : public Leg {
      public:
        MultiPhaseLeg(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                      const std::vector<boost::shared_ptr<Leg> >& legs,
                      bool toBeSorted,
                      bool permanent);
    };

    class InterestRate : public ObjectHandler::LibraryObject<QuantLib::InterestRate> {
      public:
        InterestRate(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                     QuantLib::Rate r,
                     const QuantLib::DayCounter& dc,
                     QuantLib::Compounding comp,
                     QuantLib::Frequency freq,
                     bool permanent);
    };
}

#endif
