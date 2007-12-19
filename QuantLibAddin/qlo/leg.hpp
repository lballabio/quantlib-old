
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
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
#include <ql/cashflow.hpp>
#include <ql/compounding.hpp>
#include <ql/cashflows/cashflows.hpp>

namespace QuantLib {
    class FloatingRateCouponPricer;
}

namespace QuantLibAddin {

    class Leg : public ObjectHandler::Object {
      public:
        QuantLib::Rate previousCouponRate(const QuantLib::Date& refDate) const;
        QuantLib::Rate currentCouponRate(const QuantLib::Date& refDate) const;
        QuantLib::Date startDate() const;
        QuantLib::Date maturityDate() const;
        QuantLib::Real npv(const QuantLib::YieldTermStructure&,
                           const QuantLib::Date& settlementDate,
                           const QuantLib::Date& npvDate,
                           QuantLib::Integer exDividendDays) const;
        QuantLib::Real npv(const QuantLib::InterestRate& y,
                           const QuantLib::Date& settlementDate
                           //,
                           //const QuantLib::Date& npvDate,
                           //QuantLib::Integer exDividendDays
                           ) const;
        QuantLib::Real bps(const QuantLib::YieldTermStructure&,
                           const QuantLib::Date& settlementDate,
                           const QuantLib::Date& npvDate,
                           QuantLib::Integer exDividendDays) const;
        QuantLib::Real bps(const QuantLib::InterestRate& y,
                           const QuantLib::Date& settlementDate
                           //,
                           //const QuantLib::Date& npvDate,
                           //QuantLib::Integer exDividendDays
                           ) const;
        QuantLib::Rate atmRate(const QuantLib::YieldTermStructure&,
                               const QuantLib::Date& settlementDate,
                               const QuantLib::Date& npvDate,
                               QuantLib::Integer exDividendDays,
                               QuantLib::Real npv) const;
        QuantLib::Rate irr(QuantLib::Real marketPrice,
                           const QuantLib::DayCounter& dayCounter,
                           QuantLib::Compounding compounding,
                           QuantLib::Frequency frequency,
                           QuantLib::Date settlementDate,
                           QuantLib::Real tolerance,
                           QuantLib::Size maxIterations,
                           QuantLib::Rate guess) const;
        QuantLib::Time duration(const QuantLib::InterestRate& y,
                                QuantLib::Duration::Type type,
                                QuantLib::Date settlementDate) const;
        QuantLib::Real convexity(const QuantLib::InterestRate& y,
                                QuantLib::Date settlementDate) const;

        void setCouponPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& p);

        std::vector<std::vector<boost::any> > analysis() const;
        const QuantLib::Leg& getQuantLibLeg();
      protected:
        OH_OBJ_CTOR(Leg, ObjectHandler::Object);
        // copy or shared_ptr?
        QuantLib::Leg leg_;
    };

    class MultiPhaseLeg : public Leg {
      public:
        MultiPhaseLeg(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                      const std::vector<boost::shared_ptr<Leg> >& legs,
                      bool toBeSorted,
                      bool permanent);
    };

    class SimpleCashFlowVector : public Leg {
      public:
        SimpleCashFlowVector(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                             const std::vector<QuantLib::Real>& amounts,
                             const std::vector<QuantLib::Date>& dates,
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
