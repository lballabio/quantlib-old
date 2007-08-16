
/*
 Copyright (C) 2007 Marco Bianchetti

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

#ifndef qla_stickyratchet_hpp
#define qla_stickyratchet_hpp

#include <oh/libraryobject.hpp>
#include <ql/instruments/stickyratchet.hpp>
#include <qlo/payoffs.hpp>

namespace QuantLibAddin {

    class DoubleStickyRatchetPayoff : public Payoff {
      public:
          //DoubleStickyRatchetPayoff() {}
          OH_OBJ_CTOR(DoubleStickyRatchetPayoff, Payoff)
          DoubleStickyRatchetPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real type1,
              const QuantLib::Real type2,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class RatchetPayoff : public DoubleStickyRatchetPayoff {
      public:
          RatchetPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Real initialValue,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class StickyPayoff : public DoubleStickyRatchetPayoff {
      public:
          StickyPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Real initialValue,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class RatchetMaxPayoff : public DoubleStickyRatchetPayoff {
      public:
          RatchetMaxPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class RatchetMinPayoff : public DoubleStickyRatchetPayoff {
      public:
          RatchetMinPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class StickyMaxPayoff : public DoubleStickyRatchetPayoff {
      public:
          StickyMaxPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

    class StickyMinPayoff : public DoubleStickyRatchetPayoff {
      public:
          StickyMinPayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor,
              bool permanent);
    };

}

#endif

