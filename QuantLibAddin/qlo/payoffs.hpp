
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_payoffs_hpp
#define qla_payoffs_hpp

#include <oh/libraryobject.hpp>
#include <ql/instruments/payoffs.hpp>

namespace QuantLibAddin {

    OH_LIB_CLASS(Payoff, QuantLib::Payoff)

    OH_OBJ_CLASS(TypePayoff, Payoff)

    class StrikedTypePayoff : public TypePayoff {
      public:
          //StrikedTypePayoff() {}
          OH_OBJ_CTOR(StrikedTypePayoff, TypePayoff)
          StrikedTypePayoff(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              const std::string& payoffID,
              const QuantLib::Option::Type& optionType,
              const QuantLib::Real strike,
              const QuantLib::Real thirdParameter,
              bool permanent);
          QuantLib::Real thirdParameter() const;
    };

    OH_OBJ_CLASS(PlainVanillaPayoff, StrikedTypePayoff)
    
}

#endif

