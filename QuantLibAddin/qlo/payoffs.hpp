
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_payoffs_hpp
#define qla_payoffs_hpp

#include <oh/objhandler.hpp>
#include <ql/Instruments/payoffs.hpp>

namespace QuantLibAddin {

    class Payoff : public ObjHandler::LibraryObject<QuantLib::Payoff> {};

    class TypePayoff : public Payoff {};

    class StrikedTypePayoff : public TypePayoff {
      public:
          StrikedTypePayoff() {}
          StrikedTypePayoff(
              const std::string& payoffID,
              const QuantLib::Option::Type& optionType,
              const QuantLib::Real strike,
              const QuantLib::Real thirdParameter);
          QuantLib::Real thirdParameter() const;
    };

    class PlainVanillaPayoff : public StrikedTypePayoff {};
    
}

#endif
