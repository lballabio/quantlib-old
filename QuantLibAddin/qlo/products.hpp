
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#ifndef qla_products_hpp
#define qla_products_hpp

#include <oh/objecthandler.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class MarketModelMultiProduct;
    class Payoff;
}

namespace QuantLibAddin {

    class MarketModelMultiProduct : public ObjectHandler::LibraryObject<
        QuantLib::MarketModelMultiProduct> {
      public:
          std::string evolution() const;
    };

    class MarketModelComposite : public MarketModelMultiProduct {};

    class MultiProductComposite : public MarketModelComposite {
      public:
        MultiProductComposite();
    };

    class OneStepForwards : public MarketModelMultiProduct {
      public:
        OneStepForwards(const std::vector<QuantLib::Time>& rateTimes,
                        const std::vector<QuantLib::Real>& accruals,
                        const std::vector<QuantLib::Time>& paymentTimes,
                        const std::vector<QuantLib::Rate>& strikes);
    };

    class OneStepOptionlets : public MarketModelMultiProduct {
      public:
        OneStepOptionlets(
                const std::vector<QuantLib::Time>& rateTimes,
                const std::vector<QuantLib::Real>& accruals,
                const std::vector<QuantLib::Time>& paymentTimes,
                const std::vector<boost::shared_ptr<QuantLib::Payoff> >&);
    };

    class MultiStepRatchet : public MarketModelMultiProduct {
      public:
        MultiStepRatchet(const std::vector<QuantLib::Time>& rateTimes,
                         const std::vector<QuantLib::Real>& accruals,
                         const std::vector<QuantLib::Time>& paymentTimes,
                         QuantLib::Real gearingOfFloor,
                         QuantLib::Real gearingOfFixing,
                         QuantLib::Rate spreadOfFloor,
                         QuantLib::Rate spreadOfFixing,
                         QuantLib::Real initialFloor,
                         bool payer);
    };
}

#endif
