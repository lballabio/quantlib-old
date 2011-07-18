/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2011 Ferdinando Ametrano
 Copyright (C) 2006 Chiara Fornarola

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

#ifndef qla_asset_swap_hpp
#define qla_asset_swap_hpp

#include <qlo/swap.hpp>

namespace QuantLib {
    class Bond;
}

namespace QuantLibAddin {
    
    class AssetSwap : public Swap {
    public:
        AssetSwap(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  bool payFixedRate,
                  const boost::shared_ptr<QuantLib::Bond>& bond,
                  const QuantLib::Real bondCleanPrice,
                  const boost::shared_ptr<QuantLib::IborIndex>& index,
                  QuantLib::Spread spread,
                  //const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                  const boost::shared_ptr<QuantLib::Schedule>& floatSchedule,
                  const QuantLib::DayCounter& floatingDayCount,
                  bool parSwap,
                  bool permanent);
                                      
        std::vector<std::vector<ObjectHandler::property_t> > bondLeg(
                                                    const QuantLib::Date& d) {
            return Swap::legAnalysis(0, d);
        }

        std::vector<std::vector<ObjectHandler::property_t> > floatingLeg(
                                                    const QuantLib::Date& d) {
            return Swap::legAnalysis(1, d);
        }

    };
}

#endif
