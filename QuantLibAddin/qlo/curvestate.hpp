
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

#ifndef qla_curvestate_hpp
#define qla_curvestate_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class CurveState;
}

namespace QuantLibAddin {

    OH_LIB_CLASS(CurveState, QuantLib::CurveState)

    class CMSwapCurveState : public CurveState {
      public:
        CMSwapCurveState(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Time>& rateTimes, 
            QuantLib::Size spanningForwards,
            bool permanent);
    };

    class CoterminalSwapCurveState : public CurveState {
      public:
        CoterminalSwapCurveState(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Time>& rateTimes,
            bool permanent);
    };

    class LMMCurveState : public CurveState {
      public:
        LMMCurveState(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Time>& rateTimes,
            bool permanent);
    };

    std::vector<QuantLib::Rate> qlForwardsFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Rate> qlCoterminalSwapRatesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Real> qlCoterminalSwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Rate> qlConstantMaturitySwapRatesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);

    std::vector<QuantLib::Real> qlConstantMaturitySwapAnnuitiesFromDiscountRatios(
                            const QuantLib::Size spanningForwards,
                            const QuantLib::Size firstValidIndex,
                            const std::vector<QuantLib::DiscountFactor>& ds,
                            const std::vector<QuantLib::Time>& taus);
}

#endif

