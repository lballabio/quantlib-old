/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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


#include <ql/termstructures/volatility/swaption/swaptiongaussian1dvol.hpp>
#include <ql/termstructures/volatility/swaption/gaussian1dswaptionsmilesection.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>

namespace QuantLib {


    Gaussian1dSwaptionVolatility::Gaussian1dSwaptionVolatility(boost::shared_ptr<Gaussian1dModel> &model,
                                 boost::shared_ptr<SwapIndex> &index) :
        SwaptionVolatilityStructure(model->termStructure()->referenceDate(),
                                    model->termStructure()->calendar(),
                                    Following,
                                    model->termStructure()->dayCounter()),
        model_(model), index_(index), maxSwapTenor_(100*Years) {}


    boost::shared_ptr<SmileSection>
    Gaussian1dSwaptionVolatility::smileSectionImpl(const Date& d,
                                                 const Period& tenor) const {

        boost::shared_ptr<SmileSection> raw(new Gaussian1dSwaptionSmileSection(model_, index_, d, tenor));
        return raw;
    }

    boost::shared_ptr<SmileSection>
    Gaussian1dSwaptionVolatility::smileSectionImpl(Time optionTime,
                                                 Time swapLength) const {

        DateHelper hlp(*this,optionTime);
        NewtonSafe newton;
        Date d(static_cast<BigInteger>(newton.solve(hlp, 0.1, 
                                                    365.25*optionTime+static_cast<Real>(referenceDate().serialNumber()),1.0)));
        Period tenor(static_cast<Integer>(Rounding(0).operator()(swapLength*12.0)), Months);
        d = index_->fixingCalendar().adjust(d);
        
        boost::shared_ptr<SmileSection> raw(new Gaussian1dSwaptionSmileSection(model_, index_, d, tenor));
        return raw;
    }

    Volatility Gaussian1dSwaptionVolatility::volatilityImpl(const Date& d,
                                                          const Period& tenor,
                                                          Rate strike) const {
        return smileSectionImpl(d,tenor)->volatility(strike);
    }

    Volatility Gaussian1dSwaptionVolatility::volatilityImpl(Time optionTime,
                                                          Time swapLength,
                                                          Rate strike) const {
        return smileSectionImpl(optionTime,swapLength)->volatility(strike);
    }

    

}
