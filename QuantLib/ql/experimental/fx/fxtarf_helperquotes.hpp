/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file fxtarf_helperquotes.hpp
    \brief helper quotes for fx tarf instrument
*/

#ifndef quantlib_fxtarf_helperquotes_hpp
#define quantlib_fxtarf_helperquotes_hpp

#include <ql/quote.hpp>
#include <ql/experimental/fx/fxtarf.hpp>

namespace QuantLib {

/*! These quotes can be used as the accumulatedAmount and lastAmount
    parameters in an FxTarf instrument. They read historic values
    from the ExchangeRate Manager.
    Warning: Since the ExchangeRateManager lacks observability, updates
    are only recognized, if the pricing is triggered otherwise (e.g.
    by manually calling the update method of the instrument, the pricing
    engine or the classes below). These classes do not observe anything
    therefore and do not send notifications if observed, because the only
    additional observables would be the evaluation date and the instrument
    which are (should be) observed by the pricing engines anyway.
 */

class FxTarfAccumulatedAmountHelper : public Quote {
  public:
    FxTarfAccumulatedAmountHelper(const boost::shared_ptr<FxTarf> &instrument);
    Real value() const;
    bool isValid() const;

  private:
    const boost::shared_ptr<FxTarf> instrument_;
};

class FxTarfLastAmountHelper : public Quote {
  public:
    FxTarfLastAmountHelper(const boost::shared_ptr<FxTarf> &instrument);
    Real value() const;
    bool isValid() const;

  private:
    const boost::shared_ptr<FxTarf> instrument_;
};

} // namespace QuantLib

#endif
