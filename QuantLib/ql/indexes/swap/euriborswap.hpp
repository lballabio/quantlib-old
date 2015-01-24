/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008, 2009 Ferdinando Ametrano
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

/*! \file euriborswap.hpp
    \brief Euribor %Swap indexes
*/

#ifndef quantlib_euriborswap_hpp
#define quantlib_euriborswap_hpp

#include <ql/indexes/swapindex.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <ql/currencies/europe.hpp>

using boost::shared_ptr;

namespace QuantLib {

//! %EuriborSwapIsdaFixA index base class
/*! %Euribor %Swap indexes fixed by ISDA in cooperation with
    Reuters and Intercapital Brokers at 11am Frankfurt.
    Annual 30/360 vs 6M Euribor, 1Y vs 3M Euribor.
    Reuters page ISDAFIX2 or EURSFIXA=.

    Further info can be found at <http://www.isda.org/fix/isdafix.html> or
    Reuters page ISDAFIX.

*/
template <class T> class EuriborSwapIsdaFixA_t : public SwapIndex_t<T> {
  public:
    EuriborSwapIsdaFixA_t(const Period &tenor,
                          const Handle<YieldTermStructure_t<T> > &h =
                              Handle<YieldTermStructure_t<T> >());
    EuriborSwapIsdaFixA_t(const Period &tenor,
                          const Handle<YieldTermStructure_t<T> > &forwarding,
                          const Handle<YieldTermStructure_t<T> > &discounting);
};

//! %EuriborSwapIsdaFixB index base class
/*! %Euribor %Swap indexes fixed by ISDA in cooperation with
    Reuters and Intercapital Brokers at 12am Frankfurt.
    Annual 30/360 vs 6M Euribor, 1Y vs 3M Euribor.
    Reuters page ISDAFIX2 or EURSFIXB=.

    Further info can be found at <http://www.isda.org/fix/isdafix.html> or
    Reuters page ISDAFIX.

*/
template <class T> class EuriborSwapIsdaFixB_t : public SwapIndex_t<T> {
  public:
    EuriborSwapIsdaFixB_t(const Period &tenor,
                          const Handle<YieldTermStructure_t<T> > &h =
                              Handle<YieldTermStructure_t<T> >());
    EuriborSwapIsdaFixB_t(const Period &tenor,
                          const Handle<YieldTermStructure_t<T> > &forwarding,
                          const Handle<YieldTermStructure_t<T> > &discounting);
};

//! %EuriborSwapIfrFix index base class
/*! %Euribor %Swap indexes published by IFR Markets and
    distributed by Reuters page TGM42281 and by Telerate.
    Annual 30/360 vs 6M Euribor, 1Y vs 3M Euribor.
    For more info see <http://www.ifrmarkets.com>.

*/
template <class T> class EuriborSwapIfrFix_t : public SwapIndex_t<T> {
  public:
    EuriborSwapIfrFix_t(const Period &tenor,
                        const Handle<YieldTermStructure_t<T> > &h =
                            Handle<YieldTermStructure_t<T> >());
    EuriborSwapIfrFix_t(const Period &tenor,
                        const Handle<YieldTermStructure_t<T> > &forwarding,
                        const Handle<YieldTermStructure_t<T> > &discounting);
};

typedef EuriborSwapIsdaFixA_t<Real> EuriborSwapIsdaFixA;
typedef EuriborSwapIsdaFixB_t<Real> EuriborSwapIsdaFixB;
typedef EuriborSwapIfrFix_t<Real> EuriborSwapFixIfr;

// Implementation

template <class T>
EuriborSwapIsdaFixA_t<T>::EuriborSwapIsdaFixA_t(
    const Period &tenor,
    const Handle<YieldTermStructure_t<T> > &h)
    : SwapIndex("EuriborSwapIsdaFixA", // familyName
                tenor,
                2, // settlementDays
                EURCurrency(), TARGET(),
                1 * Years,                       // fixedLegTenor
                ModifiedFollowing,               // fixedLegConvention
                Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
                tenor > 1 * Years
                    ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, h))
                    : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, h))) {}

template <class T>
EuriborSwapIsdaFixA_t<T>::EuriborSwapIsdaFixA_t(
    const Period &tenor, const Handle<YieldTermStructure_t<T> > &forwarding,
    const Handle<YieldTermStructure_t<T> > &discounting)
    : SwapIndex(
          "EuriborSwapIsdaFixA", // familyName
          tenor,
          2, // settlementDays
          EURCurrency(), TARGET(),
          1 * Years,                       // fixedLegTenor
          ModifiedFollowing,               // fixedLegConvention
          Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
          tenor > 1 * Years
              ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, forwarding))
              : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, forwarding)),
          discounting) {}

template <class T>
EuriborSwapIsdaFixB_t<T>::EuriborSwapIsdaFixB_t(
    const Period &tenor,
    const Handle<YieldTermStructure_t<T> > &h)
    : SwapIndex("EuriborSwapIsdaFixB", // familyName
                tenor,
                2, // settlementDays
                EURCurrency(), TARGET(),
                1 * Years,                       // fixedLegTenor
                ModifiedFollowing,               // fixedLegConvention
                Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
                tenor > 1 * Years
                    ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, h))
                    : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, h))) {}

template <class T>
EuriborSwapIsdaFixB_t<T>::EuriborSwapIsdaFixB_t(
    const Period &tenor, const Handle<YieldTermStructure_t<T> > &forwarding,
    const Handle<YieldTermStructure_t<T> > &discounting)
    : SwapIndex(
          "EuriborSwapIsdaFixB", // familyName
          tenor,
          2, // settlementDays
          EURCurrency(), TARGET(),
          1 * Years,                       // fixedLegTenor
          ModifiedFollowing,               // fixedLegConvention
          Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
          tenor > 1 * Years
              ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, forwarding))
              : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, forwarding)),
          discounting) {}

template <class T>
EuriborSwapIfrFix_t<T>::EuriborSwapIfrFix_t(
    const Period &tenor,
    const Handle<YieldTermStructure_t<T> > &h)
    : SwapIndex("EuriborSwapIfrFix", // familyName
                tenor,
                2, // settlementDays
                EURCurrency(), TARGET(),
                1 * Years,                       // fixedLegTenor
                ModifiedFollowing,               // fixedLegConvention
                Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
                tenor > 1 * Years
                    ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, h))
                    : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, h))) {}

template <class T>
EuriborSwapIfrFix_t<T>::EuriborSwapIfrFix_t(
    const Period &tenor, const Handle<YieldTermStructure_t<T> > &forwarding,
    const Handle<YieldTermStructure_t<T> > &discounting)
    : SwapIndex(
          "EuriborSwapIfrFix", // familyName
          tenor,
          2, // settlementDays
          EURCurrency(), TARGET(),
          1 * Years,                       // fixedLegTenor
          ModifiedFollowing,               // fixedLegConvention
          Thirty360(Thirty360::BondBasis), // fixedLegDaycounter
          tenor > 1 * Years
              ? shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(6 * Months, forwarding))
              : shared_ptr<IborIndex_t<T>>(new Euribor_t<T>(3 * Months, forwarding)),
          discounting) {}
}

#endif
