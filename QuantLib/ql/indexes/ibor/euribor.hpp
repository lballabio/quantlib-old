/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 Chiara Fornarola
 Copyright (C) 2009 Roland Lichters
 Copyright (C) 2009 Ferdinando Ametrano
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

/*! \file euribor.hpp
    \brief %Euribor index
*/

#ifndef quantlib_euribor_hpp
#define quantlib_euribor_hpp

#include <ql/indexes/iborindex.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/currencies/europe.hpp>

namespace QuantLib {

//! %Euribor index
/*! Euribor rate fixed by the ECB.

    \warning This is the rate fixed by the ECB. Use EurLibor
             if you're interested in the London fixing by BBA.
*/
template <class T> class Euribor_t : public IborIndex_t<T> {
  public:
    Euribor_t(const Period &tenor, const Handle<YieldTermStructure_t<T> > &h =
                                       Handle<YieldTermStructure_t<T> >());
};

typedef Euribor_t<Real> Euribor;

//! Actual/365 %Euribor index
/*! Euribor rate adjusted for the mismatch between the actual/360
    convention used for Euribor and the actual/365 convention
    previously used by a few pre-EUR currencies.
*/
template <class T> class Euribor365_t : public IborIndex_t<T> {
  public:
    Euribor365_t(const Period &tenor,
                 const Handle<YieldTermStructure_t<T> > &h =
                     Handle<YieldTermStructure_t<T> >());
};

typedef Euribor365_t<Real> Euribor365;

//! 1-week %Euribor index
template <class T> class EuriborSW_t : public Euribor_t<T> {
  public:
    EuriborSW_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(1, Weeks), h) {}
};

typedef EuriborSW_t<Real> EuriborSW;

//! 2-weeks %Euribor index
template <class T> class Euribor2W_t : public Euribor_t<T> {
  public:
    Euribor2W_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(2, Weeks), h) {}
};

typedef Euribor2W_t<Real> Euribor2W;

//! 3-weeks %Euribor index
template <class T> class Euribor3W_t : public Euribor_t<T> {
  public:
    Euribor3W_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(3, Weeks), h) {}
};

typedef Euribor3W_t<Real> Euribor3W;

//! 1-month %Euribor index
template <class T> class Euribor1M_t : public Euribor_t<T> {
  public:
    Euribor1M_t(const Handle<YieldTermStructure_t<T> > &h =
                  Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(1, Months), h) {}
};

typedef Euribor1M_t<Real> Euribor1M;

//! 2-months %Euribor index
template <class T> class Euribor2M_t : public Euribor_t<T> {
  public:
    Euribor2M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(2, Months), h) {}
};

typedef Euribor2M_t<Real> Euribor2M;

//! 3-months %Euribor index
template <class T> class Euribor3M_t : public Euribor_t<T> {
  public:
    Euribor3M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(3, Months), h) {}
};

typedef Euribor3M_t<Real> Euribor3M;

//! 4-months %Euribor index
template <class T> class Euribor4M_t : public Euribor_t<T> {
  public:
    Euribor4M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(4, Months), h) {}
};

typedef Euribor4M_t<Real> Euribor4M;

//! 5-months %Euribor index
template <class T> class Euribor5M_t : public Euribor_t<T> {
  public:
    Euribor5M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(5, Months), h) {}
};

typedef Euribor5M_t<Real> Euribor5M;

//! 6-months %Euribor index
template <class T> class Euribor6M_t : public Euribor_t<T> {
  public:
    Euribor6M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(6, Months), h) {}
};

typedef Euribor6M_t<Real> Euribor6M;

//! 7-months %Euribor index
template <class T> class Euribor7M_t : public Euribor_t<T> {
  public:
    Euribor7M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(7, Months), h) {}
};

typedef Euribor7M_t<Real> Euribor7M;

//! 8-months %Euribor index
template <class T> class Euribor8M_t : public Euribor_t<T> {
  public:
    Euribor8M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(8, Months), h) {}
};

typedef Euribor8M_t<Real> Euribor8M;

//! 9-months %Euribor index
template <class T> class Euribor9M_t : public Euribor_t<T> {
  public:
    Euribor9M_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(9, Months), h) {}
};

typedef Euribor9M_t<Real> Euribor9M;

//! 10-months %Euribor index
template <class T> class Euribor10M_t : public Euribor_t<T> {
  public:
    Euribor10M_t(const Handle<YieldTermStructure_t<T> > &h =
                     Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(10, Months), h) {}
};

typedef Euribor10M_t<Real> Euribor10M;

//! 11-months %Euribor index
template <class T> class Euribor11M_t : public Euribor_t<T> {
  public:
    Euribor11M_t(const Handle<YieldTermStructure_t<T> > &h =
                     Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(11, Months), h) {}
};

typedef Euribor11M_t<Real> Euribor11M;

//! 1-year %Euribor index
template <class T> class Euribor1Y_t : public Euribor_t<T> {
  public:
    Euribor1Y_t(const Handle<YieldTermStructure_t<T> > &h =
                    Handle<YieldTermStructure_t<T> >())
        : Euribor_t<T>(Period(1, Years), h) {}
};

typedef Euribor1Y_t<Real> Euribor1Y;

//! 1-week %Euribor365 index
template <class T> class Euribor365_SW_t : public Euribor365_t<T> {
  public:
    Euribor365_SW_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(1, Weeks), h) {}
};

typedef Euribor365_SW_t<Real> Euribor365_SW;

//! 2-weeks %Euribor365 index
template <class T> class Euribor365_2W_t : public Euribor365_t<T> {
  public:
    Euribor365_2W_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(2, Weeks), h) {}
};

typedef Euribor365_2W_t<Real> Euribor365_2W;

//! 3-weeks %Euribor365 index
template <class T> class Euribor365_3W_t : public Euribor365_t<T> {
  public:
    Euribor365_3W_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(3, Weeks), h) {}
};

typedef Euribor365_3W_t<Real> Euribor365_3W;

//! 1-month %Euribor365 index
template <class T> class Euribor365_1M_t : public Euribor365_t<T> {
  public:
    Euribor365_1M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(1, Months), h) {}
};

typedef Euribor365_1M_t<Real> Euribor365_1M;

//! 2-months %Euribor365 index
template <class T> class Euribor365_2M_t : public Euribor365_t<T> {
  public:
    Euribor365_2M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(2, Months), h) {}
};

typedef Euribor365_2M_t<Real> Euribor365_2M;

//! 3-months %Euribor365 index
template <class T> class Euribor365_3M_t : public Euribor365_t<T> {
  public:
    Euribor365_3M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(3, Months), h) {}
};

typedef Euribor365_3M_t<Real> Euribor365_3M;

//! 4-months %Euribor365 index
template <class T> class Euribor365_4M_t : public Euribor365_t<T> {
  public:
    Euribor365_4M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(4, Months), h) {}
};

typedef Euribor365_4M_t<Real> Euribor365_4M;

//! 5-months %Euribor365 index
template <class T> class Euribor365_5M_t : public Euribor365_t<T> {
  public:
    Euribor365_5M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(5, Months), h) {}
};

typedef Euribor365_5M_t<Real> Euribor365_5M;

//! 6-months %Euribor365 index
template <class T> class Euribor365_6M_t : public Euribor365_t<T> {
  public:
    Euribor365_6M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(6, Months), h) {}
};

typedef Euribor365_6M_t<Real> Euribor365_6M;

//! 7-months %Euribor365 index
template <class T> class Euribor365_7M_t : public Euribor365_t<T> {
  public:
    Euribor365_7M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(7, Months), h) {}
};

typedef Euribor365_7M_t<Real> Euribor365_7M;

//! 8-months %Euribor365 index
template <class T> class Euribor365_8M_t : public Euribor365_t<T> {
  public:
    Euribor365_8M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(8, Months), h) {}
};

typedef Euribor365_8M_t<Real> Euribor365_8M;

//! 9-months %Euribor365 index
template <class T> class Euribor365_9M_t : public Euribor365_t<T> {
  public:
    Euribor365_9M_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(9, Months), h) {}
};

typedef Euribor365_9M_t<Real> Euribor365_9M;

//! 10-months %Euribor365 index
template <class T> class Euribor365_10M_t : public Euribor365_t<T> {
  public:
    Euribor365_10M_t(const Handle<YieldTermStructure_t<T> > &h =
                         Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(10, Months), h) {}
};

typedef Euribor365_10M_t<Real> Euribor365_10M;

//! 11-months %Euribor365 index
template <class T> class Euribor365_11M_t : public Euribor365_t<T> {
  public:
    Euribor365_11M_t(const Handle<YieldTermStructure_t<T> > &h =
                         Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(11, Months), h) {}
};

typedef Euribor365_11M_t<Real> Euribor365_11M;

//! 1-year %Euribor365 index
template <class T> class Euribor365_1Y_t : public Euribor365_t<T> {
  public:
    Euribor365_1Y_t(const Handle<YieldTermStructure_t<T> > &h =
                        Handle<YieldTermStructure_t<T> >())
        : Euribor365_t<T>(Period(1, Years), h) {}
};

typedef Euribor365_1Y_t<Real> Euribor365_1Y;

namespace detail {
BusinessDayConvention euriborConvention(const Period &p);
bool euriborEOM(const Period &p);
}

// implementation

template <class T>
Euribor_t<T>::Euribor_t(const Period &tenor,
                        const Handle<YieldTermStructure_t<T> > &h)
    : IborIndex_t<T>("Euribor", tenor,
                     2, // settlement days
                     EURCurrency(), TARGET(), detail::euriborConvention(tenor),
                     detail::euriborEOM(tenor), Actual360(), h) {
    QL_REQUIRE(this->tenor().units() != Days,
               "for daily tenors ("
                   << this->tenor()
                   << ") dedicated DailyTenor constructor must be used");
}

template <class T>
Euribor365_t<T>::Euribor365_t(const Period &tenor,
                              const Handle<YieldTermStructure_t<T> > &h)
    : IborIndex_t<T>("Euribor365", tenor,
                     2, // settlement days
                     EURCurrency(), TARGET(), detail::euriborConvention(tenor),
                     detail::euriborEOM(tenor), Actual365Fixed(), h) {
    QL_REQUIRE(this->tenor().units() != Days,
               "for daily tenors ("
                   << this->tenor()
                   << ") dedicated DailyTenor constructor must be used");
}
}

#endif
