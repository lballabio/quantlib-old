/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005 StatPro Italia srl
 Copyright (C) 2012 Simon Shakeshaft

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

/*! \file asia.hpp
    \brief Asian currencies

    Data from http://fx.sauder.ubc.ca/currency_table.html
    and http://www.thefinancials.com/vortex/CurrencyFormats.html
*/

#ifndef quantlib_asian_currencies_hpp
#define quantlib_asian_currencies_hpp

#include <ql/currency.hpp>

namespace QuantLib {

    //! Bangladesh taka
    /*! The ISO three-letter code is BDT; the numeric code is 50.
        It is divided in 100 paisa.

        \ingroup currencies
    */
    class BDTCurrency : public Currency {
      public:
        BDTCurrency();
      protected:
        static Data bdtData_;
    };

    //! Chinese yuan
    /*! The ISO three-letter code is CNY; the numeric code is 156.
        It is divided in 100 fen.

        \ingroup currencies
    */
    class CNYCurrency : public Currency {
      public:
        CNYCurrency();
      protected:
        static Data cnyData_;
    };

    //! Honk Kong dollar
    /*! The ISO three-letter code is HKD; the numeric code is 344.
        It is divided in 100 cents.

        \ingroup currencies
    */
    class HKDCurrency : public Currency {
      public:
        HKDCurrency();
      protected:
        static Data hkdData_;
    };

    //! Israeli shekel
    /*! The ISO three-letter code is ILS; the numeric code is 376.
        It is divided in 100 agorot.

        \ingroup currencies
    */
    class ILSCurrency : public Currency {
      public:
        ILSCurrency();
      protected:
        static Data ilsData_;
    };

    //! Indian rupee
    /*! The ISO three-letter code is INR; the numeric code is 356.
        It is divided in 100 paise.

        \ingroup currencies
    */
    class INRCurrency : public Currency {
      public:
        INRCurrency();
      protected:
        static Data inrData_;
    };

    //! Iraqi dinar
    /*! The ISO three-letter code is IQD; the numeric code is 368.
        It is divided in 1000 fils.

        \ingroup currencies
    */
    class IQDCurrency : public Currency {
      public:
        IQDCurrency();
      protected:
        static Data iqdData_;
    };

    //! Iranian rial
    /*! The ISO three-letter code is IRR; the numeric code is 364.
        It has no subdivisions.

        \ingroup currencies
    */
    class IRRCurrency : public Currency {
      public:
        IRRCurrency();
      protected:
        static Data irrData_;
    };

    //! Japanese yen
    /*! The ISO three-letter code is JPY; the numeric code is 392.
        It is divided into 100 sen.

        \ingroup currencies
    */
    class JPYCurrency : public Currency {
      public:
        JPYCurrency();
      protected:
        static Data jpyData_;
    };

    //! South-Korean won
    /*! The ISO three-letter code is KRW; the numeric code is 410.
        It is divided in 100 chon.

        \ingroup currencies
    */
    class KRWCurrency : public Currency {
      public:
        KRWCurrency();
      protected:
        static Data krwData_;
    };

    //! Kuwaiti dinar
    /*! The ISO three-letter code is KWD; the numeric code is 414.
        It is divided in 1000 fils.

        \ingroup currencies
    */
    class KWDCurrency : public Currency {
      public:
        KWDCurrency();
      protected:
        static Data kwdData_;
    };

    //! Nepal rupee
    /*! The ISO three-letter code is NPR; the numeric code is 524.
        It is divided in 100 paise.

        \ingroup currencies
    */
    class NPRCurrency : public Currency {
      public:
        NPRCurrency();
      protected:
        static Data nprData_;
    };

    //! Pakistani rupee
    /*! The ISO three-letter code is PKR; the numeric code is 586.
        It is divided in 100 paisa.

        \ingroup currencies
    */
    class PKRCurrency : public Currency {
      public:
        PKRCurrency();
      protected:
        static Data pkrData_;
    };

    //! Saudi riyal
    /*! The ISO three-letter code is SAR; the numeric code is 682.
        It is divided in 100 halalat.

        \ingroup currencies
    */
    class SARCurrency : public Currency {
      public:
        SARCurrency();
      protected:
        static Data sarData_;
    };

    //! %Singapore dollar
    /*! The ISO three-letter code is SGD; the numeric code is 702.
        It is divided in 100 cents.

        \ingroup currencies
    */
    class SGDCurrency : public Currency {
      public:
        SGDCurrency();
      protected:
        static Data sgdData_;
    };

    //! Thai baht
    /*! The ISO three-letter code is THB; the numeric code is 764.
        It is divided in 100 stang.

        \ingroup currencies
    */
    class THBCurrency : public Currency {
      public:
        THBCurrency();
      protected:
        static Data thbData_;
    };

    //! %Taiwan dollar
    /*! The ISO three-letter code is TWD; the numeric code is 901.
        It is divided in 100 cents.

        \ingroup currencies
    */
    class TWDCurrency : public Currency {
      public:
        TWDCurrency();
      protected:
        static Data twdData_;
    };
}

#endif
