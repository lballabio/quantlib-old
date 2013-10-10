/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006 StatPro Italia srl
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

/*! \file europe.hpp
    \brief European currencies

    Data from http://fx.sauder.ubc.ca/currency_table.html
    and http://www.thefinancials.com/vortex/CurrencyFormats.html
*/

#ifndef quantlib_european_currencies_hpp
#define quantlib_european_currencies_hpp

#include <ql/currency.hpp>

namespace QuantLib {

    //! Bulgarian lev
    /*! The ISO three-letter code is BGL; the numeric code is 100.
        It is divided in 100 stotinki.

        \ingroup currencies
    */
    class BGLCurrency : public Currency {
      public:
        BGLCurrency();
      protected:
        static Data bglData_;
    };

    //! Belarussian ruble
    /*! The ISO three-letter code is BYR; the numeric code is 974.
        It has no subdivisions.

        \ingroup currencies
    */
    class BYRCurrency : public Currency {
      public:
        BYRCurrency();
      protected:
        static Data byrData_;
    };

    //! Swiss franc
    /*! The ISO three-letter code is CHF; the numeric code is 756.
        It is divided into 100 cents.

        \ingroup currencies
    */
    class CHFCurrency : public Currency {
      public:
        CHFCurrency();
      protected:
        static Data chfData_;
    };

    //! Cyprus pound
    /*! The ISO three-letter code is CYP; the numeric code is 196.
        It is divided in 100 cents.

        \ingroup currencies
    */
    class CYPCurrency : public Currency {
      public:
        CYPCurrency();
      protected:
        static Data cypData_;
    };

    //! Czech koruna
    /*! The ISO three-letter code is CZK; the numeric code is 203.
        It is divided in 100 haleru.

        \ingroup currencies
    */
    class CZKCurrency : public Currency {
      public:
        CZKCurrency();
      protected:
        static Data czkData_;
    };

    //! Danish krone
    /*! The ISO three-letter code is DKK; the numeric code is 208.
        It is divided in 100 øre.

        \ingroup currencies
    */
    class DKKCurrency : public Currency {
      public:
        DKKCurrency();
      protected:
        static Data dkkData_;
    };

    //! Estonian kroon
    /*! The ISO three-letter code is EEK; the numeric code is 233.
        It is divided in 100 senti.

        \ingroup currencies
    */
    class EEKCurrency : public Currency {
      public:
        EEKCurrency();
      protected:
        static Data eekData_;
    };

    //! European Euro
    /*! The ISO three-letter code is EUR; the numeric code is 978.
        It is divided into 100 cents.

        \ingroup currencies
    */
    class EURCurrency : public Currency {
      public:
        EURCurrency();
      protected:
        static Data eurData_;
    };

    //! British pound sterling
    /*! The ISO three-letter code is GBP; the numeric code is 826.
        It is divided into 100 pence.

        \ingroup currencies
    */
    class GBPCurrency : public Currency {
      public:
        GBPCurrency();
      protected:
        static Data gbpData_;
    };

    //! Hungarian forint
    /*! The ISO three-letter code is HUF; the numeric code is 348.
        It has no subdivisions.

        \ingroup currencies
    */
    class HUFCurrency : public Currency {
      public:
        HUFCurrency();
      protected:
        static Data hufData_;
    };

    //! Icelandic krona
    /*! The ISO three-letter code is ISK; the numeric code is 352.
        It is divided in 100 aurar.

        \ingroup currencies
    */
    class ISKCurrency : public Currency {
      public:
        ISKCurrency();
      protected:
        static Data iskData_;
    };

    //! Lithuanian litas
    /*! The ISO three-letter code is LTL; the numeric code is 440.
        It is divided in 100 centu.

        \ingroup currencies
    */
    class LTLCurrency : public Currency {
      public:
        LTLCurrency();
      protected:
        static Data ltlData_;
    };

    //! Latvian lat
    /*! The ISO three-letter code is LVL; the numeric code is 428.
        It is divided in 100 santims.

        \ingroup currencies
    */
    class LVLCurrency : public Currency {
      public:
        LVLCurrency();
      protected:
        static Data lvlData_;
    };

    //! Maltese lira
    /*! The ISO three-letter code is MTL; the numeric code is 470.
        It is divided in 100 cents.

        \ingroup currencies
    */
    class MTLCurrency : public Currency {
      public:
        MTLCurrency();
      protected:
        static Data mtlData_;
    };

    //! Norwegian krone
    /*! The ISO three-letter code is NOK; the numeric code is 578.
        It is divided in 100 øre.

        \ingroup currencies
    */
    class NOKCurrency : public Currency {
      public:
        NOKCurrency();
      protected:
        static Data nokData_;
    };

    //! Polish zloty
    /*! The ISO three-letter code is PLN; the numeric code is 985.
        It is divided in 100 groszy.

        \ingroup currencies
    */
    class PLNCurrency : public Currency {
      public:
        PLNCurrency();
      protected:
        static Data plnData_;
    };

    //! Romanian leu
    /*! The ISO three-letter code was ROL; the numeric code was 642.
        It was divided in 100 bani.

        Obsoleted by the new leu since July 2005.

        \ingroup currencies
    */
    class ROLCurrency : public Currency {
      public:
       ROLCurrency();
      protected:
        static Data rolData_;
    };

    //! Romanian new leu
    /*! The ISO three-letter code is RON; the numeric code is 946.
        It is divided in 100 bani.

        \ingroup currencies
    */
    class RONCurrency : public Currency {
      public:
        RONCurrency();
      protected:
        static Data ronData_;
    };

    //! Swedish krona
    /*! The ISO three-letter code is SEK; the numeric code is 752.
        It is divided in 100 öre.

        \ingroup currencies
    */
    class SEKCurrency : public Currency {
      public:
        SEKCurrency();
      protected:
        static Data sekData_;
    };

    //! Slovenian tolar
    /*! The ISO three-letter code is SIT; the numeric code is 705.
        It is divided in 100 stotinov.

        \ingroup currencies
    */
    class SITCurrency : public Currency {
      public:
        SITCurrency();
      protected:
        static Data sitData_;
    };

    //! Slovak koruna
    /*! The ISO three-letter code is SKK; the numeric code is 703.
        It is divided in 100 halierov.

        \ingroup currencies
    */
    class SKKCurrency : public Currency {
      public:
        SKKCurrency();
      protected:
        static Data skkData_;
    };

    //! Turkish lira
    /*! The ISO three-letter code was TRL; the numeric code was 792.
        It was divided in 100 kurus.

        Obsoleted by the new Turkish lira since 2005.

        \ingroup currencies
    */
    class TRLCurrency : public Currency {
      public:
        TRLCurrency();
      protected:
        static Data trlData_;
    };

    //! New Turkish lira
    /*! The ISO three-letter code is TRY; the numeric code is 949.
        It is divided in 100 new kurus.

        \ingroup currencies
    */
    class TRYCurrency : public Currency {
      public:
        TRYCurrency();
      protected:
        static Data tryData_;
    };

    // currencies obsoleted by Euro

    //! Austrian shilling
    /*! The ISO three-letter code was ATS; the numeric code was 40.
        It was divided in 100 groschen.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class ATSCurrency : public Currency {
      public:
        ATSCurrency();
      protected:
        static Data atsData_;
    };

    //! Belgian franc
    /*! The ISO three-letter code was BEF; the numeric code was 56.
        It had no subdivisions.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class BEFCurrency : public Currency {
      public:
        BEFCurrency();
      protected:
        static Data befData_;
    };

    //! Deutsche mark
    /*! The ISO three-letter code was DEM; the numeric code was 276.
        It was divided into 100 pfennig.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class DEMCurrency : public Currency {
      public:
        DEMCurrency();
      protected:
        static Data demData_;
    };

    //! Spanish peseta
    /*! The ISO three-letter code was ESP; the numeric code was 724.
        It was divided in 100 centimos.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class ESPCurrency : public Currency {
      public:
        ESPCurrency();
      protected:
        static Data espData_;
    };

    //! Finnish markka
    /*! The ISO three-letter code was FIM; the numeric code was 246.
        It was divided in 100 penniä.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class FIMCurrency : public Currency {
      public:
        FIMCurrency();
      protected:
        static Data fimData_;
    };

    //! French franc
    /*! The ISO three-letter code was FRF; the numeric code was 250.
        It was divided in 100 centimes.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class FRFCurrency : public Currency {
      public:
        FRFCurrency();
      protected:
        static Data frfData_;
    };

    //! Greek drachma
    /*! The ISO three-letter code was GRD; the numeric code was 300.
        It was divided in 100 lepta.

        Obsoleted by the Euro since 2001.

        \ingroup currencies
    */
    class GRDCurrency : public Currency {
      public:
        GRDCurrency();
      protected:
        static Data grdData_;
    };

    //! Irish punt
    /*! The ISO three-letter code was IEP; the numeric code was 372.
        It was divided in 100 pence.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class IEPCurrency : public Currency {
      public:
        IEPCurrency();
      protected:
        static Data iepData_;
    };

    //! Italian lira
    /*! The ISO three-letter code was ITL; the numeric code was 380.
        It had no subdivisions.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class ITLCurrency : public Currency {
      public:
        ITLCurrency();
      protected:
        static Data itlData_;
    };

    //! Luxembourg franc
    /*! The ISO three-letter code was LUF; the numeric code was 442.
        It was divided in 100 centimes.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class LUFCurrency : public Currency {
      public:
        LUFCurrency();
      protected:
        static Data lufData_;
    };

    //! Dutch guilder
    /*! The ISO three-letter code was NLG; the numeric code was 528.
        It was divided in 100 cents.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class NLGCurrency : public Currency {
      public:
        NLGCurrency();
      protected:
        static Data nlgData_;
    };

    //! Portuguese escudo
    /*! The ISO three-letter code was PTE; the numeric code was 620.
        It was divided in 100 centavos.

        Obsoleted by the Euro since 1999.

        \ingroup currencies
    */
    class PTECurrency : public Currency {
      public:
        PTECurrency();
      protected:
        static Data pteData_;
    };

}

#endif
