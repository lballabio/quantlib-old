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

#include <ql/currencies/europe.hpp>

namespace QuantLib {

    namespace {

        struct null_deleter {
            void operator()(void const *) const {
            }
        };

    }

    Currency::Data BGLCurrency::bglData_("Bulgarian lev", "BGL", 100,
                                         "lv", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data BYRCurrency::byrData_("Belarussian ruble", "BYR", 974,
                                         "BR", "", 1,
                                         Rounding(),
                                         "%2% %1$.0f");

    Currency::Data CHFCurrency::chfData_("Swiss franc", "CHF", 756,
                                         "SwF", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data CYPCurrency::cypData_("Cyprus pound", "CYP", 196,
                                         "\xA3" "C", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data CZKCurrency::czkData_("Czech koruna", "CZK", 203,
                                         "Kc", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data DKKCurrency::dkkData_("Danish krone", "DKK", 208,
                                         "Dkr", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");
    
    Currency::Data EEKCurrency::eekData_("Estonian kroon", "EEK", 233,
                                         "KR", "", 100,
                                         Rounding(),
                                         "%1$.2f %2%");
                                                   
    Currency::Data EURCurrency::eurData_("European Euro", "EUR", 978,
                                         "", "", 100,
                                         ClosestRounding(2),
                                         "%2% %1$.2f");

    Currency::Data GBPCurrency::gbpData_("British pound sterling", "GBP", 826,
                                         "\xA3", "p", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data HUFCurrency::hufData_("Hungarian forint", "HUF", 348,
                                         "Ft", "", 1,
                                         Rounding(),
                                         "%1$.0f %3%");

    Currency::Data ISKCurrency::iskData_("Iceland krona", "ISK", 352,
                                         "IKr", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data LTLCurrency::ltlData_("Lithuanian litas", "LTL", 440,
                                         "Lt", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");


    Currency::Data LVLCurrency::lvlData_("Latvian lat", "LVL", 428,
                                         "Ls", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data MTLCurrency::mtlData_("Maltese lira", "MTL", 470,
                                         "Lm", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data NOKCurrency::nokData_("Norwegian krone", "NOK", 578,
                                         "NKr", "", 100,
                                         Rounding(),
                                         "%3% %1$.2f");

    Currency::Data PLNCurrency::plnData_("Polish zloty", "PLN", 985,
                                         "zl", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data ROLCurrency::rolData_("Romanian leu", "ROL", 642,
                                         "L", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data RONCurrency::ronData_("Romanian new leu",
                                         "RON", 946,
                                         "L", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");
    
    Currency::Data SEKCurrency::sekData_("Swedish krona", "SEK", 752,
                                         "kr", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data SITCurrency::sitData_("Slovenian tolar", "SIT", 705,
                                         "SlT", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data SKKCurrency::skkData_("Slovak koruna", "SKK", 703,
                                         "Sk", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");

    Currency::Data TRLCurrency::trlData_("Turkish lira", "TRL", 792,
                                         "TL", "", 100,
                                         Rounding(),
                                         "%1$.0f %3%");

    Currency::Data TRYCurrency::tryData_("New Turkish lira", "TRY", 949,
                                         "YTL", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%");


    Currency::Data ATSCurrency::atsData_("Austrian shilling", "ATS", 40,
                                         "", "", 100,
                                         Rounding(),
                                         "%2% %1$.2f",
                                         EURCurrency());
    
    Currency::Data BEFCurrency::befData_("Belgian franc", "BEF", 56,
                                         "", "", 1,
                                         Rounding(),
                                         "%2% %1$.0f",
                                         EURCurrency());

    Currency::Data DEMCurrency::demData_("Deutsche mark", "DEM", 276,
                                         "DM", "", 100,
                                         Rounding(),
                                         "%1$.2f %3%",
                                         EURCurrency());

    Currency::Data ESPCurrency::espData_("Spanish peseta", "ESP", 724,
                                         "Pta", "", 100,
                                         Rounding(),
                                         "%1$.0f %3%",
                                         EURCurrency());

    Currency::Data FIMCurrency::fimData_("Finnish markka", "FIM", 246,
                                                 "mk", "", 100,
                                                 Rounding(),
                                                 "%1$.2f %3%",
                                                 EURCurrency());

    Currency::Data FRFCurrency::frfData_("French franc", "FRF", 250,
                                         "", "", 100,
                                         Rounding(),
                                         "%1$.2f %2%",
                                         EURCurrency());

    Currency::Data GRDCurrency::grdData_("Greek drachma", "GRD", 300,
                                         "", "", 100,
                                         Rounding(),
                                         "%1$.2f %2%",
                                         EURCurrency());

    Currency::Data IEPCurrency::iepData_("Irish punt", "IEP", 372,
                                         "", "", 100,
                                         Rounding(),
                                         "%2% %1$.2f",
                                         EURCurrency());

    Currency::Data ITLCurrency::itlData_("Italian lira", "ITL", 380,
                                         "L", "", 1,
                                         Rounding(),
                                         "%3% %1$.0f",
                                         EURCurrency());

    Currency::Data LUFCurrency::lufData_("Luxembourg franc", "LUF", 442,
                                         "F", "", 100,
                                         Rounding(),
                                         "%1$.0f %3%",
                                         EURCurrency());

    Currency::Data NLGCurrency::nlgData_("Dutch guilder", "NLG", 528,
                                                  "f", "", 100,
                                                  Rounding(),
                                                  "%3% %1$.2f",
                                                  EURCurrency());

    Currency::Data PTECurrency::pteData_("Portuguese escudo", "PTE", 620,
                                         "Esc", "", 100,
                                         Rounding(),
                                         "%1$.0f %3%",
                                         EURCurrency());

    BGLCurrency::BGLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&bglData_, null_deleter());
    }

    BYRCurrency::BYRCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&byrData_, null_deleter());
    }

    CHFCurrency::CHFCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&chfData_, null_deleter());
    }

    CYPCurrency::CYPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&cypData_, null_deleter());
    }

    CZKCurrency::CZKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&czkData_, null_deleter());
    }

    DKKCurrency::DKKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&dkkData_, null_deleter());
    }

    EEKCurrency::EEKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&eekData_, null_deleter());
    }

    EURCurrency::EURCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&eurData_, null_deleter());
    }

    GBPCurrency::GBPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&gbpData_, null_deleter());
    }

    HUFCurrency::HUFCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&hufData_, null_deleter());
    }

    ISKCurrency::ISKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&iskData_, null_deleter());
    }

    LTLCurrency::LTLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&ltlData_, null_deleter());
    }

    LVLCurrency::LVLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&lvlData_, null_deleter());
    }

    MTLCurrency::MTLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&mtlData_, null_deleter());
    }

    NOKCurrency::NOKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&nokData_, null_deleter());
    }

    PLNCurrency::PLNCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&plnData_, null_deleter());
    }

    ROLCurrency::ROLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&rolData_, null_deleter());
    }

    RONCurrency::RONCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&ronData_, null_deleter());
    }

    SEKCurrency::SEKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&sekData_, null_deleter());
    }

    SITCurrency::SITCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&sitData_, null_deleter());
    }

    SKKCurrency::SKKCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&skkData_, null_deleter());
    }

    TRLCurrency::TRLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&trlData_, null_deleter());
    }

    TRYCurrency::TRYCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&tryData_, null_deleter());
    }

    ATSCurrency::ATSCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&atsData_, null_deleter());
    }

    BEFCurrency::BEFCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&befData_, null_deleter());
    }

    DEMCurrency::DEMCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&demData_, null_deleter());
    }

    ESPCurrency::ESPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&espData_, null_deleter());
    }

    FIMCurrency::FIMCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&fimData_, null_deleter());
    }

    FRFCurrency::FRFCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&frfData_, null_deleter());
    }

    GRDCurrency::GRDCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&grdData_, null_deleter());
    }

    IEPCurrency::IEPCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&iepData_, null_deleter());
    }

    ITLCurrency::ITLCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&itlData_, null_deleter());
    }

    LUFCurrency::LUFCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&lufData_, null_deleter());
    }

    NLGCurrency::NLGCurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&nlgData_, null_deleter());
    }

    PTECurrency::PTECurrency() {
        data_ = boost::shared_ptr<Currency::Data>(&pteData_, null_deleter());
    }

}

