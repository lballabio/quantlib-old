
/*
 Copyright (C) 2007 Marco Bianchetti

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/stickyratchet.hpp>


namespace QuantLibAddin {

    DoubleStickyRatchetPayoff::DoubleStickyRatchetPayoff(
            const QuantLib::Real type1,
            const QuantLib::Real type2,
            const QuantLib::Real gearing1,
            const QuantLib::Real gearing2,
            const QuantLib::Real gearing3,
            const QuantLib::Spread spread1,
            const QuantLib::Spread spread2,
            const QuantLib::Spread spread3,
            const QuantLib::Real initialValue1,
            const QuantLib::Real initialValue2,
            const QuantLib::Real accrualFactor) {
        libraryObject_ = boost::shared_ptr<QuantLib::DoubleStickyRatchetPayoff>(
        new QuantLib::DoubleStickyRatchetPayoff(
            type1,
            type2,
            gearing1,
            gearing2,
            gearing3,
            spread1,
            spread2,
            spread3,
            initialValue1,
            initialValue2,
            accrualFactor));    
        }

    RatchetPayoff::RatchetPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Real initialValue,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::RatchetPayoff>(
            new QuantLib::RatchetPayoff(
              gearing1,
              gearing2,
              spread1,
              spread2,
              initialValue,
              accrualFactor));
            }

    StickyPayoff::StickyPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Real initialValue,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::StickyPayoff>(
            new QuantLib::StickyPayoff(
              gearing1,
              gearing2,
              spread1,
              spread2,
              initialValue,
              accrualFactor));
            }

    RatchetMaxPayoff::RatchetMaxPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::RatchetMaxPayoff>(
            new QuantLib::RatchetMaxPayoff(
              gearing1,
              gearing2,
              gearing3,
              spread1,
              spread2,
              spread3,
              initialValue1,
              initialValue2,
              accrualFactor));
            }

    RatchetMinPayoff::RatchetMinPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::RatchetMinPayoff>(
            new QuantLib::RatchetMinPayoff(
              gearing1,
              gearing2,
              gearing3,
              spread1,
              spread2,
              spread3,
              initialValue1,
              initialValue2,
              accrualFactor));
            }

    StickyMaxPayoff::StickyMaxPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::StickyMaxPayoff>(
            new QuantLib::StickyMaxPayoff(
              gearing1,
              gearing2,
              gearing3,
              spread1,
              spread2,
              spread3,
              initialValue1,
              initialValue2,
              accrualFactor));
            }

    StickyMinPayoff::StickyMinPayoff(
              const QuantLib::Real gearing1,
              const QuantLib::Real gearing2,
              const QuantLib::Real gearing3,
              const QuantLib::Spread spread1,
              const QuantLib::Spread spread2,
              const QuantLib::Spread spread3,
              const QuantLib::Real initialValue1,
              const QuantLib::Real initialValue2,
              const QuantLib::Real accrualFactor) {
            libraryObject_ = boost::shared_ptr<QuantLib::StickyMinPayoff>(
            new QuantLib::StickyMinPayoff(
              gearing1,
              gearing2,
              gearing3,
              spread1,
              spread2,
              spread3,
              initialValue1,
              initialValue2,
              accrualFactor));
            }
}
