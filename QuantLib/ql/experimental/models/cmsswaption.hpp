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

/*! \file cmsswaption.hpp
    \brief cms swaption class
*/

#ifndef quantlib_instruments_cmsswaption_hpp
#define quantlib_instruments_cmsswaption_hpp

#include <ql/option.hpp>
#include <ql/experimental/models/cmsswap.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

    //! cms swaption class
    /*! \ingroup instruments
    */

    class CmsSwaption : public Option {
      public:
        class arguments;
        class engine;
        CmsSwaption(const boost::shared_ptr<CmsSwap>& swap, const boost::shared_ptr<Exercise>& exercise);
        //! \name Instrument interface
        //@{
        bool isExpired() const;
        void setupArguments(PricingEngine::arguments*) const;
        //@}
        //! \name Inspectors
        //@{
        VanillaSwap::Type type() const { return swap_->type(); }
        const boost::shared_ptr<CmsSwap>& underlyingSwap() const {
            return swap_;
        }
        //@}
      private:
        // arguments
        boost::shared_ptr<CmsSwap> swap_;
    };

    //! %Arguments for cms swaption calculation
    class CmsSwaption::arguments : public CmsSwap::arguments,
                                public Option::arguments {
      public:
        arguments() {}
        boost::shared_ptr<CmsSwap> swap;
        void validate() const;
    };

    //! base class for cms swaption engines
    class CmsSwaption::engine
        : public GenericEngine<CmsSwaption::arguments, CmsSwaption::results> {};

}

#endif
