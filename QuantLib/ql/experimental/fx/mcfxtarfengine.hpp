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

/*! \file mcfxtarfengine.hpp
    \brief Monte Carlo engine for FX Tarf
*/

#ifndef quantlib_pricingengines_mc_fxtarf_hpp
#define quantlib_pricingengines_mc_fxtarf_hpp

#include <ql/processes/blackscholesprocess.hpp>
#include <ql/experimental/fx/fxtarf.hpp>
#include <ql/experimental/fx/proxyengine.hpp>
#include <boost/make_shared.hpp>

namespace QuantLib {

class McFxTarfEngine : public FxTarf::engine {
  public:
    McFxTarfEngine(
        const boost::shared_ptr<GeneralizedBlackScholesProcess> &process)
        : process_(process) {
        registerWith(process_);
    }

    void calculate() const;

    boost::shared_ptr<ProxyDescription> proxy() const {
        return boost::make_shared<FxTarf::Proxy>();
    }

  private:
    boost::shared_ptr<GeneralizedBlackScholesProcess> process_;
};

} // namespace QuantLib

#endif
