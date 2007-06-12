
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Marco Bianchetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Giorgio Facchinetti

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/marketmodelevolvers.hpp>
#include <ql/models/marketmodels/evolvers/lognormalfwdratepc.hpp>
#include <ql/models/marketmodels/evolvers/lognormalfwdrateipc.hpp>
#include <ql/models/marketmodels/evolvers/normalfwdratepc.hpp>

namespace QuantLibAddin {
 
    LogNormalFwdRatePc::LogNormalFwdRatePc(
        const boost::shared_ptr<QuantLib::MarketModel>& pseudoRoot,
        const QuantLib::BrownianGeneratorFactory& generatorFactory,
        const std::vector<QuantLib::Size>& numeraires)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelEvolver>(
            new QuantLib::LogNormalFwdRatePc(pseudoRoot,
                                               generatorFactory,
                                               numeraires));
    }

    LogNormalFwdRateIpc::LogNormalFwdRateIpc(
        const boost::shared_ptr<QuantLib::MarketModel>& pseudoRoot,
        const QuantLib::BrownianGeneratorFactory& generatorFactory,
        const std::vector<QuantLib::Size>& numeraires)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelEvolver>(
            new QuantLib::LogNormalFwdRateIpc(pseudoRoot,
                                                generatorFactory,
                                                numeraires));
    }

    NormalFwdRatePc::NormalFwdRatePc(
        const boost::shared_ptr<QuantLib::MarketModel>& pseudoRoot,
        const QuantLib::BrownianGeneratorFactory& generatorFactory,
        const std::vector<QuantLib::Size>& numeraires)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::MarketModelEvolver>(
            new QuantLib::NormalFwdRatePc(pseudoRoot,
                                                generatorFactory,
                                                numeraires));
    }

}
