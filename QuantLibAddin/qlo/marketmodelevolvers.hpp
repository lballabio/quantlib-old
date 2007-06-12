
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano
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

#ifndef qla_marketmodelevolvers_hpp
#define qla_marketmodelevolvers_hpp

#include <oh/objecthandler.hpp>
#include <ql/models/marketmodels/evolver.hpp>
#include <ql/models/marketmodels/marketmodel.hpp>
#include <ql/models/marketmodels/browniangenerator.hpp>

namespace QuantLibAddin {
    
    class MarketModelEvolver : public ObjectHandler::LibraryObject<
        QuantLib::MarketModelEvolver> {
    };

    class LogNormalFwdRatePc : public MarketModelEvolver {
    public:
        LogNormalFwdRatePc(const boost::shared_ptr<QuantLib::MarketModel>&,
                             const QuantLib::BrownianGeneratorFactory&,
                             const std::vector<QuantLib::Size>& numeraires);
    };

    class LogNormalFwdRateIpc : public MarketModelEvolver {
     public:
        LogNormalFwdRateIpc(const boost::shared_ptr<QuantLib::MarketModel>&,
                              const QuantLib::BrownianGeneratorFactory&,
                              const std::vector<QuantLib::Size>& numeraires);
    };
    class NormalFwdRatePc : public MarketModelEvolver {
     public:
    NormalFwdRatePc(const boost::shared_ptr<QuantLib::MarketModel>&,
                              const QuantLib::BrownianGeneratorFactory&,
                              const std::vector<QuantLib::Size>& numeraires);
    };

}

#endif
