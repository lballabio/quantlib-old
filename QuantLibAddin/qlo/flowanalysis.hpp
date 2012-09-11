/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2011 Ferdinando Ametrano
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_analysis_hpp
#define qla_analysis_hpp

#include <boost/shared_ptr.hpp>
#include <vector>
#include <oh/property.hpp>

namespace QuantLib {
    class CashFlow;
    class Date;
    typedef std::vector<boost::shared_ptr<CashFlow> > Leg;
}

namespace QuantLibAddin {

    std::vector<std::vector<ObjectHandler::property_t> >
    flowAnalysis(const QuantLib::Leg& leg,
                 const QuantLib::Date& d);

}

#endif
