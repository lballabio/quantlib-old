
/*
 Copyright (C) 2006 Ferdinando Ametrano

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
#include <qlo/statistics.hpp>

namespace QuantLibAddin {

    Statistics::Statistics(std::vector<QuantLib::Real> values, 
                           std::vector<QuantLib::Real> weights)
        {               
            libraryObject_ = boost::shared_ptr<QuantLib::Statistics>(
                new QuantLib::Statistics());

            QL_REQUIRE(weights.empty() || values.size()==weights.size(),
                "Values and weights vectors must have the same number of elements.");

            if (!values.empty()) {
                if (!weights.empty())
                    libraryObject_->addSequence(values.begin(),
                                                values.end(),
                                                weights.begin());
                else
                    libraryObject_->addSequence(values.begin(),
                                                values.end());
            }
        }

}
