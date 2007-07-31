
/*
 Copyright (C) 2007 Eric Ehlers

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

#include <Clients/C++/set_fixings.hpp>
#include <Addins/C++/addincpp.hpp>

#include <iostream>
using std::cout;
using std::endl;

using namespace QuantLibAddinCpp;

template <class T>
std::vector<T> extractVector(const std::string &objectID) {
    std::vector<std::vector<double> > valueMatrix = 
        boost::any_cast<std::vector<std::vector<double> > >(ohPropertyValue2(objectID, "values"));
    std::vector<T> ret;
    for (std::vector<std::vector<double> >::const_iterator i = valueMatrix.begin(); i!=valueMatrix.end(); i++)
        ret.push_back((*i)[0]);
    return ret;
}

void addFixings(const std::vector<ObjectHandler::Variant> &timeStamps, const std::string &fixingsID, const std::string &indexID) {
    std::vector<double> fixings = extractVector<double>(fixingsID);
    qlIndexAddFixings(indexID, timeStamps, fixings);
}

void setFixings() {
    std::vector<ObjectHandler::Variant> timeStamps = extractVector<ObjectHandler::Variant>("timestamps");
    addFixings(timeStamps, "EURSFIXA5Y", "EuriborSwapFixA5Y");
    addFixings(timeStamps, "EURIBOR6MD", "EURIBOR6M");
}

