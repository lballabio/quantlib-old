
/*
 Copyright (C) 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/ohthing.hpp>

namespace ObjHandler {

    std::string Thing::ohThingFunc0(const std::string &p0) {
        return p0;
    }

    double Thing::ohThingFunc1(const double &p0) {
        return p0;
    }

    std::string Thing::ohThingFunc2(const std::string &p0, const std::string &p1, const std::string &p2) {
        return p0 + " | " + p1 + " | " + p2;
    }

    std::string Thing::ohThingFunc3(const std::string &p0, const std::vector<std::string> &p1) {
        std::string ret = p0;
        for (std::vector<std::string>::const_iterator i = p1.begin(); i != p1.end(); i++) {
            ret += " | " + *i;
        }
        return ret;
    }

    std::vector<std::string> Thing::ohThingFunc4(const std::string &p0, const std::vector<std::string> &p1) {
        std::vector<std::string> ret;
        for (std::vector<std::string>::const_iterator i = p1.begin(); i != p1.end(); i++) {
            ret.push_back(*i + " | " + p0);
        }
        return ret;
    }

    std::string Thing::ohThingFunc5(const std::string &p0, const std::string &p1) {
        return p0 + " | " + p1;
    }

    std::vector<std::string> Thing::ohThingFunc6(const std::string &p0) {
        std::vector<std::string> ret;
        ret.push_back(p0);
        return ret;
    }


}

