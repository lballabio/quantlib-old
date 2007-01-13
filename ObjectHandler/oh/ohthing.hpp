
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef oh_ohthing_hpp
#define oh_ohthing_hpp

#include <oh/object.hpp>

namespace ObjHandler {

    class Thing : public Object {
      public:
        Thing(const std::string& stringParam,
              double doubleParam)
        : stringParameter_(stringParam), doubleParameter_(doubleParam) {};
        const std::string& stringParameter() { return stringParameter_; }
        const std::string& setStringParameter(const std::string& stringPar) {
            stringParameter_ = stringPar;
            return stringParameter_;
        }
        double doubleParameter() { return doubleParameter_; }
        double setDoubleParameter(double doubleParam) {
            doubleParameter_ = doubleParam;
            return doubleParameter_;
        }

        std::string ohThingFunc0(const std::string &);
        double ohThingFunc1(const double &);
        std::string ohThingFunc2(const std::string &, const std::string &, const std::string &);
        std::string ohThingFunc3(const std::string &, const std::vector<std::string> &);
        std::vector<std::string> ohThingFunc4(const std::string &, const std::vector<std::string> &);
        std::string ohThingFunc5(const std::string &, const std::string &);
        std::vector<std::string> ohThingFunc6(const std::string &);
      private:
        std::string stringParameter_;
        double doubleParameter_;
    };
}

#endif
