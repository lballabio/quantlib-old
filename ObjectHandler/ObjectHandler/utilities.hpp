
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#ifndef utilities_hpp
#define utilities_hpp

#include <ObjectHandler/object.hpp>

namespace ObjHandler {

    std::string toUpper(const std::string &s);
    std::ostream& operator<<(std::ostream& out, const any_ptr& a);
    int setLogFile(const std::string &newLogFileName);
    void logMessage(const std::string &msg);

}


#endif
