
/*
 Copyright (C) 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ohxl/configuration.hpp>
#include <oh/exception.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/xloper.hpp>

namespace ObjectHandler {

    void Configuration::init() {
        Xloper xWorkspace;
        Excel(xlfGetWorkspace, &xWorkspace, 1, TempInt(37));
        rowCharacter_ = xWorkspace->val.array.lparray[5].val.str[1];
        colCharacter_ = xWorkspace->val.array.lparray[6].val.str[1];
        initialized_ = true;
    }
    const char &Configuration::rowCharacter() {
        OH_REQUIRE(initialized_, "Configuration not initialized");
        return rowCharacter_; 
    }

    const char &Configuration::colCharacter() { 
        OH_REQUIRE(initialized_, "Configuration not initialized");
        return colCharacter_; 
    }

}

