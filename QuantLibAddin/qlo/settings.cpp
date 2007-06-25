
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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

#include <qlo/settings.hpp>

#include <ql/settings.hpp>

namespace QuantLibAddin {

    void qlSettingsSetEvaluationDate(const QuantLib::Date& evalDate) {
        QuantLib::Settings::instance().evaluationDate() = evalDate;
    }

    void qlSettingsSetEnforceTodaysHistoricFixings(bool b) {
        QuantLib::Settings::instance().enforcesTodaysHistoricFixings() = b;
    }
}
