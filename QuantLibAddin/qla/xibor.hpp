
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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

#ifndef qla_xibor_hpp
#define qla_xibor_hpp

#include <oh/objhandler.hpp>
#include <ql/Indexes/xibor.hpp>

namespace QuantLibAddin {
    class Xibor : public ObjHandler::Object {
    public:
        Xibor(
            const std::string &indexName,
            const std::string &crrID,
            const long &tenor,
            const std::string &timeUnitsID,
            const QuantLib::Calendar& calendar,
            const std::string &fltBDCID,
            const QuantLib::DayCounter &fltDayCounter,
            const long &fixingDays,
            const std::string &fwdCurveId,
            const std::vector<long> &lDates,
            const std::vector<double> &fixings);

        const QuantLib::Xibor& getObject() const {return *index_;}

        double fixing(const long &lFixingDate) const;

        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(index_);
        }

    private:
        boost::shared_ptr<QuantLib::Xibor> index_;
    };
}

#endif

