
/*
 Copyright (C) 2005 Walter Penschke

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

#ifndef qla_zerocurve_hpp
#define qla_zerocurve_hpp

#include <oh/objhandler.hpp>
#include <ql/TermStructures/zerocurve.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_RATES                     "RATES"
#define FIELD_DATES                     "DATES"
#define IDX_RATES                       0
#define IDX_DATES                       1

namespace QuantLibAddin {

    class ZeroCurve : public ObjHandler::Object {
    public:
        ZeroCurve(ObjHandler::ArgStack& args);

        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(zeroCurve_);
        }

    private:
        boost::shared_ptr<QuantLib::ZeroCurve> zeroCurve_;
    };
}

#endif



