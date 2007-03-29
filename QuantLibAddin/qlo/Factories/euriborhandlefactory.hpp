
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_euriborhandlefactory_hpp
#define qla_euriborhandlefactory_hpp

#include <qlo/typefactory.hpp>

namespace QuantLibAddin {

     // a singleton to store the Handle<YieldTermStructure>
    // shared by all enumerated Euribor classes
    class EuriborHandle : public QuantLib::Singleton<EuriborHandle> {
        friend class QuantLib::Singleton<EuriborHandle>;
    public:
        const QuantLib::Handle<QuantLib::YieldTermStructure> &handleYieldTermStructure() const {
            return handleYieldTermStructure_;
        }
        void linkEuriborHandle(boost::shared_ptr<QuantLib::YieldTermStructure> yieldTermStructure) {
            handleYieldTermStructure_.linkTo(yieldTermStructure);
        }
    private:
        QuantLib::RelinkableHandle<QuantLib::YieldTermStructure> handleYieldTermStructure_;
    };
   
 }

#endif

