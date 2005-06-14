
/*
 Copyright (C) 2005 Plamen Neykov
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

#ifndef qla_fixedcouponbond_hpp
#define qla_fixedcouponbond_hpp

#include <oh/objhandler.hpp>
#include <ql/Instruments/fixedcouponbond.hpp>
#include <qla/enumfactory.hpp>

namespace QuantLibAddin {

    class FixedCouponBond : public ObjHandler::Object {
    public:
        FixedCouponBond(ObjHandler::ArgumentStack& args);

        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(myFixedCouponBond);
        }
        const QuantLib::FixedCouponBond& getObject() const {return *myFixedCouponBond;}

        double theoreticalYield(const std::string &compounding) const { 
            return myFixedCouponBond->yield(CREATE_ENUM(QuantLib::Compounding, compounding));
        }
        double cleanPrice(double yield, const std::string &compounding, long settlementDate) const;
        double dirtyPrice(double yield, const std::string &compounding, long settlementDate) const;
        double yield(double cleanPrice, const std::string &compounding, long settlementDate) const;

        double accruedAmount(long  d) const;

    private:
        boost::shared_ptr<QuantLib::FixedCouponBond> myFixedCouponBond;
    };
}

#endif



