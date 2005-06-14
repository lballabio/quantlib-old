
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_simpleswap_hpp
#define qla_simpleswap_hpp

#include <oh/objhandler.hpp>
#include <ql/Instruments/simpleswap.hpp>

namespace QuantLibAddin {

    class SimpleSwap : public ObjHandler::Object {
    public:
        SimpleSwap(ObjHandler::ArgumentStack& args);

        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(swap_);
        }
		const QuantLib::SimpleSwap& getObject() const {return *swap_;}

    private:
        boost::shared_ptr<QuantLib::SimpleSwap> swap_;
    };
}

#endif

