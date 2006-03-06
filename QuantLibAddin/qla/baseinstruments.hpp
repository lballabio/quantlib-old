
/*
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

#ifndef qla_baseinstruments_hpp
#define qla_baseinstruments_hpp

#include <oh/objhandler.hpp>
#include <ql/instrument.hpp>
#include <ql/Instruments/bond.hpp>

#define EXPORT_QL_OBJECT(CLASS) \
    const CLASS& getObject() const { \
        return *boost::dynamic_pointer_cast<CLASS>(mInstrument); \
    } \
	//

namespace QuantLibAddin {
    class Instrument : public ObjHandler::Object {
    public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(mInstrument);
        }

        const QuantLib::Instrument& getObject() const { 
            return *mInstrument; 
        }

    protected:
        boost::shared_ptr<QuantLib::Instrument> mInstrument;
    };

    class Bond : public Instrument {
    public:
        EXPORT_QL_OBJECT(QuantLib::Bond)
    };
}

#endif

