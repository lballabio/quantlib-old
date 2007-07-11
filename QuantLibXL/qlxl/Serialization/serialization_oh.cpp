
/*  
 Copyright (C) 2007 Eric Ehlers
 
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

#include <qlxl/Serialization/serialization_oh.hpp>
#include <oh/ValueObjects/vo_range.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>

namespace QuantLibXL {

    void register_oh(boost::archive::xml_oarchive &ar) {
    
            ar.register_type<ObjectHandler::ValueObjects::ohRange>();
    
    }
    
    void register_oh(boost::archive::xml_iarchive &ar) {
    
            ar.register_type<ObjectHandler::ValueObjects::ohRange>();

    }
    
}

