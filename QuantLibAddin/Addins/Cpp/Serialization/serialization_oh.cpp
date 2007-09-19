
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

#include <boost/config.hpp>
#if defined BOOST_MSVC
#pragma warning(disable : 4267)
#endif

#include <Addins/Cpp/Serialization/serialization_oh.hpp>
//#include <oh/ValueObjects/vo_group.hpp>
#include <oh/ValueObjects/vo_range.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>

/*
Register ObjectHandler classes with boost serialization framework ("BSF" below).

- This source file appears twice in the project:
    QuantLibXL/qlxl/Serialization/serialization_oh.cpp
    QuantLibAddin/Addins/Cpp/Serialization/serialization_oh.cpp
  The reason that this source code is duplicated within each Addin rather than
  being centralized within ObjectHandler is that certain BSF functionality
  cannot be split across dynamically linked binaries.

- At present three classes are registered, this value 3 is hard-coded
  into gensrc script gensrc/Addins/serialization.py so if you add new classes
  here you also need to change the script.  gensrc uses the value to keep
  track of the IDs which the BSF assigns to each addin class.

- Here we explicitly register
    boost::shared_ptr<ObjectHandler::ValueObject>
    std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >
  This isn't strictly necessary because if we neglect to register these classes
  then the BSF will do so automatically on our behalf the first time we attempt
  to serialize them.  We register them explicitly in order to retain control
  over all ID numbers assigned by the BSF to our classes.
*/

namespace QuantLibAddinCpp {

    void register_oh(boost::archive::xml_oarchive &ar) {
    
        // class ID 0 in the boost serialization framework
        ar.register_type<boost::shared_ptr<ObjectHandler::ValueObject> >();
        // class ID 1 in the boost serialization framework
        ar.register_type<std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > >();
        // class ID 2 in the boost serialization framework
        ar.register_type<ObjectHandler::ValueObjects::ohRange>();
        // class ID 3 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohGroup>();
    
    }
    
    void register_oh(boost::archive::xml_iarchive &ar) {
    
        // class ID 0 in the boost serialization framework
        ar.register_type<boost::shared_ptr<ObjectHandler::ValueObject> >();
        // class ID 1 in the boost serialization framework
        ar.register_type<std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > >();
        // class ID 2 in the boost serialization framework
        ar.register_type<ObjectHandler::ValueObjects::ohRange>();
        // class ID 3 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohGroup>();

    }
    
}

