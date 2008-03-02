
/*  
 Copyright (C) 2007, 2008 Eric Ehlers
 
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

//This suppression seems no longer to be necessary?...
//#include <boost/config.hpp>
//#if defined BOOST_MSVC
//#pragma warning(disable : 4267)
//#endif

#include <qlo/serialization/register/serialization_oh.hpp>
#include <oh/valueobject.hpp>
//#include <oh/valueobjects/vo_group.hpp>
//#include <oh/valueobjects/vo_range.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>

/*
Register ObjectHandler classes with boost serialization framework ("BSF" below).

- At present four classes are registered, this value 4 is hard-coded
  into gensrc script gensrc/addins/serialization.py so if you add new classes
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

namespace QuantLibAddin {

    void register_oh(boost::archive::xml_oarchive &ar) {
    
        // class ID 0 in the boost serialization framework
        ar.register_type<boost::shared_ptr<ObjectHandler::ValueObject> >();
        // class ID 1 in the boost serialization framework
        ar.register_type<std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > >();
        // class ID 2 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohGroup>();
        // class ID 3 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohRange>();
    
    }
    
    void register_oh(boost::archive::xml_iarchive &ar) {
    
        // class ID 0 in the boost serialization framework
        ar.register_type<boost::shared_ptr<ObjectHandler::ValueObject> >();
        // class ID 1 in the boost serialization framework
        ar.register_type<std::vector<boost::shared_ptr<ObjectHandler::ValueObject> > >();
        // class ID 2 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohGroup>();
        // class ID 3 in the boost serialization framework
        //ar.register_type<ObjectHandler::ValueObjects::ohRange>();

    }
    
}

