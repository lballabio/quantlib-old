
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <oh/config.hpp>
#endif
#include <oh/objhandlerdefines.hpp>
#include <oh/objecthandler.hpp>
#include <oh/utilities.hpp>
#include <oh/exception.hpp>
#include <oh/logger.hpp>

#include <iomanip>

namespace ObjHandler {

    std::ostream& operator<<(std::ostream& out, const any_ptr& a) {
        if (a->empty())
            return out << "null";
        else if (a->type() == typeid(int))
            return out << boost::any_cast<int>(*a);
        else if (a->type() == typeid(double))
            return out << boost::any_cast<double>(*a);
        else if (a->type() == typeid(std::string))
            return out << boost::any_cast<std::string>(*a);
        else if (a->type() == typeid(std::vector<long>)) {
            out << std::endl;
            std::vector<long> v= boost::any_cast< std::vector<long> >(*a);
            for (std::vector<long>::const_iterator i = v.begin();
            i != v.end(); i++) {
                out << *i << std::endl;
            }
            return out;       
        } else if (a->type() == typeid(std::vector<double>)) {
            out << std::endl;
            std::vector<double> v= boost::any_cast< std::vector<double> >(*a);
            for (std::vector<double>::const_iterator i = v.begin();
            i != v.end(); i++) {
                out << *i << std::endl;
            }
            return out;       
        } else
            throw Exception("any_ptr << operator: unrecognized type");
    }

    std::ostream& operator<<(std::ostream& out, const Properties &p) {
        out << std::endl;
        Properties::const_iterator it;
        for (it = p.begin(); it != p.end(); it++) {
            ObjectProperty property = *it;
            out << std::left << "property = " << std::setw(10) << property.name() <<
                " value = " << property() << std::endl;
        } 
        return out;
    }

    void logObject(const std::string &handle) {
        std::ostringstream msg;
        obj_ptr object = 
            ObjectHandler::instance().retrieveObject(handle);
        if (!object) {
            msg << "no object in repository with handle = " << handle << std::endl;
        } else {
            msg << "log dump of object with handle = " << handle << std::endl;
            msg << object->getProperties();
        }
        Logger::instance().logMessage(msg.str());
    }

    void logAllObjects() {
        std::vector < std::string > handles = 
            ObjectHandler::instance().getHandles();
        for (std::vector < std::string >::const_iterator i = handles.begin();
                i!=handles.end(); i++)
            logObject(*i);
    }
    
}

