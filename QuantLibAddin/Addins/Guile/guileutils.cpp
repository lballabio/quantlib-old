
/*
 Copyright (C) 2005, 2006 Eric Ehlers
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

#include <Addins/Guile/guileutils.hpp>

SCM anyToPairValue(const boost::any& a)
{
    SCM value;
    if (a.type() == typeid(int)) {
        value = gh_int2scm(boost::any_cast<int>(a));
    } else if (a.type() == typeid(long)) {
        value = gh_long2scm(boost::any_cast<long>(a));
    } else if (a.type() == typeid(double)) {
        value = gh_double2scm(boost::any_cast<double>(a));
    } else if (a.type() == typeid(bool)) {
        value = gh_bool2scm(boost::any_cast<bool>(a));
    } else if (a.type() == typeid(std::string)) {
        value = gh_str02scm(boost::any_cast<std::string>(a).c_str());
    } else if (a.type() == typeid(std::vector<long>)) {
        std::vector<long> v = boost::any_cast< std::vector<long> >(a);
        value = SCM_EOL;
        for (std::size_t i=v.size() ; --i != std::size_t(-1) ; )
            value = gh_cons(gh_long2scm(v[i]), value);
    } else if (a.type() == typeid(std::vector<double>)) {
        std::vector<double> v = boost::any_cast< std::vector<double> >(a);
        value = SCM_EOL;
        for (std::size_t i=v.size() ; --i != std::size_t(-1) ; )
            value = gh_cons(gh_double2scm(v[i]), value);
    } else {
        throw ObjHandler::Exception("anyToPairValue: unrecognized type");
    }
    return value;
}

/*
SCM anyToDottedPair(const ObjHandler::ObjectProperty& property)
{
    SCM value = anyToPairValue(*property());
    SCM label = gh_str02scm(property.name().c_str());
    return gh_cons(label, value);
}

SCM propertiesToAList(const ObjHandler::Properties& properties)
{
    SCM rtn = SCM_EOL;
    for (std::size_t i = properties.size() ; --i != std::size_t(-1) ; ) {
        SCM dottedPair = anyToDottedPair(properties[i]);
        rtn = gh_cons(dottedPair, rtn);
    }
    return rtn;
}
*/

