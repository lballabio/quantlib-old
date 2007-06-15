
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file
    \brief A conversion function to write a value of type boost::any to a stream
*/

#ifndef oh_anytostream_hpp
#define oh_anytostream_hpp

#include <oh/variant.hpp>

namespace ObjectHandler {

    //! Helper template to simplify the syntax of the function below.
    template <class T>
    std::ostream& anytostream(std::ostream& out, const boost::any& any) {
        return out << boost::any_cast<T>(any);
    }

    //! Inspect the type of the boost::any value and output it to the stream.
    inline std::ostream& operator<<(std::ostream& out, const boost::any& any) {
        if (any.empty())
            return out << "null";
        //else if (any.type() == typeid(int))
        //    return out << boost::any_cast<int>(any);
        else if (any.type() == typeid(long))
            return anytostream<long>(out, any);
        else if (any.type() == typeid(double))
            return anytostream<double>(out, any);
        else if (any.type() == typeid(bool))
            return anytostream<bool>(out, any);
        else if (any.type() == typeid(std::string))
            return anytostream<std::string>(out, any);
        else if (any.type() == typeid(Variant))
            return anytostream<Variant>(out, any);
        else if (any.type() == typeid(std::vector<long>))
            return anytostream<std::vector<long> >(out, any);
        else if (any.type() == typeid(std::vector<double>))
            return anytostream<std::vector<double> >(out, any);
        else if (any.type() == typeid(std::vector<bool>))
            return anytostream<std::vector<bool> >(out, any);
        else if (any.type() == typeid(std::vector<std::string>))
            return anytostream<std::vector<std::string> >(out, any);
        //else if (any.type() == typeid(std::vector<boost::any>))
            //return anytostream<std::vector<boost::any> >(out, any);
        else if (any.type() == typeid(std::vector<Variant>))
            return anytostream<std::vector<Variant> >(out, any);
        else if (any.type() == typeid(std::vector<std::vector<long> >))
            return anytostream<std::vector<std::vector<long> > >(out, any);
        else if (any.type() == typeid(std::vector<std::vector<double> >))
            return anytostream<std::vector<std::vector<double> > >(out, any);
        else if (any.type() == typeid(std::vector<std::vector<bool> >))
            return anytostream<std::vector<std::vector<bool> > >(out, any);
        else if (any.type() == typeid(std::vector<std::vector<std::string> >))
            return anytostream<std::vector<std::vector<std::string> > >(out, any);
        //else if (any.type() == typeid(std::vector<std::vector<boost::any> >))
            //return anytostream<std::vector<std::vector<boost::any> > >(out, any);
        else if (any.type() == typeid(std::vector<std::vector<Variant> >))
            return anytostream<std::vector<std::vector<Variant> > >(out, any);
        else
            return out << "unrecognized type";
    }

}

#endif

