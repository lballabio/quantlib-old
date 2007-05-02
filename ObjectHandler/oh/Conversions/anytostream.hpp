
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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

/*! \file
    \brief A conversion function to write a value of type boost::any to a stream.
*/

#ifndef oh_anytostream_hpp
#define oh_anytostream_hpp

namespace ObjectHandler {

    //! Output a vector of values to the stream.
    template <typename T>
    std::ostream& operator<<(std::ostream &out, const std::vector<T> &v) {
        typename std::vector<T>::const_iterator i;
        for (i = v.begin(); i != v.end(); i++) {
            out << *i << " ";
        }
        return out;
    }

    //! Output a matrix of values to the stream.
    template <typename T>
    std::ostream& operator<<(std::ostream &out, const std::vector<std::vector<T> > &vv) {
        typename std::vector<std::vector<T> >::const_iterator i;
        for (i = vv.begin(); i != vv.end(); i++) {
            out << *i << std::endl;
        }
        return out;
    }

    //! Inspect the type of the boost::any value and output it to the stream.
    inline std::ostream& operator<<(std::ostream& out, const boost::any& any) {
        if (any.empty())
            return out << "null";
        else if (any.type() == typeid(int))
            return out << boost::any_cast<int>(any);
        else if (any.type() == typeid(long))
            return out << boost::any_cast<long>(any);
        else if (any.type() == typeid(double))
            return out << boost::any_cast<double>(any);
        else if (any.type() == typeid(bool))
            return out << boost::any_cast<bool>(any);
        else if (any.type() == typeid(std::string))
            return out << boost::any_cast<std::string>(any);
        else if (any.type() == typeid(std::vector<long>)) {
            std::vector<long> v= boost::any_cast< std::vector<long> >(any);
            return out << v;
        } else if (any.type() == typeid(std::vector<double>)) {
            std::vector<double> v= boost::any_cast< std::vector<double> >(any);
            return out << v;
        } else if (any.type() == typeid(std::vector<bool>)) {
            std::vector<bool> v= boost::any_cast< std::vector<bool> >(any);
            return out << v;
        } else if (any.type() == typeid(std::vector<std::string>)) {
            std::vector<std::string> v= boost::any_cast< std::vector<std::string> >(any);
            return out << v;
        } else if (any.type() == typeid(std::vector<boost::any>)) {
            std::vector<boost::any> v= boost::any_cast< std::vector<boost::any> >(any);
            return out << v;
        } else if (any.type() == typeid(std::vector<std::vector<long> >)) {
            std::vector<std::vector<long> > vv = 
                boost::any_cast< std::vector<std::vector<long> > >(any);
            return out << vv;
        } else if (any.type() == typeid(std::vector<std::vector<double> >)) {
            std::vector<std::vector<double> > vv = 
                boost::any_cast< std::vector<std::vector<double> > >(any);
            return out << vv;
        } else if (any.type() == typeid(std::vector<std::vector<bool> >)) {
            std::vector<std::vector<bool> > vv = 
                boost::any_cast< std::vector<std::vector<bool> > >(any);
            return out << vv;
        } else if (any.type() == typeid(std::vector<std::vector<std::string> >)) {
            std::vector<std::vector<std::string> > vv = 
                boost::any_cast< std::vector<std::vector<std::string> > >(any);
            return out << vv;
        } else if (any.type() == typeid(std::vector<std::vector<boost::any> >)) {
            std::vector<std::vector<boost::any> > vv = 
                boost::any_cast< std::vector<std::vector<boost::any> > >(any);
            return out << vv;
        } else {
            return out << "unrecognized type";
        }
    }

}

#endif

