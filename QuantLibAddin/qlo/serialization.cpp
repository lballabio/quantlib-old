
/*
 Copyright (C) 2007 François du Vignaud

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


#include <qlo/serialization.hpp>
#include <fstream>
#  pragma warning(disable: 4996)
#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/serialization/vector.hpp>

using std::vector;
using std::string;
using QuantLib::Real;

namespace QuantLibAddin {
    // some generic code...
    namespace {
        template <class T>
        void save_serializable(const T &s, const std::string& filename)
        {
            std::ofstream ofs(filename.c_str());
            assert(ofs.good());
            boost::archive::xml_oarchive oa(ofs);
            oa << BOOST_SERIALIZATION_NVP(s);
        }

        template <class T>
        void restore_serializable(T &s, const std::string& filename)
        {
            std::ifstream ifs(filename.c_str());
            assert(ifs.good());
            boost::archive::xml_iarchive ia(ifs);
            ia >> BOOST_SERIALIZATION_NVP(s);
        }
    }

    // serializing function definitions
    bool serializeNumericVector(const std::vector<Real> data, 
                               const std::string& path)
    {
        save_serializable(data, path);
        return true;
    }

     bool serializeNumericMatrix(const std::vector<std::vector<Real>> data,  
                                const std::string& path)  {
        save_serializable(data, path);
        return true;
    }

    // deserializing function definitions
    std::vector<Real> restoreNumericVector(const std::string& path)
    {
        std::vector<Real> data;
        restore_serializable(data, path);
        return data;
    }
    
    std::vector<std::vector<Real>> restoreNumericMatrix(const std::string& path)
    {
        std::vector<std::vector<Real>> data;
        restore_serializable(data, path);
        return data;
    }
}
