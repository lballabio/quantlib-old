/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005, 2008 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif
#include <qlo/conversions/conversions.hpp>
#include <ql/utilities/dataparsers.hpp>
#include <ql/time/date.hpp>
#include <ql/time/period.hpp>
#include <ql/interestrate.hpp>
#include <ql/timeseries.hpp>
#include <qlo/timeseries.hpp>

namespace QuantLibAddin {

    double libraryToScalar(const QuantLib::InterestRate &i) {
        return i.rate();
    }

    double libraryToScalar(const QuantLib::Rate &r) {
        return r;
    }

    long libraryToScalar(const QuantLib::Date &d) {
        return d.serialNumber();
    }

    std::string libraryToScalar(const QuantLib::Period &period) {
        std::ostringstream s;
        s << period;
        return s.str();
    }

    std::vector<long> libraryToVector(const std::vector<QuantLib::Date> &v) {
        std::vector<long> ret;
        ret.reserve(v.size());
        for (std::vector<QuantLib::Date>::const_iterator i = v.begin(); i != v.end(); ++i)
            ret.push_back(i->serialNumber());
        return ret;
    }

    std::vector<std::string> libraryToVector(const std::vector<QuantLib::Period> &v) {
        std::vector<std::string> ret;
        ret.reserve(v.size());
        for (std::vector<QuantLib::Period>::const_iterator i = v.begin(); i != v.end(); ++i)
            ret.push_back(libraryToScalar(*i));
        return ret;
    }

    std::vector<double> libraryToVector(const std::vector<QuantLib::Real> &v) {
        std::vector<double> ret;
        ret.reserve(v.size());
        for (std::vector<QuantLib::Real>::const_iterator i = v.begin(); i != v.end(); ++i)
            ret.push_back(*i);
        return ret;
    }

    void cppToLibrary(const std::string &in, QuantLib::Period &ret) {
        ret = QuantLib::PeriodParser::parse(in);
        ret.normalize();
    }

    void cppToLibrary(const long &in, QuantLib::Size &ret) {
        ret = QuantLib::Size(in);
    }

#if defined(__GNUC__) && defined(__x86_64__)
    void cppToLibrary(const long &in, QuantLib::Natural &ret) {
        ret = QuantLib::Natural(in);
    }
#endif

    QuantLib::Matrix vvToQlMatrix(const std::vector<std::vector<double> > &vv) {
        int rows = vv.size();
        int cols = rows ? vv[0].size() : 0;
        QuantLib::Matrix m(rows, cols);
        for (int i=0; i<rows; ++i)
            for (int j=0; j<cols; ++j)
                m[i][j] = vv[i][j];
        return m;
    }

     std::vector<std::vector<double> > qlMatrixToVv(const QuantLib::Matrix &m) {
        std::vector<std::vector<double> > vv;
        for(unsigned int r=0; r<m.rows(); ++r) {
            std::vector<double> v;
            for(unsigned int c=0; c<m.columns(); ++c) {
                v.push_back(m[r][c]);
            }
            vv.push_back(v);
        }
        return vv;
    }

}
#include <oh/objecthandler.hpp>
#include <qlo/conversions/coercehandle.hpp>
#include <qlo/conversions/coerceobject.hpp>
#include <qlo/conversions/varianttoquotehandle.hpp>
#include <qlo/conversions/varianttodate.hpp>
#include <qlo/conversions/varianttoquote.hpp>
#include <qlo/conversions/varianttoperiod.hpp>
#include <qlo/conversions/varianttosize.hpp>
#include <qlo/conversions/conversion_tmpl.hpp>

namespace ObjectHandler {

    template<> 
    QuantLib::Date convert2<QuantLib::Date, property_t>(const property_t& c) {
        return convertDate(c);
    }

    template<> 
    QuantLib::Period convert2<QuantLib::Period, property_t>(const property_t& c) {
        return convertPeriod(c);
    }

    template<> 
    QuantLib::Size convert2<QuantLib::Size, property_t>(const property_t& p) { 
        return convertSize(p); 
    }

    template<>
    boost::shared_ptr<QuantLib::Quote> convert2<boost::shared_ptr<QuantLib::Quote>, property_t>(const property_t& c) {
        return convertQuote(c);
    }

    template<>
    QuantLib::Handle<QuantLib::Quote> convert2<QuantLib::Handle<QuantLib::Quote>, property_t>(const property_t& c) {
        return convertQH(c);
    }

    template<> 
    QuantLib::TimeSeriesDef convert2<QuantLib::TimeSeriesDef, property_t>(const property_t& c) {
        return convertTimeSeriesDef(c);
    }

    /*
    template<> 
    QuantLib::Date convert2<QuantLib::Date, ConvertOper>(const ConvertOper& c) {
        return convertDate(c);
    }

    template<> 
    QuantLib::Period convert2<QuantLib::Period, ConvertOper>(const ConvertOper& c) {
        return convertPeriod(c);
    }
    
    template<> 
    QuantLib::Size convert2<QuantLib::Size, ConvertOper>(const ConvertOper& p) {
        return convertSize(p); 
    }

    template<>
    QuantLib::Handle<QuantLib::Quote> convert2<QuantLib::Handle<QuantLib::Quote>, ConvertOper>(const ConvertOper& c) {
        return convertQH(c);
    }
    */

}
