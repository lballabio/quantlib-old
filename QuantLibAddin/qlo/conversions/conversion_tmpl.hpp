/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Plamen Neykov
 Copyright (C) 2010, 2011 Eric Ehlers

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

/*! \file
    \brief template conversion functions for QuantLib data types - used to convert from property_t or OPER to an C++ data type
    Not to be included directly by client code - instead the template functions in oh/Conversions/convert2.hpp should be used.
*/

#ifndef qlo_Conversions_conversion_tmpl_hpp
#define qlo_Conversions_conversion_tmpl_hpp

namespace ObjectHandler {

    inline bool is_numeric(const std::string &s, double &d) {
        try {
            d = boost::lexical_cast<double>(s);
            return true;
        } catch(...) {
            return false;
        }
    }

    template<class container_t>
    QuantLib::Date convertDate(const container_t& c) {
        if(c.type() == typeid(long))
            return QuantLib::Date(c.operator long());
        else if(c.type() == typeid(double))
            return QuantLib::Date(static_cast<QuantLib::BigInteger>(c.operator double()));
        else if(c.type() == typeid(std::string)) {
            std::string s = c.operator std::string();
            double d;
            if (is_numeric(s, d))
                return QuantLib::Date(static_cast<QuantLib::BigInteger>(d));
            if (QuantLib::IMM::isIMMcode(s, false))
                return QuantLib::IMM::date(s);
            QuantLib::Period p = QuantLib::PeriodParser::parse(s);
            QuantLib::Date d2 = QuantLib::Settings::instance().evaluationDate();
            return d2 + p;
        }
        else {
            OH_FAIL("unable to convert type '" << c.type().name() << "' to type 'QuantLib::Date'");
        }
    }

    template <class container_t>
    QuantLib::Size convertSize(const container_t& c) {
        return c.operator long();
    }
    
    template<class container_t>
    QuantLib::Period convertPeriod(const container_t& c) {
        return QuantLib::PeriodParser::parse(c.operator std::string());
    }

    template<class container_t>
    boost::shared_ptr<QuantLib::Quote> convertQuote(const container_t& c) {
        if(c.type() == typeid(double))
            return boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(c.operator double()));
        else if(c.type() == typeid(std::string)) {
            std::string s = c.operator std::string();
            double d;
            if (is_numeric(s, d))
                return boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(d));
            OH_GET_OBJECT(temp, s, ObjectHandler::Object)
            return QuantLibAddin::CoerceObject<QuantLibAddin::Quote, QuantLib::Quote, QuantLib::Quote>()(temp);
        } else {
            OH_FAIL("unable to convert type '" << c.type().name() << "' to type 'QuantLib::Quote'");
        }
    }

    template<class container_t>
    QuantLib::Handle<QuantLib::Quote> convertQuoteHandle(const container_t& c) {
        if(c.type() == typeid(long))
            return QuantLib::Handle<QuantLib::Quote>(boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(c.operator long())));
        else if(c.type() == typeid(double))
            return QuantLib::Handle<QuantLib::Quote>(boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(c.operator double())));
        else if(c.type() == typeid(std::string)) {
            std::string s = c.operator std::string();
            double d;
            if (is_numeric(s, d))
                return QuantLib::Handle<QuantLib::Quote>(boost::shared_ptr<QuantLib::Quote>(new QuantLib::SimpleQuote(d)));
            OH_GET_OBJECT(object, s, ObjectHandler::Object)
            return QuantLibAddin::CoerceHandle<QuantLibAddin::Quote, QuantLib::Quote>()(object);
        }
        else
            OH_FAIL("unable to convert type '" << c.type().name() << "' to type 'QuantLib::Quote'");
    }
    
    template<class container_t>
    QuantLib::TimeSeriesDef convertTimeSeriesDef(const container_t& c) {
        if(c.type() == typeid(std::string)) {
            std::string s = c.operator std::string();
            OH_GET_UNDERLYING(temp, s, QuantLibAddin::TimeSeriesDef, QuantLib::TimeSeriesDef)
            return temp;
        } else {
            OH_FAIL("unable to convert type '" << c.type().name() << "' to type 'QuantLib::TimeSeriesDef'");
        }
    }

}

#endif
