/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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

/*! \file
    \brief \t static_visitor classes performing conversions on type Variant
*/

#ifndef oh_variant_visitors_hpp
#define oh_variant_visitors_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <oh/other.hpp>
#include <boost/variant.hpp>
#include <string>

namespace ObjectHandler {

    //! Convert a Variant to a long
    class VariantToLong : public boost::static_visitor<> {
    public:
        VariantToLong(long &l) : l_(l) {}
        void operator()(const long &l) { l_= l; }
        void operator()(const double &d) { l_ = static_cast<long>(d); }
        void operator()(const bool &b) { l_ = b; }
        void operator()(const std::string&) { OH_FAIL("invalid conversion"); }
        void operator()(const ObjectHandler::Other&) { OH_FAIL("invalid conversion"); }
    private:
        long &l_;
    };

    //! Convert a Variant to a double
    class VariantToDouble : public boost::static_visitor<> {
    public:
        VariantToDouble(double &l) : l_(l) {}
        void operator()(const long &l) { l_= l; }
        void operator()(const double &d) { l_= d; }
        void operator()(const bool &b) { l_ = b; }
        void operator()(const std::string&) { OH_FAIL("invalid conversion"); }
        void operator()(const ObjectHandler::Other&) { OH_FAIL("invalid conversion"); }
    private:
        double &l_;
    };

    //! Convert a Variant to a boolean
    class VariantToBoolean : public boost::static_visitor<> {
    public:
        VariantToBoolean(bool &l) : l_(l) {}
        void operator()(const long &l) { OH_FAIL("invalid conversion"); }
        void operator()(const double &d) { OH_FAIL("invalid conversion"); }
        void operator()(const bool &l) { l_= l; }
        void operator()(const std::string&) { OH_FAIL("invalid conversion"); }
        void operator()(const ObjectHandler::Other&) { OH_FAIL("invalid conversion"); }
    private:
        bool &l_;
    };

    //! Convert a Variant to a std::string
    class VariantToString : public boost::static_visitor<> {
    public:
        VariantToString(std::string &l) : l_(l) {}
        void operator()(const long &l) { OH_FAIL("invalid conversion"); }
        void operator()(const double &d) { OH_FAIL("invalid conversion"); }
        void operator()(const bool &l) { OH_FAIL("invalid conversion"); }
        void operator()(const std::string &l) { l_= l; }
        void operator()(const ObjectHandler::Other&) { OH_FAIL("invalid conversion"); }
    private:
        std::string &l_;
    };

    //! Convert a Variant to type Missing
    class VariantMissing : public boost::static_visitor<> {
    public:
        VariantMissing(bool &l) : l_(l) {}
        void operator()(const long &l) { l_ = false; }
        void operator()(const double &d) { l_ = false; }
        void operator()(const bool &l) { l_ = false; }
        void operator()(const std::string &l) { l_ = false; }
        void operator()(const Other &l) { l_ = l.type()==Null; }
    private:
        bool &l_;
    };

    //! Convert a Variant to type Error
    class VariantError : public boost::static_visitor<> {
    public:
        VariantError(bool &l) : l_(l) {}
        void operator()(const long &l) { l_ = false; }
        void operator()(const double &d) { l_ = false; }
        void operator()(const bool &l) { l_ = false; }
        void operator()(const std::string &l) { l_ = false; }
        void operator()(const ObjectHandler::Other &l) { l_ = l.type()==Error; }
    private:
        bool &l_;
    };

    //! Return the Type of a Variant
    class VariantType : public boost::static_visitor<> {
    public:
        VariantType(Type &type) : type_(type) {}
        void operator()(const long &type) { type_ = Long; }
        void operator()(const double &d) { type_ = Double; }
        void operator()(const bool &type) { type_ = Boolean; }
        void operator()(const std::string &type) { type_ = String; }
        void operator()(const ObjectHandler::Other &other) { type_ = other.type(); }
    private:
        Type &type_;
    };

}

#endif

