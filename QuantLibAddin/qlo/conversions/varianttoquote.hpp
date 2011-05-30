/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2008 Plamen Neykov

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

#ifndef qla_convert_quote_hpp
#define qla_convert_quote_hpp

#include <qlo/quote.hpp>
#include <ql/quote.hpp>
#include <ql/quotes/simplequote.hpp>

namespace ObjectHandler {
    
    class ConvertOper;

    template<>
    boost::shared_ptr<QuantLib::Quote> convert2<boost::shared_ptr<QuantLib::Quote>, property_t>(const property_t& c);

    template<>
    boost::shared_ptr<QuantLib::Quote> convert2<boost::shared_ptr<QuantLib::Quote>, ConvertOper>(const ConvertOper& c);

}

#endif
