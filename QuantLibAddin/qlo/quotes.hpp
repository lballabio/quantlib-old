
/*
 Copyright (C) 2006 Francois du Vignaud
 Copyright (C) 2006 Giorgio Facchinetti

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

#ifndef qla_quotes_hpp
#define qla_quotes_hpp

#include <qlo/index.hpp>
#include <oh/object.hpp>
#include <qlo/termstructures.hpp>
#include <ql/Quotes/derivedquote.hpp>
#include <ql/Quotes/futuresconvadjustmentquote.hpp>


namespace QuantLibAddin {

    class Quote : public ObjHandler::LibraryObject<QuantLib::Quote> {};

    class SimpleQuote : public Quote {
      public:
        SimpleQuote(QuantLib::Real value);
    };

    class ForwardValueQuote : public Quote {
      public:
        ForwardValueQuote(const boost::shared_ptr<QuantLib::IborIndex>&,
                          const QuantLib::Date& fixingDate);
    };

    class ImpliedStdDevQuote : public Quote {
      public:
        ImpliedStdDevQuote(const QuantLib::Option::Type& optionType,
                           const QuantLib::Handle<QuantLib::Quote>& forward,
                           const QuantLib::Handle<QuantLib::Quote>& price,
                           QuantLib::Real strike,
                           QuantLib::Real guess,
                           QuantLib::Real accuracy);
    };

    class EurodollarFuturesImpliedStdDevQuote : public Quote {
      public:
        EurodollarFuturesImpliedStdDevQuote(
                        const QuantLib::Handle<QuantLib::Quote>& forward,
                        const QuantLib::Handle<QuantLib::Quote>& callPrice,
                        const QuantLib::Handle<QuantLib::Quote>& putPrice,
                        QuantLib::Real strike,
                        QuantLib::Real guess,
                        QuantLib::Real accuracy);
    };

     class FuturesConvAdjustmentQuote : public Quote {
      public:
        FuturesConvAdjustmentQuote(
                    const boost::shared_ptr<QuantLib::IborIndex>& index,
                    const std::string& immCode, 
                    const QuantLib::Handle<QuantLib::Quote>& futuresQuote,
                    const QuantLib::Handle<QuantLib::Quote>& volatility,
                    const QuantLib::Handle<QuantLib::Quote>& meanReversion);
    };

}

#endif
