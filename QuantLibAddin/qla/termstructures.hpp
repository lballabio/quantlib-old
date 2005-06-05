
/*
 Copyright (C) 2005 Plamen Neykov
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

#ifndef qla_termstructures_hpp
#define qla_termstructures_hpp

#include <qla/zerocurve.hpp>
#include <ql/termstructure.hpp>
#include <ql/TermStructures/piecewiseflatforward.hpp>
#include <ql/TermStructures/ratehelpers.hpp>

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    class RateHelper : public ObjHandler::Object {
      public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(rateHelper_);
        }
		double setQuote(double quote);
      protected:
        boost::shared_ptr<QuantLib::SimpleQuote> quote_;
        boost::shared_ptr<QuantLib::RateHelper> rateHelper_;
    };
    
    class DepositRateHelper : public RateHelper {
      public:
        DepositRateHelper(ObjHandler::ArgumentStack& args);
    };

    class SwapRateHelper : public RateHelper {
      public:
        SwapRateHelper(ObjHandler::ArgumentStack& args);
    };

	class FutureRateHelper : public RateHelper {
      public:
        FutureRateHelper(ObjHandler::ArgumentStack& args);
    };

    class YieldTermStructure : public ObjHandler::Object {
      public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(termStructure_);
        }
		double getDf(long d, bool ipol, double) const;
      protected:
        boost::shared_ptr<QuantLib::YieldTermStructure> termStructure_;
    };
    
    class PiecewiseFlatForward : public YieldTermStructure {
      public:
        PiecewiseFlatForward(ObjHandler::ArgumentStack& args);
    };
}

#endif

