
/*
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

#ifndef qla_couponvectors_hpp
#define qla_couponvectors_hpp

#include <oh/objhandler.hpp>
#include <ql/CashFlows/cashflowvectors.hpp>

namespace QuantLibAddin {
    
    typedef std::vector<boost::shared_ptr<QuantLib::CashFlow> > CashFlowVector;
    
    class CouponVector : public ObjHandler::Object {
      public:
        virtual boost::shared_ptr<void> getReference() const {
            return boost::shared_ptr<void>();
        }
        
        const CashFlowVector& getObject() const {
            return cashFlowVector_;
        }
        
        virtual std::vector<std::vector<double> >  getLeg() = 0;
        
        double getBPS(const std::string &termStructureID) const;
        
      protected:
        CashFlowVector cashFlowVector_;
    };
    
    class FixedRateCouponVector : public CouponVector {
      public:
        FixedRateCouponVector(
            const std::string                       &scheduleID,
            const QuantLib::BusinessDayConvention   &convention,
            const std::vector<double>               &nominals,
            const std::vector<double>               &couponRates,
            const QuantLib::DayCounter              &dayCountID);
        
        virtual std::vector<std::vector<double> > getLeg();
    };
    
    class FloatingRateCouponVector : public CouponVector {
      public:
        FloatingRateCouponVector(
            const std::string         &scheduleID,
            const std::vector<double> &nominals,
            const std::string         &indexID,
            const std::vector<double> &spreads);
        
        virtual std::vector<std::vector<double> > getLeg();        
    };
    
}

#endif

