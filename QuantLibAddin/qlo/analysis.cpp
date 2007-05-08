
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 StatPro Italia srl

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/analysis.hpp>
#include <ql/cashflows/capflooredcoupon.hpp>
#include <ql/cashflows/digitalcoupon.hpp>
#include <ql/indexes/interestrateindex.hpp>

namespace QuantLibAddin {

    class AnalysisGenerator : public QuantLib::AcyclicVisitor,
                              public QuantLib::Visitor<QuantLib::CashFlow>,
                              public QuantLib::Visitor<QuantLib::Coupon>,
                              public QuantLib::Visitor<QuantLib::FloatingRateCoupon>,
                              public QuantLib::Visitor<QuantLib::CappedFlooredCoupon>,
                              public QuantLib::Visitor<QuantLib::DigitalCoupon> {
      private:
        std::vector<std::vector<boost::any> > flowAnalysis_;
        static const QuantLib::Size numberOfColumns_ = 20;
      public:
        AnalysisGenerator();
        void reset();
        void visit(QuantLib::CashFlow& c);
        void visit(QuantLib::Coupon& c);
        void visit(QuantLib::FloatingRateCoupon& c);
        void visit(QuantLib::CappedFlooredCoupon& c);
        void visit(QuantLib::DigitalCoupon& c);
        const std::vector<std::vector<boost::any> >& analysis() const;
    };

#define PAYMENT_DATE 0
#define AMOUNT 1
#define NOMINAL 2
#define ACCRUAL_START_DATE 3
#define ACCRUAL_END_DATE 4
#define ACCRUAL_DAYS 5
#define INDEX 6
#define FIXING_DAYS 7
#define FIXING_DATES 8
#define DAY_COUNTER 9
#define ACCRUAL_PERIOD 10
#define EFFECTIVE_RATE 11
#define FLOOR 12
#define GEARING 13
#define INDEX_FIXING 14
#define CONV_ADJ 15
#define SPREAD 16
#define CAP 17
#define DIGITALRATE 18
#define FUTUREUSE2 19

    AnalysisGenerator::AnalysisGenerator() { reset(); }

    void AnalysisGenerator::reset() {
        flowAnalysis_.clear();

        std::vector<boost::any> headings(numberOfColumns_);
        headings[PAYMENT_DATE]=std::string("Payment Date");
        headings[AMOUNT]=std::string("Amount");

        headings[NOMINAL]=std::string("Nominal");
        headings[ACCRUAL_START_DATE]=std::string("Accrual Start Date");
        headings[ACCRUAL_END_DATE]=std::string("Accrual End Date");
        headings[ACCRUAL_DAYS]=std::string("Accrual Days");
        headings[DAY_COUNTER]=std::string("Day Counter");
        headings[ACCRUAL_PERIOD]=std::string("Accrual Period");
        headings[EFFECTIVE_RATE]=std::string("Effective Rate");

        headings[FIXING_DAYS]=std::string("Fixing Days");
        headings[FIXING_DATES]=std::string("Fixing Dates");
        headings[INDEX]=std::string("Index");
        headings[FLOOR]=std::string("Floor");
        headings[GEARING]=std::string("Gearing");
        headings[INDEX_FIXING]=std::string("Index Fixing");
        headings[CONV_ADJ]=std::string("Conv. Adj.");
        headings[SPREAD]=std::string("Spread");
        headings[CAP]=std::string("Cap");
        headings[DIGITALRATE]=std::string("Digital Rate");
        headings[FUTUREUSE2]=std::string("---");

        flowAnalysis_.push_back(headings);
    }

    void AnalysisGenerator::visit(QuantLib::CashFlow& c) {
        std::vector<boost::any> cf(numberOfColumns_, std::string("#N/A"));
        cf[PAYMENT_DATE]=c.date().serialNumber();
        try {
            cf[AMOUNT]=c.amount();
        } catch(...) {}
        flowAnalysis_.push_back(cf);
    }

    void AnalysisGenerator::visit(QuantLib::Coupon& c) {
        visit(static_cast<QuantLib::CashFlow&>(c));
        flowAnalysis_.back()[NOMINAL]=c.nominal();
        flowAnalysis_.back()[ACCRUAL_START_DATE]=c.accrualStartDate().serialNumber();
        flowAnalysis_.back()[ACCRUAL_END_DATE]=c.accrualEndDate().serialNumber();
        flowAnalysis_.back()[ACCRUAL_DAYS]=c.accrualDays();
        flowAnalysis_.back()[DAY_COUNTER]=c.dayCounter().name();
        flowAnalysis_.back()[ACCRUAL_PERIOD]=c.accrualPeriod();
        try {
            flowAnalysis_.back()[EFFECTIVE_RATE]=c.rate();
        } catch(...) {}
    };

    void AnalysisGenerator::visit(QuantLib::FloatingRateCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flowAnalysis_.back()[FIXING_DAYS]=c.fixingDays();
        flowAnalysis_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flowAnalysis_.back()[INDEX]=c.index()->name();
        flowAnalysis_.back()[FLOOR]=std::string("#N/A");
        flowAnalysis_.back()[GEARING]=c.gearing();
        try {
            flowAnalysis_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flowAnalysis_.back()[CONV_ADJ]=c.convexityAdjustment();
        } catch(...) {}
        flowAnalysis_.back()[SPREAD]=c.spread();
        flowAnalysis_.back()[CAP]=std::string("#N/A");
    }

    void AnalysisGenerator::visit(QuantLib::CappedFlooredCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flowAnalysis_.back()[FIXING_DAYS]=c.fixingDays();
        flowAnalysis_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flowAnalysis_.back()[INDEX]=c.index()->name();
        if (c.floor() != QuantLib::Null<QuantLib::Rate>())
            flowAnalysis_.back()[FLOOR]=c.floor();
        else
            flowAnalysis_.back()[FLOOR]=std::string("#N/A");
        flowAnalysis_.back()[GEARING]=c.gearing();
        try {
            flowAnalysis_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flowAnalysis_.back()[CONV_ADJ]=c.convexityAdjustment();
        } catch(...) {}
        flowAnalysis_.back()[SPREAD]=c.spread();
        if (c.cap() != QuantLib::Null<QuantLib::Rate>())
            flowAnalysis_.back()[CAP]=c.cap();
        else
            flowAnalysis_.back()[CAP]=std::string("#N/A");
    }

    void AnalysisGenerator::visit(QuantLib::DigitalCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flowAnalysis_.back()[FIXING_DAYS]=c.fixingDays();
        flowAnalysis_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flowAnalysis_.back()[INDEX]=c.index()->name();
        if (c.hasCollar())
            flowAnalysis_.back()[FLOOR]=c.callStrike();
        else if (c.hasCall() && !c.isCallAdded())
            flowAnalysis_.back()[FLOOR]=c.callStrike();
        else if (c.hasPut() && !c.isPutAdded())
            flowAnalysis_.back()[FLOOR]=c.putStrike();
        else
            flowAnalysis_.back()[FLOOR]=std::string("#N/A");
        flowAnalysis_.back()[GEARING]=c.gearing();
        try {
            flowAnalysis_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flowAnalysis_.back()[CONV_ADJ]=c.underlying()->convexityAdjustment();
        } catch(...) {}
        flowAnalysis_.back()[SPREAD]=c.spread();
        if (c.hasCollar())
            flowAnalysis_.back()[CAP]=c.putStrike();
        else if (c.hasCall() && c.isCallAdded())
            flowAnalysis_.back()[CAP]=c.callStrike();
        else if (c.hasPut() && c.isPutAdded())
            flowAnalysis_.back()[CAP]=c.putStrike();
        else
            flowAnalysis_.back()[CAP]=std::string("#N/A");
        if (c.cashRate() != QuantLib::Null<QuantLib::Rate>())
            flowAnalysis_.back()[DIGITALRATE]=c.cashRate();
        else
            flowAnalysis_.back()[DIGITALRATE]=std::string("#N/A");
    }

    const std::vector<std::vector<boost::any> >& AnalysisGenerator::analysis() const {
        return flowAnalysis_;
    }

    std::vector<std::vector<boost::any> > flowAnalysis(const QuantLib::Leg& leg) {
        AnalysisGenerator generator;
        for (QuantLib::Size i=0; i<leg.size(); ++i)
            leg[i]->accept(generator);
        return generator.analysis();
    }
}

