/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2011 Ferdinando Ametrano
 Copyright (C) 2006 StatPro Italia srl

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

#include <qlo/flowanalysis.hpp>
#include <ql/cashflows/capflooredcoupon.hpp>
#include <ql/cashflows/digitalcoupon.hpp>
#include <ql/indexes/interestrateindex.hpp>

using QuantLib::Visitor;
using std::vector;
using ObjectHandler::property_t;

namespace QuantLibAddin {

    class AnalysisGenerator : public QuantLib::AcyclicVisitor,
                              public Visitor<QuantLib::CashFlow>,
                              public Visitor<QuantLib::Coupon>,
                              public Visitor<QuantLib::FloatingRateCoupon>,
                              public Visitor<QuantLib::CappedFlooredCoupon>,
                              public Visitor<QuantLib::DigitalCoupon> {
      private:
        vector<vector<property_t> > flows_;
        static const QuantLib::Size numberOfColumns_ = 20;
      public:
        AnalysisGenerator();
        void reset();
        void visit(QuantLib::CashFlow& c);
        void visit(QuantLib::Coupon& c);
        void visit(QuantLib::FloatingRateCoupon& c);
        void visit(QuantLib::CappedFlooredCoupon& c);
        void visit(QuantLib::DigitalCoupon& c);
        const vector<vector<property_t> >& analysis() const;
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
#define CALLDIGITALRATE 18
#define PUTDIGITALRATE 19

    AnalysisGenerator::AnalysisGenerator() { reset(); }

    void AnalysisGenerator::reset() {
        flows_.clear();

        vector<property_t> headings(numberOfColumns_);
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
        headings[PUTDIGITALRATE]=std::string("Put Digital Payoff");
        headings[CALLDIGITALRATE]=std::string("Call Digital Payoff");

        flows_.push_back(headings);
    }

    void AnalysisGenerator::visit(QuantLib::CashFlow& c) {
        vector<property_t> cf(numberOfColumns_, std::string("#N/A"));
        cf[PAYMENT_DATE]=c.date().serialNumber();
        try {
            cf[AMOUNT]=c.amount();
        } catch(...) {}
        flows_.push_back(cf);
    }

    void AnalysisGenerator::visit(QuantLib::Coupon& c) {
        visit(static_cast<QuantLib::CashFlow&>(c));
        flows_.back()[NOMINAL]=c.nominal();
        flows_.back()[ACCRUAL_START_DATE]=c.accrualStartDate().serialNumber();
        flows_.back()[ACCRUAL_END_DATE]=c.accrualEndDate().serialNumber();
        flows_.back()[ACCRUAL_DAYS]=(long)c.accrualDays();
        flows_.back()[DAY_COUNTER]=c.dayCounter().name();
        flows_.back()[ACCRUAL_PERIOD]=c.accrualPeriod();
        try {
            flows_.back()[EFFECTIVE_RATE]=c.rate();
        } catch(...) {}
    };

    void AnalysisGenerator::visit(QuantLib::FloatingRateCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flows_.back()[FIXING_DAYS]=(long)c.fixingDays();
        flows_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flows_.back()[INDEX]=c.index()->name();
        flows_.back()[FLOOR]=std::string("#N/A");
        flows_.back()[GEARING]=c.gearing();
        try {
            flows_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flows_.back()[CONV_ADJ]=c.convexityAdjustment();
        } catch(...) {}
        flows_.back()[SPREAD]=c.spread();
        flows_.back()[CAP]=std::string("#N/A");
    }

    void AnalysisGenerator::visit(QuantLib::CappedFlooredCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flows_.back()[FIXING_DAYS]=(long)c.fixingDays();
        flows_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flows_.back()[INDEX]=c.index()->name();
        if (c.floor() != QuantLib::Null<QuantLib::Rate>())
            flows_.back()[FLOOR]=c.floor();
        else
            flows_.back()[FLOOR]=std::string("#N/A");
        flows_.back()[GEARING]=c.gearing();
        try {
            flows_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flows_.back()[CONV_ADJ]=c.convexityAdjustment();
        } catch(...) {}
        flows_.back()[SPREAD]=c.spread();
        if (c.cap() != QuantLib::Null<QuantLib::Rate>())
            flows_.back()[CAP]=c.cap();
        else
            flows_.back()[CAP]=std::string("#N/A");
    }

    void AnalysisGenerator::visit(QuantLib::DigitalCoupon& c) {
        visit(static_cast<QuantLib::Coupon&>(c));
        flows_.back()[FIXING_DAYS]=(long)c.fixingDays();
        flows_.back()[FIXING_DATES]=c.fixingDate().serialNumber();
        flows_.back()[INDEX]=c.index()->name();
        if (c.hasPut())
            flows_.back()[FLOOR]=c.putStrike();
        else
            flows_.back()[FLOOR]=std::string("#N/A");
        flows_.back()[GEARING]=c.gearing();
        try {
            flows_.back()[INDEX_FIXING]=c.indexFixing();
        } catch(...) {}
        try {
            flows_.back()[CONV_ADJ]=c.underlying()->convexityAdjustment();
        } catch(...) {}
        flows_.back()[SPREAD]=c.spread();
        if (c.hasCall())
            flows_.back()[CAP]=c.callStrike();
        else
            flows_.back()[CAP]=std::string("#N/A");
        if (c.putDigitalPayoff() != QuantLib::Null<QuantLib::Rate>())
            flows_.back()[PUTDIGITALRATE]=c.putDigitalPayoff();
        else
            flows_.back()[PUTDIGITALRATE]=std::string("#N/A");
        if (c.callDigitalPayoff() != QuantLib::Null<QuantLib::Rate>())
            flows_.back()[CALLDIGITALRATE]=c.callDigitalPayoff();
        else
            flows_.back()[CALLDIGITALRATE]=std::string("#N/A");
    }

    const vector<vector<property_t> >& AnalysisGenerator::analysis() const {
        return flows_;
    }

    vector<vector<property_t> > flowAnalysis(const QuantLib::Leg& leg,
                                             const QuantLib::Date& d) {
        AnalysisGenerator generator;
        for (QuantLib::Size i=0; i<leg.size(); ++i) {
            if (leg[i]->date()>d)
            leg[i]->accept(generator);
        }
        return generator.analysis();
    }
}
