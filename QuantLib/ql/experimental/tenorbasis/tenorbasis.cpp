/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Ferdinando Ametrano
 Copyright (C) 2015 Paolo Mazzocchi

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

#include <ql/experimental/tenorbasis/tenorbasis.hpp>
#include <ql/indexes/iborindex.hpp>


using boost::shared_ptr;
using std::vector;

namespace QuantLib {

    TenorBasis::TenorBasis(Size nArguments,
                           shared_ptr<IborIndex> iborIndex,
                           const Handle<YieldTermStructure>& baseCurve,
                           Date referenceDate)
    : CalibratedModel(nArguments),
      index_(iborIndex), baseCurve_(baseCurve), referenceDate_(referenceDate) {
        
        QL_REQUIRE(iborIndex!=0, "empty iborIndex");
        if (referenceDate_ == Date()) {
            Date today = Settings::instance().evaluationDate();
            referenceDate_ = iborIndex->valueDate(today);
            // avoid the following as the Hande could be empty
            //referenceDate_ = baseCurve_->referenceDate();
        }
        dc_ = index_->dayCounter();
        bdc_ = index_->businessDayConvention();
        eom_ = index_->endOfMonth();
        cal_ = index_->fixingCalendar();
        tenor_ = index_->tenor();
        Date endDate = cal_.advance(referenceDate_, tenor_, bdc_, eom_);
        tau_ = dc_.yearFraction(referenceDate_, endDate);
    }

    Spread TenorBasis::value(Date d) const {
        Time t = timeFromReference(d);
        return value(t);
    }

    Rate TenorBasis::tenorForwardRate(Date d1) const {
        Date d2 = cal_.advance(d1, tenor_, bdc_, eom_);
        // baseCurve must be a discounting curve...
        // otherwise it could not provide fwd(d1, d2) with d2-d1!=tau
        Real accrFactor = baseCurve_->discount(d1) / baseCurve_->discount(d2);
        Time dt = dc_.yearFraction(d1, d2);
        Rate baseFwd = (accrFactor - 1.0) / dt;

        Rate basis = value(d1);
        return baseFwd + basis;
    }

    Rate TenorBasis::tenorForwardRate(Time t1) const {
        // we need Date algebra to calculate d2
        Date d1 = dateFromTime(t1);
        return tenorForwardRate(d1);
    }

    Rate TenorBasis::forwardRate(Date d1) const {
        Date d2 = cal_.advance(d1, tenor_, bdc_, eom_);
        return forwardRate(d1, d2);
    }

    Rate TenorBasis::forwardRate(Date d1,
                                 Date d2) const {
        // baseCurve must be a discounting curve...
        // otherwise it could not provide fwd(t1, t2) with t2-t1!=tau_
        Real accrFactor = baseCurve_->discount(d1) / baseCurve_->discount(d2);
        Real instContBasisIntegral = integrate_(d1, d2);
        accrFactor *= std::exp(instContBasisIntegral);

        Time dt = dc_.yearFraction(d1, d2);
        Rate fwd = (accrFactor - 1.0) / dt;
        return fwd;
    }

    Rate TenorBasis::forwardRate(Time t1,
                                 Time t2) const {
        Date d1 = dateFromTime(t1);
        Date d2 = dateFromTime(t2);
        return forwardRate(d1, d2);
    }

    Time TenorBasis::timeFromReference(Date d) const {
        // Actual365Fixed is hardcoded. It must be an invertible DayCounter
        // see also TenorBasis::dateFromTime(Time t)
        return Actual365Fixed().yearFraction(referenceDate_, d);
    }

    Date TenorBasis::dateFromTime(Time t) const {
        // Actual365Fixed is hardcoded. It must be an invertible DayCounter
        // see also TenorBasis::timeFromReference(Date d)
        BigInteger result =
            referenceDate_.serialNumber() + BigInteger(t*365.0);
        if (result >= Date::maxDate().serialNumber())
            return Date::maxDate();
        return Date(result);
    }

    const shared_ptr<IborIndex>& TenorBasis::iborIndex() const {
        return index_;
    }

    const Handle<YieldTermStructure>& TenorBasis::baseCurve() const {
        return baseCurve_;
    }

    Real TenorBasis::integrate_(Date d1) const {
        Date d2 = cal_.advance(d1, tenor_, bdc_, eom_);
        return integrate_(d1, d2);
    }

    Real TenorBasis::integrate_(Date d1,
                                Date d2) const {
        Time t1 = timeFromReference(d1);
        Time t2 = timeFromReference(d2);
        return integrate_(t1, t2);
    }


    AbcdTenorBasis::AbcdTenorBasis(shared_ptr<IborIndex> iborIndex,
                                   const Handle<YieldTermStructure>& baseCurve,
                                   Date referenceDate,
                                   bool isSimple,
                                   const std::vector<Real>& coeff)
    : TenorBasis(4, iborIndex, baseCurve, referenceDate),coeff_(coeff) {
        //arguments_[0] = ConstantParameter(coeff[0], NoConstraint());
        //arguments_[1] = ConstantParameter(coeff[1], NoConstraint());
        //arguments_[2] = ConstantParameter(coeff[2], NoConstraint());
        //arguments_[3] = ConstantParameter(coeff[3], NoConstraint());
        isSimple_ = isSimple;
        generateArguments();
    }

    AbcdTenorBasis::AbcdTenorBasis(shared_ptr<IborIndex> iborIndex,
                                   const Handle<YieldTermStructure>& baseCurve,
                                   Date referenceDate,
                                   bool isSimple,
                                   boost::shared_ptr<AbcdMathFunction> f)
    : TenorBasis(4, iborIndex, baseCurve, referenceDate),
      coeff_(f->coefficients()) {
        //arguments_[0] = ConstantParameter(coeff[0], NoConstraint());
        //arguments_[1] = ConstantParameter(coeff[1], NoConstraint());
        //arguments_[2] = ConstantParameter(coeff[2], NoConstraint());
        //arguments_[3] = ConstantParameter(coeff[3], NoConstraint());
        isSimple_ = isSimple;
        generateArguments();
    }

    void AbcdTenorBasis::generateArguments(){
        if (isSimple_) {
            basis_ = 
                    shared_ptr<AbcdMathFunction>(new AbcdMathFunction(coeff_));
            vector<Real> c = basis_->definiteDerivativeCoefficients(0.0, tau_);
            c[0] *= tau_;
            c[1] *= tau_;
            // unaltered c[2] (the c in abcd)
            c[3] *= tau_;
            instBasis_ = shared_ptr<AbcdMathFunction>(new AbcdMathFunction(c));
        } else {
            instBasis_ = 
                    shared_ptr<AbcdMathFunction>(new AbcdMathFunction(coeff_));
            vector<Real> c = 
                           instBasis_->definiteIntegralCoefficients(0.0, tau_);
            c[0] /= tau_;
            c[1] /= tau_;
            // unaltered c[2] (the c in abcd)
            c[3] /= tau_;
            basis_ = shared_ptr<AbcdMathFunction>(new AbcdMathFunction(c));
        }
        
    }

    const vector<Real>& AbcdTenorBasis::coefficients() const {
        return basis_->coefficients();
    }

    const vector<Real>& AbcdTenorBasis::instCoefficients() const {
        return instBasis_->coefficients();
    }

    Date AbcdTenorBasis::maximumLocation() const {
        Time maximumLocation = basis_->maximumLocation();
        return dateFromTime(maximumLocation);
    }

    Spread AbcdTenorBasis::maximumValue() const {
        Date d = maximumLocation();
        return TenorBasis::value(d);
    }

    Real AbcdTenorBasis::integrate_(Time t1,
                                    Time t2) const {
        return instBasis_->definiteIntegral(t1, t2);
    }

    //void AbcdTenorBasis::generateArguments(){
    //    if (isSimple_) {
    //        vector<Real> coef = basis_->coefficients();
    //        basis_.reset(new AbcdMathFunction(coef));
    //        vector<Real> c = basis_->definiteDerivativeCoefficients(0.0, tau_);
    //        c[0] *= tau_;
    //        c[1] *= tau_;
    //        // unaltered c[2] (the c in abcd)
    //        c[3] *= tau_;
    //        instBasis_.reset(new AbcdMathFunction(c));
    //    }
    //    else {
    //        vector<Real> coef = instBasis_->coefficients();
    //        instBasis_.reset(new AbcdMathFunction(coef));
    //        vector<Real> c = instBasis_->definiteIntegralCoefficients(0.0, tau_);
    //        c[0] /= tau_;
    //        c[1] /= tau_;
    //        // unaltered c[2] (the c in abcd)
    //        c[3] /= tau_;
    //        basis_.reset(new AbcdMathFunction(c));
    //    }
    //}

    PolynomialTenorBasis::PolynomialTenorBasis(
                                shared_ptr<IborIndex> iborIndex,
                                const Handle<YieldTermStructure>& baseCurve,
                                Date referenceDate,
                                bool isSimple,
                                const std::vector<Real>& coeff)
    : TenorBasis(coeff.size(), iborIndex, baseCurve, referenceDate),
      isSimple_(isSimple), coeff_(coeff) {
        //for (Size i = 0; i<coeff_.size(); ++i)
        //    arguments_[i] = ConstantParameter(coeff_[i], NoConstraint());
        generateArguments();
    }

    PolynomialTenorBasis::PolynomialTenorBasis(
                                   shared_ptr<IborIndex> iborIndex,
                                   const Handle<YieldTermStructure>& baseCurve,
                                   Date referenceDate,
                                   bool isSimple,
                                   boost::shared_ptr<PolynomialFunction> f)
    : TenorBasis(f->coefficients().size(), iborIndex, baseCurve, referenceDate),
      isSimple_(isSimple), coeff_(f->coefficients()) {
        //for (Size i = 0; i<coeff_.size(); ++i)
        //    arguments_[i] = ConstantParameter(coeff_[i], NoConstraint());
        generateArguments();
    }

    void PolynomialTenorBasis::generateArguments() {
        if (isSimple_) {
            basis_ =
                shared_ptr<PolynomialFunction>(new PolynomialFunction(coeff_));
            std::vector<Real> c =
                basis_->definiteDerivativeCoefficients(0.0, tau_);
            for (Size i=0; i<c.size(); ++i)
                c[i] *= tau_;
            instBasis_ =
                shared_ptr<PolynomialFunction>(new PolynomialFunction(c));
        } else {
            instBasis_ =
                shared_ptr<PolynomialFunction>(new PolynomialFunction(coeff_));
            std::vector<Real> c =
                instBasis_->definiteIntegralCoefficients(0.0, tau_);
            for (Size i=0; i<c.size(); ++i)
                c[i] /= tau_;
            basis_ =
                shared_ptr<PolynomialFunction>(new PolynomialFunction(c));
        }
    }
   
    const vector<Real>& PolynomialTenorBasis::coefficients() const {
        return basis_->coefficients();
    }

    const vector<Real>& PolynomialTenorBasis::instCoefficients() const {
        return instBasis_->coefficients();
    }

    Real PolynomialTenorBasis::integrate_(Time t1,
                                          Time t2) const {
        return instBasis_->definiteIntegral(t1, t2);
    }

    //void calibrate(const std::vector<boost::shared_ptr<CalibrationHelperBase> >& h,
    //               OptimizationMethod& method,
    //               const EndCriteria& endCriteria,
    //               const Constraint& additionalConstraint,
    //               const std::vector<Real>& weights,
    //               const std::vector<bool>& fixParameters){
    //    for (Size i = 0; i < h.size(); ++i){
    //        const boost::shared_ptr<CalibrationHelperBase>& helper = h[i];
    //         //check for valid quote
    //        //QL_REQUIRE(helper->quote()->isValid(),
    //        //    io::ordinal(j + 1) << " instrument (maturity: " <<
    //        //    helper->latestDate() << ") has an invalid quote");
    //        //helper->setTermStructure(const_cast<Curve*>(basis_));
    //        helper->setTermStructure(basis_);
    //    }
    //    CalibratedModel::calibrate(h,
    //                               method,
    //                               endCriteria,
    //                               additionalConstraint,
    //                               weights,
    //                               fixParameters);
    //}
}

//if (isSimple_) {
//    basis_ = f;
//    vector<Real> c = f->definiteDerivativeCoefficients(0.0, tau_);
//    for (Size i=0; i<c.size(); ++i)
//        c[i] *= tau_;
//    instBasis_ = shared_ptr<PolynomialFunction>(new
//        PolynomialFunction(c));
//} else {
//    instBasis_ = f;
//    vector<Real> c = f->definiteIntegralCoefficients(0.0, tau_);
//    for (Size i=0; i<c.size(); ++i)
//        c[i] /= tau_;
//    basis_ = shared_ptr<PolynomialFunction>(new PolynomialFunction(c));
//}
//vector<Real> coef = f->coefficients();