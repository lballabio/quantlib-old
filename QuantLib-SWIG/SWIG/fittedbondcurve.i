/*
 Copyright (C) 2014 StatPro Italia srl

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

#ifndef quantlib_fitted_bond_i
#define quantlib_fitted_bond_i

%include termstructures.i
%include interpolation.i
%include ratehelpers.i

%{
using QuantLib::FittedBondDiscountCurve;

typedef boost::shared_ptr<YieldTermStructure> FittedBondDiscountCurvePtr;
typedef QuantLib::FittedBondDiscountCurve::FittingMethod FittingMethod;

std::vector<boost::shared_ptr<BondHelper> > convert_bond_helpers(
                 const std::vector<boost::shared_ptr<RateHelper> >& helpers) {
    std::vector<boost::shared_ptr<BondHelper> > result(helpers.size());
    for (int i=0; i<helpers.size(); ++i)
        result[i] = boost::dynamic_pointer_cast<BondHelper>(helpers[i]);
    return result;
}
%}

class FittingMethod {
  public:
    virtual ~FittingMethod() = 0;
    Array solution() const;
};

%rename(FittedBondDiscountCurve) FittedBondDiscountCurvePtr;
class FittedBondDiscountCurvePtr
    : public boost::shared_ptr<YieldTermStructure> {
  public:
    %extend {
        FittedBondDiscountCurvePtr(
                   Natural settlementDays,
                   const Calendar& calendar,
                   const std::vector<boost::shared_ptr<RateHelper> >& helpers,
                   const DayCounter& dayCounter,
                   const FittingMethod& fittingMethod,
                   Real accuracy = 1.0e-10,
                   Size maxEvaluations = 10000,
                   const Array& guess = Array(),
                   Real simplexLambda = 1.0) {
            return new FittedBondDiscountCurvePtr(
                new FittedBondDiscountCurve(settlementDays,
                                            calendar,
                                            convert_bond_helpers(helpers),
                                            dayCounter,
                                            fittingMethod,
                                            accuracy,
                                            maxEvaluations,
                                            guess,
                                            simplexLambda));
        }

        FittedBondDiscountCurvePtr(
                   const Date &referenceDate,
                   const std::vector<boost::shared_ptr<RateHelper> >& helpers,
                   const DayCounter& dayCounter,
                   const FittingMethod& fittingMethod,
                   Real accuracy = 1.0e-10,
                   Size maxEvaluations = 10000,
                   const Array &guess = Array(),
                   Real simplexLambda = 1.0) {
            return new FittedBondDiscountCurvePtr(
                new FittedBondDiscountCurve(referenceDate,
                                            convert_bond_helpers(helpers),
                                            dayCounter,
                                            fittingMethod,
                                            accuracy,
                                            maxEvaluations,
                                            guess,
                                            simplexLambda));
        }

        const FittingMethod& fitResults() const {
            return boost::dynamic_pointer_cast<FittedBondDiscountCurve>(*self)
                ->fitResults();
        }
    }
};


%{
using QuantLib::ExponentialSplinesFitting;
using QuantLib::NelsonSiegelFitting;
using QuantLib::SvenssonFitting;
using QuantLib::CubicBSplinesFitting;
using QuantLib::SimplePolynomialFitting;
%}

class ExponentialSplinesFitting : public FittingMethod {
  public:
    ExponentialSplinesFitting(bool constrainAtZero = true);
};

class NelsonSiegelFitting : public FittingMethod {
  public:
    NelsonSiegelFitting();
};

class SvenssonFitting : public FittingMethod {
  public:
    SvenssonFitting();
};

class CubicBSplinesFitting : public FittingMethod {
  public:
    CubicBSplinesFitting(const std::vector<Time>& knotVector,
                         bool constrainAtZero = true);
};

class SimplePolynomialFitting : public FittingMethod {
  public:
    #if defined(SWIGJAVA)
    SimplePolynomialFitting(Natural degree);
    #else
    SimplePolynomialFitting(Natural degree,
                            bool constrainAtZero = true);
    #endif
};


#endif
