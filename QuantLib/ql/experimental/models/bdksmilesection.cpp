#include <ql/experimental/models/bdksmilesection.hpp>
#include <ql/pricingengines/blackformula.hpp>

#include <iostream>

namespace QuantLib {

    BdkSmileSection::BdkSmileSection(
        const boost::shared_ptr<SmileSection> source, const Real mu,
        const Real nu, const Real leftCutoff, const Real rightCutoff,
        const Real atm)
        : source_(source), mu_(mu), nu_(nu), leftCutoff_(leftCutoff),
          rightCutoff_(rightCutoff) {

        if (atm == Null<Real>()) {
            f_ = source_->atmLevel();
        } else {
            f_ = atm;
        }

        // compute paramters for price extrapolation at left bound

        Real left1d = source->digitalOptionPrice(leftCutoff_, Option::Put);
        Real left2d = source->density(leftCutoff_);
        Real p0 = source->optionPrice(leftCutoff_, Option::Put);
        Real l1 = left1d / p0;
        Real l2 = -left1d / (p0 * p0) * left1d + 1.0 / p0 * left2d;

        lc_ = 0.5 * (l2 + mu_ / (leftCutoff_ * leftCutoff_));
        lb_ = l1 - mu_ / leftCutoff_ - 2.0 * lc_ * leftCutoff_;
        la_ = log(p0) - mu_ * log(leftCutoff_) - lb_ * leftCutoff_ -
              lc_ * leftCutoff_ * leftCutoff_;

        // compute parameters for price extrapolation at right bound

        Real right1d_ = -source->digitalOptionPrice(rightCutoff_);
        Real right2d_ = source->density(rightCutoff_);
        Real p1 = source->optionPrice(rightCutoff_);
        Real r1 = right1d_ / p1;
        Real r2 = -right1d_ / (p1 * p1) * right1d_ + 1.0 / p1 * right2d_;

        rc_ = r1 * rightCutoff_ * rightCutoff_ * rightCutoff_ +
              0.5 * rightCutoff_ * rightCutoff_ *
                  (rightCutoff_ * rightCutoff_ * r2 + nu_);
        rb_ = -rightCutoff_ * rightCutoff_ * r1 - nu_ * rightCutoff_ -
              2.0 * rc_ / rightCutoff_;
        ra_ = log(p1) + nu_ * log(rightCutoff_) - rb_ / rightCutoff_ -
              rc_ / (rightCutoff_ * rightCutoff_);
    }

    Real BdkSmileSection::optionPrice(Rate strike, Option::Type type,
                                      Real discount) const {
        if (strike < leftCutoff_) {
            return discount * (pow(strike, mu_) * exp(la_ + lb_ * strike +
                                                      lc_ * strike * strike) +
                               (type == Option::Put ? 0.0 : f_ - strike));
        }
        if (strike > rightCutoff_) {
            return discount *
                   (pow(strike, -nu_) *
                        exp(ra_ + rb_ / strike + rc_ / (strike * strike)) +
                    (type == Option::Call ? 0.0 : strike - f_));
        }
        return source_->optionPrice(strike, type, discount);
    }

    Real BdkSmileSection::volatilityImpl(Rate strike) const {
        Real vol = 0.0;
        try {
            vol = blackFormulaImpliedStdDev(Option::Call, strike, f_,
                                            optionPrice(strike)) /
                  sqrt(source_->exerciseTime());
        }
        catch (QuantLib::Error) {
        }
        return vol;
    }
}
