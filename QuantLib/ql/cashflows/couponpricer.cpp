//todo move to header, once template'zed

#include <ql/cashflows/couponpricer.hpp>

namespace QuantLib {

    void SubPeriodsPricer::initialize(const FloatingRateCoupon& coupon) {
        coupon_ =  dynamic_cast<const SubPeriodsCoupon*>(&coupon);
        QL_REQUIRE(coupon_, "sub-periods coupon required");
        gearing_ = coupon_->gearing();
        spread_ = coupon_->spread();

        Date paymentDate = coupon_->date();

        boost::shared_ptr<IborIndex> index =
            boost::dynamic_pointer_cast<IborIndex>(coupon_->index());
        const Handle<YieldTermStructure>& rateCurve =
            index->forwardingTermStructure();
        discount_ = rateCurve->discount(paymentDate);
        accrualFactor_ = coupon_->accrualPeriod();
        spreadLegValue_ = spread_ * accrualFactor_* discount_;

        startTime_ = coupon_->startTime();
        endTime_ = coupon_->endTime();
        observationTimes_ = coupon_->observationTimes();
        observations_ = coupon_->observations();

        const std::vector<Date>& observationDates =
            coupon_->observationsSchedule()->dates();

        QL_REQUIRE(observationDates.size()==observations_+2,
                   "incompatible size of initialValues vector");

        initialValues_ = std::vector<Real>(observationDates.size(),0.);

        observationCvg_ = std::vector<Real>(observationDates.size(),0.);

        observationIndexStartDates_ =
            std::vector<Date>(observationDates.size());
        observationIndexEndDates_ =
            std::vector<Date>(observationDates.size());

        Calendar calendar = index->fixingCalendar();

        for(Size i=0; i<observationDates.size(); i++) {
            Date fixingDate = calendar.advance(
                                 observationDates[i],
                                 -static_cast<Integer>(coupon_->fixingDays()),
                                 Days);

            initialValues_[i] =
                index->fixing(fixingDate) + coupon_->rateSpread();

            Date fixingValueDate = index->valueDate(fixingDate);
            Date endValueDate = index->maturityDate(fixingValueDate);

            observationIndexStartDates_[i] = fixingValueDate;
            observationIndexEndDates_[i] = endValueDate;

            observationCvg_[i] =
                index->dayCounter().yearFraction(fixingValueDate, endValueDate);
        }
    }

    Real SubPeriodsPricer::swapletRate() const {
        return swapletPrice()/(accrualFactor_*discount_);
    }

    Real SubPeriodsPricer::capletPrice(Rate) const {
        QL_FAIL("SubPeriodsPricer::capletPrice not implemented");
    }

    Rate SubPeriodsPricer::capletRate(Rate) const {
        QL_FAIL("SubPeriodsPricer::capletRate not implemented");
    }

    Real SubPeriodsPricer::floorletPrice(Rate) const {
        QL_FAIL("SubPeriodsPricer::floorletPrice not implemented");
    }

    Rate SubPeriodsPricer::floorletRate(Rate) const {
        QL_FAIL("SubPeriodsPricer::floorletRate not implemented");
    }

    Real AveragingRatePricer::swapletPrice() const {
        // past or future fixing is managed in InterestRateIndex::fixing()

        Size nCount = initialValues_.size();
        double dAvgRate = 0.0, dTotalCvg = 0.0, dTotalPayment = 0.0;
        for (Size i=0; i<nCount; i++) {
            dTotalPayment += initialValues_[i] * observationCvg_[i];
            dTotalCvg += observationCvg_[i];
        }

        dAvgRate =  dTotalPayment/dTotalCvg;

        Real swapletPrice = dAvgRate*coupon_->accrualPeriod()*discount_;
        return gearing_ * swapletPrice + spreadLegValue_;
    }

    Real CompoundingRatePricer::swapletPrice() const {
        // past or future fixing is managed in InterestRateIndex::fixing()

        double dNotional = 1.0;

        Size nCount = initialValues_.size();
        double dCompRate = 0.0, dTotalCvg = 0.0, dTotalPayment = 0.0;
        for (Size i=0; i<nCount; i++) {
            dTotalPayment = initialValues_[i] * observationCvg_[i]*dNotional;
            dNotional += dTotalPayment;
            dTotalCvg += observationCvg_[i];
        }

        dCompRate = (dNotional - 1.0)/dTotalCvg;

        Real swapletPrice = dCompRate*coupon_->accrualPeriod()*discount_;
        return gearing_ * swapletPrice + spreadLegValue_;
    }


}
