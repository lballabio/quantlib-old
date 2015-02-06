/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004 Jeff Yu
 Copyright (C) 2004 M-Dimension Consulting Inc.
 Copyright (C) 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2007, 2008, 2009 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2008 Simon Ibbotson

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

/*! \file bond.hpp
    \brief concrete bond class
*/

#ifndef quantlib_bond_hpp
#define quantlib_bond_hpp

#include <ql/instrument.hpp>

#include <ql/time/calendar.hpp>
#include <ql/cashflow.hpp>
#include <ql/compounding.hpp>
#include <ql/handle.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/coupon.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/termstructures/yield/zerospreadedtermstructure.hpp>
#include <iostream>
#include <vector>

namespace QuantLib {

    class DayCounter;

	template<class>
	class YieldTermStructure_t;
	typedef YieldTermStructure_t<Real> YieldTermStructure;

	template<class T>
	class Bond_t;

	namespace impl {
		template<class T>
		Date startDate(const Bond_t<T>& bond);

		template<class T>
		Date maturityDate(const Bond_t<T>& bond);

		template<class T>
		bool isTradable(const Bond_t<T>& bond, Date settlement = Date());

		template<class T>
		T cleanPrice(const Bond_t<T>& bond,
			T yield,
			const DayCounter& dayCounter,
			Compounding compounding,
			Frequency frequency,
			Date settlement = Date());

		template<class T>
		T yield(const Bond_t<T>& bond,
			T cleanPrice,
			const DayCounter& dayCounter,
			Compounding compounding,
			Frequency frequency,
			Date settlement = Date(),
			Real accuracy = 1.0e-10,
			Size maxIterations = 100,
			Rate guess = 0.05);

		template<class T>
		T zSpread(const Bond_t<T>& bond,
			T cleanPrice,
			const shared_ptr<YieldTermStructure_t<T> >& d,
			const DayCounter& dayCounter,
			Compounding compounding,
			Frequency frequency,
			Date settlement = Date(),
			Real accuracy = 1.0e-10,
			Size maxIterations = 100,
			Rate guess = 0.0);

		template<class T>
		T nextCouponRate(const Bond_t<T>& bond,Date settlement = Date());

		template<class T>
		T previousCouponRate(const Bond_t<T>& bond, Date settlement = Date());

		template<class T>
		Date nextCashFlowDate(const Bond_t<T>& bond, Date settlement = Date());

		template<class T>
		Date previousCashFlowDate(const Bond_t<T>& bond, Date settlement = Date());

	}

    //! Base bond class
    /*! Derived classes must fill the uninitialized data members.

        \warning Most methods assume that the cash flows are stored
                 sorted by date, the redemption(s) being after any
                 cash flow at the same date. In particular, if there's
                 one single redemption, it must be the last cash flow,

        \ingroup instruments

        \test
        - price/yield calculations are cross-checked for consistency.
        - price/yield calculations are checked against known good
          values.
    */
	template<class T>
    class Bond_t : public Instrument_t<T> {
      public:
        //! constructor for amortizing or non-amortizing bonds.
        /*! Redemptions and maturity are calculated from the coupon
            data, if available.  Therefore, redemptions must not be
            included in the passed cash flows.
        */

		  typedef typename Leg_t<T>::Type LegType;

		Bond_t(Natural settlementDays,
			  const Calendar& calendar,
			  const Date& issueDate = Date(),
			  const typename Leg_t<T>::Type& coupons = LegType());

        //! old constructor for non amortizing bonds.
        /*! \warning The last passed cash flow must be the bond
                     redemption. No other cash flow can have a date
                     later than the redemption date.
        */
		Bond_t(Natural settlementDays,
			const Calendar& calendar,
			Real faceAmount,
			const Date& maturityDate,
			const Date& issueDate = Date(),
			const typename Leg_t<T>::Type& cashflows = LegType());

        class arguments;
        class results;
        class engine;

        //! \name Instrument interface
        //@{
        bool isExpired() const;
        //@}
        //! \name Inspectors
        //@{
        Natural settlementDays() const;
        const Calendar& calendar() const;

        const std::vector<T>& notionals() const;
        virtual T notional(Date d = Date()) const;

        /*! \note returns all the cashflows, including the redemptions. */
		const typename Leg_t<T>::Type& cashflows() const { return this->cashflows_; }
        /*! returns just the redemption flows (not interest payments) */
		const typename Leg_t<T>::Type& redemptions() const { return this->redemptions_; }
		/*! return just the coupon payments */
		typename Leg_t<T>::Type couponflows() const {
			typename Leg_t<T>::Type coupons;
			if (!cashflows_.empty()) {
				coupons.push_back(cashflows_[0]);

				for (Size i = 1; i<cashflows_.size(); ++i) {
					if (cashflows_[i]->date() == cashflows_[i - 1]->date())
						continue;
					coupons.push_back(cashflows_[i]);
				}
			}
			return coupons;
		}
        /*! returns the redemption, if only one is defined */
        const boost::shared_ptr<CashFlow_t<T> >& redemption() const;

        Date startDate() const;
        Date maturityDate() const;
        Date issueDate() const;

        bool isTradable(Date d = Date()) const;
        Date settlementDate(Date d = Date()) const;
        //@}

        //! \name Calculations
        //@{

        //! theoretical clean price
        /*! The default bond settlement is used for calculation.

            \warning the theoretical price calculated from a flat term
                     structure might differ slightly from the price
                     calculated from the corresponding yield by means
                     of the other overload of this function. If the
                     price from a constant yield is desired, it is
                     advisable to use such other overload.
        */
        T cleanPrice() const;

        //! theoretical dirty price
        /*! The default bond settlement is used for calculation.

            \warning the theoretical price calculated from a flat term
                     structure might differ slightly from the price
                     calculated from the corresponding yield by means
                     of the other overload of this function. If the
                     price from a constant yield is desired, it is
                     advisable to use such other overload.
        */
        T dirtyPrice() const;

        //! theoretical settlement value
        /*! The default bond settlement date is used for calculation. */
        T settlementValue() const;

        //! theoretical bond yield
        /*! The default bond settlement and theoretical price are used
            for calculation.
        */
        T yield(const DayCounter& dc,
                   Compounding comp,
                   Frequency freq,
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) const;

        //! clean price given a yield and settlement date
        /*! The default bond settlement is used if no date is given. */
        T cleanPrice(T yield,
                        const DayCounter& dc,
                        Compounding comp,
                        Frequency freq,
                        Date settlementDate = Date()) const;

        //! dirty price given a yield and settlement date
        /*! The default bond settlement is used if no date is given. */
        T dirtyPrice(T yield,
                        const DayCounter& dc,
                        Compounding comp,
                        Frequency freq,
                        Date settlementDate = Date()) const;

        //! settlement value as a function of the clean price
        /*! The default bond settlement date is used for calculation. */
        T settlementValue(T cleanPrice) const;

        //! yield given a (clean) price and settlement date
        /*! The default bond settlement is used if no date is given. */
        T yield(T cleanPrice,
                   const DayCounter& dc,
                   Compounding comp,
                   Frequency freq,
                   Date settlementDate = Date(),
                   Real accuracy = 1.0e-8,
                   Size maxEvaluations = 100) const;

		//! Zero spread calculation
		virtual T zSpread(T cleanPrice,
			                   const boost::shared_ptr<YieldTermStructure_t<T> >& yieldCurve,
							   const DayCounter& dayCounter,
							   Compounding compounding,
							   Frequency frequency,
							   Date settlementDate = Date(),
							   Real accuracy = 1.0e-10,
							   Size maxIterations = 100,
							   Rate guess = 0.0);

        //! accrued amount at a given date
        /*! The default bond settlement is used if no date is given. */
        virtual T accruedAmount(Date d = Date()) const;
        //@}

        /*! Expected next coupon: depending on (the bond and) the given date
            the coupon can be historic, deterministic or expected in a
            stochastic sense. When the bond settlement date is used the coupon
            is the already-fixed not-yet-paid one.

            The current bond settlement is used if no date is given.
        */
        virtual T nextCouponRate(Date d = Date()) const;

        //! Previous coupon already paid at a given date
        /*! Expected previous coupon: depending on (the bond and) the given
            date the coupon can be historic, deterministic or expected in a
            stochastic sense. When the bond settlement date is used the coupon
            is the last paid one.

            The current bond settlement is used if no date is given.
        */
        T previousCouponRate(Date d = Date()) const;

        Date nextCashFlowDate(Date d = Date()) const;
        Date previousCashFlowDate(Date d = Date()) const;

      protected:
        void setupExpired() const;
        void setupArguments(PricingEngine::arguments*) const;
        void fetchResults(const PricingEngine::results*) const;

        /*! This method can be called by derived classes in order to
            build redemption payments from the existing cash flows.
            It must be called after setting up the cashflows_ vector
            and will fill the notionalSchedule_, notionals_, and
            redemptions_ data members.

            If given, the elements of the redemptions vector will
            multiply the amount of the redemption cash flow.  The
            elements will be taken in base 100, i.e., a redemption
            equal to 100 does not modify the amount.

            \pre The cashflows_ vector must contain at least one
                 coupon and must be sorted by date.
        */
        void addRedemptionsToCashflows(const std::vector<T>& redemptions
                                                       = std::vector<T>());

        /*! This method can be called by derived classes in order to
            build a bond with a single redemption payment.  It will
            fill the notionalSchedule_, notionals_, and redemptions_
            data members.
        */
        void setSingleRedemption(T notional,
                                 T redemption,
                                 const Date& date);

        /*! This method can be called by derived classes in order to
            build a bond with a single redemption payment.  It will
            fill the notionalSchedule_, notionals_, and redemptions_
            data members.
        */
        void setSingleRedemption(T notional,
                                 const boost::shared_ptr<CashFlow_t<T> >& redemption);

        /*! used internally to collect notional information from the
            coupons. It should not be called by derived classes,
            unless they already provide redemption cash flows (in
            which case they must set up the redemptions_ data member
            independently).  It will fill the notionalSchedule_ and
            notionals_ data members.
        */
        void calculateNotionalsFromCashflows();

        Natural settlementDays_;
        Calendar calendar_;
        std::vector<Date> notionalSchedule_;
        std::vector<T> notionals_;
		typename Leg_t<T>::Type cashflows_; // all cashflows
		typename Leg_t<T>::Type redemptions_; // the redemptions

        Date maturityDate_, issueDate_;
        mutable T settlementValue_;
    };

	template<class T>
    class Bond_t<T>::arguments : public PricingEngine::arguments {
      public:
        Date settlementDate;
        typename Leg_t<T>::Type cashflows;
        Calendar calendar;
        void validate() const;
    };

	template<class T>
    class Bond_t<T>::results : public Instrument_t<T>::results {
      public:
        T settlementValue;
        void reset() {
            settlementValue = Null<Real>();
            Instrument_t<T>::results::reset();
        }
    };

	template<class T>
    class Bond_t<T>::engine : public GenericEngine<typename Bond_t<T>::arguments,
                                              typename Bond_t<T>::results> {};

	typedef Bond_t<double> Bond;

    // implementation
	
	template<class T>
    inline Natural Bond_t<T>::settlementDays() const {
        return settlementDays_;
    }

	template<class T>
    inline const Calendar& Bond_t<T>::calendar() const {
        return calendar_;
    }

	template<class T>
    inline const std::vector<T>& Bond_t<T>::notionals() const {
        return notionals_;
    }

	template<class T>
    inline Date Bond_t<T>::issueDate() const {
        return issueDate_;
    }

	template<class T>
	Bond_t<T>::Bond_t(Natural settlementDays,
		const Calendar& calendar,
		const Date& issueDate,
		const typename Leg_t<T>::Type& coupons)
		: settlementDays_(settlementDays), calendar_(calendar),
		cashflows_(coupons), issueDate_(issueDate) {

			if (!coupons.empty()) {
				std::sort(cashflows_.begin(), cashflows_.end(),
					earlier_than<shared_ptr<CashFlow_t<T> > >());

				if (issueDate_ != Date()) {
					QL_REQUIRE(issueDate_<cashflows_[0]->date(),
						"issue date (" << issueDate_ <<
						") must be earlier than first payment date (" <<
						cashflows_[0]->date() << ")");
				}

				maturityDate_ = coupons.back()->date();

				addRedemptionsToCashflows();
			}

			this->registerWith(Settings::instance().evaluationDate());
	}

	template<class T>
	Bond_t<T>::Bond_t(Natural settlementDays,
		const Calendar& calendar,
		Real faceAmount,
		const Date& maturityDate,
		const Date& issueDate,
		const typename Leg_t<T>::Type& cashflows)
		: settlementDays_(settlementDays), calendar_(calendar),
		cashflows_(cashflows), maturityDate_(maturityDate),
		issueDate_(issueDate) {

			if (!cashflows.empty()) {

				std::sort(cashflows_.begin(), cashflows_.end()-1,
					earlier_than<shared_ptr<CashFlow_t<T> > >());

				if (maturityDate_ == Date())
					maturityDate_ = CashFlows::maturityDate<T>(cashflows);

				if (issueDate_ != Date()) {
					QL_REQUIRE(issueDate_<cashflows_[0]->date(),
						"issue date (" << issueDate_ <<
						") must be earlier than first payment date (" <<
						cashflows_[0]->date() << ")");
				}

				notionals_.resize(2);
				notionalSchedule_.resize(2);

				notionalSchedule_[0] = Date();
				notionals_[0] = faceAmount;

				notionalSchedule_[1] = maturityDate_;
				notionals_[1] = 0.0;

				redemptions_.push_back(cashflows.back());
			}

			this->registerWith(Settings::instance().evaluationDate());
	}

	template<class T>
	bool Bond_t<T>::isExpired() const {
		// this is the Instrument interface, so it doesn't use
		// BondFunctions, and includeSettlementDateFlows is true
		// (unless QL_TODAY_PAYMENTS will set it to false later on)
		return CashFlows::isExpired<T>(cashflows_,
			true,
			Settings::instance().evaluationDate());
	}

	template<class T>
	T Bond_t<T>::notional(Date d) const {
		if (d == Date())
			d = settlementDate();

		if (d > notionalSchedule_.back()) {
			// after maturity
			return 0.0;
		}

		// After the check above, d is between the schedule
		// boundaries.  We search starting from the second notional
		// date, since the first is null.  After the call to
		// lower_bound, *i is the earliest date which is greater or
		// equal than d.  Its index is greater or equal to 1.
		std::vector<Date>::const_iterator i =
			std::lower_bound(notionalSchedule_.begin()+1,
			notionalSchedule_.end(), d);
		Size index = std::distance(notionalSchedule_.begin(), i);

		if (d < notionalSchedule_[index]) {
			// no doubt about what to return
			return notionals_[index-1];
		} else {
			// d is equal to a redemption date.
			// As per bond conventions, the payment has occurred;
			// the bond already changed notional.
			return notionals_[index];
		}
	}

	template<class T>
	const boost::shared_ptr<CashFlow_t<T> >& Bond_t<T>::redemption() const {
		QL_REQUIRE(redemptions_.size() == 1,
			"multiple redemption cash flows given");
		return redemptions_.back();
	}

	template<class T>
	Date Bond_t<T>::startDate() const {
		return impl::startDate<T>(*this);
	}

	template<class T>
	Date Bond_t<T>::maturityDate() const {
		if (maturityDate_!=Null<Date>())
			return maturityDate_;
		else
			return impl::maturityDate<T>(*this);
	}

	template<class T>
	bool Bond_t<T>::isTradable(Date d) const {
		return impl::isTradable<T>(*this, d);
	}

	template<class T>
	Date Bond_t<T>::settlementDate(Date d) const {
		if (d==Date())
			d = Settings::instance().evaluationDate();

		// usually, the settlement is at T+n...
		Date settlement = calendar_.advance(d, settlementDays_, Days);
		// ...but the bond won't be traded until the issue date (if given.)
		if (issueDate_ == Date())
			return settlement;
		else
			return std::max(settlement, issueDate_);
	}

	template<class T>
	T Bond_t<T>::cleanPrice() const {

		return dirtyPrice() - accruedAmount(settlementDate());
	}

	template<class T>
	T Bond_t<T>::dirtyPrice() const {
		T currentNotional = notional(settlementDate());
		if (currentNotional == 0.0)
			return 0.0;
		else
			return settlementValue()*100.0/currentNotional;
	}

	template<class T>
	T Bond_t<T>::settlementValue() const {
		LazyObject::calculate();
		QL_REQUIRE(settlementValue_ != Null<Real>(),
			"settlement value not provided");
		return settlementValue_;
	}

	template<class T>
	T Bond_t<T>::settlementValue(T cleanPrice) const {
		T dirtyPrice = cleanPrice + accruedAmount(settlementDate());
		return dirtyPrice / 100.0 * notional(settlementDate());
	}

	template<class T>
	T Bond_t<T>::yield(const DayCounter& dc,
		Compounding comp,
		Frequency freq,
		Real accuracy,
		Size maxEvaluations) const {
			T currentNotional = notional(settlementDate());
			if (currentNotional == 0.0)
				return 0.0;

			return impl::yield<T>(*this, cleanPrice(), dc, comp, freq,
				settlementDate(),
				accuracy, maxEvaluations);
	}

	template<class T>
	T Bond_t<T>::cleanPrice(T y,
		const DayCounter& dc,
		Compounding comp,
		Frequency freq,
		Date settlement) const {
			return impl::cleanPrice<T>(*this, y, dc, comp, freq, settlement);
	}

	template<class T>
	T Bond_t<T>::dirtyPrice(T y,
		const DayCounter& dc,
		Compounding comp,
		Frequency freq,
		Date settlement) const {
			T currentNotional = notional(settlement);
			if (currentNotional == 0.0)
				return 0.0;

			return impl::cleanPrice<T>(*this, y, dc, comp, freq, settlement)
				+ accruedAmount(settlement);
	}

	template<class T>
	T Bond_t<T>::yield(T cleanPrice,
		const DayCounter& dc,
		Compounding comp,
		Frequency freq,
		Date settlement,
		Real accuracy,
		Size maxEvaluations) const {
			T currentNotional = notional(settlement);
			if (currentNotional == 0.0)
				return 0.0;

			return impl::yield<T>(*this, cleanPrice, dc, comp, freq,
				settlement, accuracy, maxEvaluations);
	}

	template<class T>
	T Bond_t<T>::zSpread(T cleanPrice, 
		const boost::shared_ptr<YieldTermStructure_t<T> >& yieldCurve, 
		const DayCounter& dayCounter, 
		Compounding compounding, 
		Frequency frequency, 
		Date settlementDate,
		Real accuracy,
		Size maxIterations,
		Rate guess) {
			return impl::zSpread<T>(*this, cleanPrice, yieldCurve, dayCounter, compounding,
				frequency, settlementDate, accuracy, maxIterations, guess);
	}

	template<class T>
	T Bond_t<T>::accruedAmount(Date settlement) const {
		T currentNotional = notional(settlement);
		if (currentNotional == 0.0)
			return 0.0;

		if (settlement == Date())
			settlement = settlementDate();

		QL_REQUIRE(impl::isTradable<T>(*this, settlement),
			"non tradable at " << settlement <<
			" (maturity being " << maturityDate() << ")");

		return CashFlows::accruedAmount<T>(this->cashflows(),
			false, settlement) *
			100.0 / notional(settlement);
	}

	template<class T>
	T Bond_t<T>::nextCouponRate(Date settlement) const {
		return impl::nextCouponRate<T>(*this, settlement);
	}

	template<class T>
	T Bond_t<T>::previousCouponRate(Date settlement) const {
		return impl::previousCouponRate<T>(*this, settlement);
	}

	template<class T>
	Date Bond_t<T>::nextCashFlowDate(Date settlement) const {
		return impl::nextCashFlowDate<T>(*this, settlement);
	}

	template<class T>
	Date Bond_t<T>::previousCashFlowDate(Date settlement) const {
		return impl::previousCashFlowDate<T>(*this, settlement);
	}

	template<class T>
	void Bond_t<T>::setupExpired() const {
		Instrument_t<T>::setupExpired();
		settlementValue_ = 0.0;
	}

	template<class T>
	void Bond_t<T>::setupArguments(PricingEngine::arguments* args) const {
		Bond_t<T>::arguments* arguments = dynamic_cast<Bond_t<T>::arguments*>(args);
		QL_REQUIRE(arguments != 0, "wrong argument type");

		arguments->settlementDate = settlementDate();
		arguments->cashflows = cashflows_;
		arguments->calendar = calendar_;
	}

	template<class T>
	void Bond_t<T>::fetchResults(const PricingEngine::results* r) const {

		Instrument_t<T>::fetchResults(r);

		const Bond_t<T>::results* results =
			dynamic_cast<const Bond_t<T>::results*>(r);
		QL_ENSURE(results != 0, "wrong result type");

		this->settlementValue_ = results->settlementValue;
	}

	template<class T>
	void Bond_t<T>::addRedemptionsToCashflows(const std::vector<T>& redemptions) {
		// First, we gather the notional information from the cashflows
		calculateNotionalsFromCashflows();
		// Then, we create the redemptions based on the notional
		// information and we add them to the cashflows vector after
		// the coupons.
		this->redemptions_.clear();
        // to do. Need adjustment for AD<double>
		for (Size i=1; i<notionalSchedule_.size(); ++i) {
			T R = i < redemptions.size() ? redemptions[i] :
				!redemptions.empty()   ? redemptions.back() :
				100.0;
		T amount = (R/100.0)*(notionals_[i-1]-notionals_[i]);
		boost::shared_ptr<CashFlow_t<T> > payment;
		if (i < this->notionalSchedule_.size()-1)
			payment.reset(new AmortizingPayment_t<T>(amount,
			notionalSchedule_[i]));
		else
			payment.reset(new Redemption_t<T>(amount, this->notionalSchedule_[i]));
		this->cashflows_.push_back(payment);
		this->redemptions_.push_back(payment);
		}
		// stable_sort now moves the redemptions to the right places
		// while ensuring that they follow coupons with the same date.
		std::stable_sort(this->cashflows_.begin(), this->cashflows_.end(),
			earlier_than<shared_ptr<CashFlow_t<T> > >());
	}

	template<class T>
	void Bond_t<T>::setSingleRedemption(T notional,
		T redemption,
		const Date& date) {

		boost::shared_ptr<CashFlow_t<T> > redemptionCashflow(
				new Redemption_t<T>(notional*redemption/100.0, date));
		setSingleRedemption(notional, redemptionCashflow);
	}

	template<class T>
	void Bond_t<T>::setSingleRedemption(T notional,
		const boost::shared_ptr<CashFlow_t<T> >& redemption) {
			this->notionals_.resize(2);
			this->notionalSchedule_.resize(2);
			this->redemptions_.clear();

			this->notionalSchedule_[0] = Date();
			this->notionals_[0] = notional;

			this->notionalSchedule_[1] = redemption->date();
			this->notionals_[1] = 0.0;

			this->cashflows_.push_back(redemption);
			this->redemptions_.push_back(redemption);
	}

	template<class T>
	void Bond_t<T>::calculateNotionalsFromCashflows() {
		this->notionalSchedule_.clear();
		this->notionals_.clear();

		Date lastPaymentDate = Date();
		this->notionalSchedule_.push_back(Date());
		for (Size i = 0; i<this->cashflows_.size(); ++i) {
			boost::shared_ptr<Coupon_t<T> > coupon =
				boost::dynamic_pointer_cast<Coupon_t<T> >(this->cashflows_[i]);
			if (!coupon)
				continue;

			T notional = coupon->nominal();
			// we add the notional only if it is the first one...
			if (this->notionals_.empty()) {
				this->notionals_.push_back(coupon->nominal());
				lastPaymentDate = coupon->date();
			}
			else if (!close(notional, this->notionals_.back())) {
				// ...or if it has changed.
				QL_REQUIRE(notional < notionals_.back(),
					"increasing coupon notionals");
				this->notionals_.push_back(coupon->nominal());
				// in this case, we also add the last valid date for
				// the previous one...
				this->notionalSchedule_.push_back(lastPaymentDate);
				// ...and store the candidate for this one.
				lastPaymentDate = coupon->date();
			} else {
				// otherwise, we just extend the valid range of dates
				// for the current notional.
				lastPaymentDate = coupon->date();
			}
		}
		QL_REQUIRE(!(this->notionals_.empty()), "no coupons provided");
		this->notionals_.push_back(0.0);
		this->notionalSchedule_.push_back(lastPaymentDate);
	}

	template<class T>
	void Bond_t<T>::arguments::validate() const {
		QL_REQUIRE(settlementDate != Date(), "no settlement date provided");
		QL_REQUIRE(!cashflows.empty(), "no cash flow provided");
		for (Size i=0; i<cashflows.size(); ++i)
			QL_REQUIRE(cashflows[i], "null cash flow provided");
	}

}

#include <ql/pricingengines/bond/bondfunctionsimpl.hpp>

#endif
