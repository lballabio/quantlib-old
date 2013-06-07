/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file cmsswap.hpp
    \brief cms vs libor swap
	If no payment convention is given, the floating leg schedule convention is used
	The swap index and the ibor index should be linked to valid forwarding and discounting curves
*/

#ifndef quantlib_cms_swap_hpp
#define quantlib_cms_swap_hpp

#include <ql/instruments/swap.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/time/schedule.hpp>
#include <boost/optional.hpp>

namespace QuantLib {

    class IborIndex;
	class SwapIndex;

    //! cms swap: cms vs libor leg

    class CmsSwap : public Swap {
      public:
        enum Type { Receiver = -1, Payer = 1 }; // this refers to the structured (i.e. cms) coupon leg
        class arguments;
        class results;
        class engine;
        CmsSwap(
            Type type,
            Real nominal,
            const Schedule& structuredSchedule,
			const boost::shared_ptr<SwapIndex>& swapIndex,
            Spread structuredSpread,
			Rate cappedRate,
			Rate flooredRate,
            const DayCounter& structuredDayCount,
            const Schedule& floatSchedule,
            const boost::shared_ptr<IborIndex>& iborIndex,
            Spread spread,
            const DayCounter& floatingDayCount,
            boost::optional<BusinessDayConvention> paymentConvention =
                                                                 boost::none);
        //! \name Inspectors
        //@{
        Type type() const;
        Real nominal() const;

        const Schedule& structuredSchedule() const;
        Spread structuredSpread() const;
		const boost::shared_ptr<SwapIndex>& swapIndex() const;
        const DayCounter& structuredDayCount() const;
		Rate cappedRate() const;
		Rate flooredRate() const;

        const Schedule& floatingSchedule() const;
        const boost::shared_ptr<IborIndex>& iborIndex() const;
        Spread spread() const;
        const DayCounter& floatingDayCount() const;

        BusinessDayConvention paymentConvention() const;

        const Leg& structuredLeg() const;
        const Leg& floatingLeg() const;
        //@}

        //! \name Results
        //@{
		const Spread fairStructuredSpread() const;
        //@}
        // other
        void setupArguments(PricingEngine::arguments* args) const;
        void fetchResults(const PricingEngine::results*) const;
      private:
        void setupExpired() const;
        Type type_;
        Real nominal_;
        Schedule structuredSchedule_;
		boost::shared_ptr<SwapIndex> swapIndex_;
        Spread structuredSpread_;
		Rate cappedRate_, flooredRate_;
        DayCounter structuredDayCount_;
        Schedule floatingSchedule_;
        boost::shared_ptr<IborIndex> iborIndex_;
        Spread spread_;
        DayCounter floatingDayCount_;
        BusinessDayConvention paymentConvention_;
        // results
		mutable Spread fairStructuredSpread_;
    };


    //! %Arguments for cms swap calculation
    class CmsSwap::arguments : public Swap::arguments {
      public:
        arguments() : type(Receiver),
                      nominal(Null<Real>()) {}
        Type type;
        Real nominal;

		std::vector<Time> structuredAccrualTimes;
        std::vector<Date> structuredResetDates;
        std::vector<Date> structuredFixingDates;
        std::vector<Date> structuredPayDates;

		std::vector<Time> floatingAccrualTimes;
        std::vector<Date> floatingResetDates;
        std::vector<Date> floatingFixingDates;
        std::vector<Date> floatingPayDates;

        std::vector<Spread> structuredSpreads;
		std::vector<Rate> structuredCappedRates;
		std::vector<Rate> structuredFlooredRates;

        std::vector<Spread> floatingSpreads;
        void validate() const;
    };

    //! %Results from cms swap calculation
    class CmsSwap::results : public Swap::results {
      public:
		Spread fairStructuredSpread;
        void reset();
    };

    class CmsSwap::engine : public GenericEngine<CmsSwap::arguments,
                                                     CmsSwap::results> {};

    // inline definitions

    inline CmsSwap::Type CmsSwap::type() const {
        return type_;
    }

    inline Real CmsSwap::nominal() const {
        return nominal_;
    }

    inline const Schedule& CmsSwap::structuredSchedule() const {
        return structuredSchedule_;
    }

	inline const boost::shared_ptr<SwapIndex>& CmsSwap::swapIndex() const {
		return swapIndex_;
	}

    inline Spread CmsSwap::structuredSpread() const {
        return structuredSpread_;
    }

	inline Rate CmsSwap::cappedRate() const {
        return cappedRate_;
    }
    
	inline Rate CmsSwap::flooredRate() const {
        return flooredRate_;
    }

    inline const DayCounter& CmsSwap::structuredDayCount() const {
        return structuredDayCount_;
    }

    inline const Schedule& CmsSwap::floatingSchedule() const {
        return floatingSchedule_;
    }

    inline const boost::shared_ptr<IborIndex>& CmsSwap::iborIndex() const {
        return iborIndex_;
    }

    inline Spread CmsSwap::spread() const {
        return spread_;
    }

    inline const DayCounter& CmsSwap::floatingDayCount() const {
        return floatingDayCount_;
    }

    inline BusinessDayConvention CmsSwap::paymentConvention() const {
        return paymentConvention_;
    }

    inline const Leg& CmsSwap::structuredLeg() const {
        return legs_[0];
    }

    inline const Leg& CmsSwap::floatingLeg() const {
        return legs_[1];
    }

	inline const Real CmsSwap::fairStructuredSpread() const {
		return fairStructuredSpread_;
	}

}

#endif
