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

/*! \file gsr.hpp
    \brief GSR 1 factor model
*/

#ifndef quantlib_gsr_hpp
#define quantlib_gsr_hpp

#include <ql/mathconstants.hpp>
#include <ql/time/schedule.hpp>
#include <ql/math/integrals/simpsonintegral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

#include <ql/experimental/models/onefactormodel.hpp>
#include <ql/experimental/models/gsrprocess.hpp>

#include <boost/math/special_functions.hpp>

namespace QuantLib {

    //! One factor gsr model, formulation is in forward measure

    class Gsr : public OneFactorModel, public CalibratedModel {

      public:

		// constant mean reversion
		Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T = 60.0);
		// piecewise mean reversion (with same step dates as volatilities)
		Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T = 60.0);
		// constant mean reversion (calibrated), the first dummy parameter indicates this 
        // (can be set to true or false, does not matter)
		Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T = 60.0);
		// piecewise mean reversion (with same step dates as volatilities, calibrated)
		Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T = 60.0);

		const Real numeraireTime() const; 
		const void numeraireTime(const Real T);

      protected:

        const Real numeraireImpl(const Time t, const Real y, const Handle<YieldTermStructure>& yts) const = 0;

        const Real zerobondImpl(const Time T, const Time t, const Real y, 
                                        const Handle<YieldTermStructure>& yts) const = 0;
        
		void generateArguments() {
			calculate();
			boost::dynamic_pointer_cast<GsrProcess>(stateProcess_)->flushCache();
			notifyObservers();
		}

        void update() {
            LazyObject::update();
        }

      private:

  		void initialize(Real);

		const bool calibrateReversion_;

        NullParameter dummyParameter_;

		Parameter& reversion_;
		Parameter reversionNc_; // this is used if reversion is not calibrated
		Parameter& sigma_;
		
		std::vector<Date> volstepdates_; // this is shared between vols and reverisons in case of piecewise reversions
		std::vector<Time> volsteptimes_;
		Array volsteptimesArray_; // FIXME this is redundant (just a copy of volsteptimes_)
		std::vector<Real> volatilities_;
		std::vector<Real> reversions_;

    };



    inline const Real Gsr::numeraireTime() const {
        return boost::dynamic_pointer_cast<GsrProcess>(stateProcess_)->getForwardMeasureTime();
    }

    inline const void Gsr::numeraireTime(const Real T) {
        boost::dynamic_pointer_cast<GsrProcess>(stateProcess_)->setForwardMeasureTime(T);
        calculate();
    }

}


#endif

