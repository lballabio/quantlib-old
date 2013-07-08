/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2003, 2004, 2008 StatPro Italia srl

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

#ifndef quantlib_test_custom_utilities_hpp
#define quantlib_test_custom_utilities_hpp

#include <ql/instruments/payoffs.hpp>
#include <ql/exercise.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/volatility/equityfx/blackvoltermstructure.hpp>
#include <ql/quote.hpp>
#include <ql/patterns/observable.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
//#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/function.hpp>
#include <vector>
#include <string>
#include <numeric>
#include <iomanip>

using namespace QuantLib;

#ifndef LENGTH
#define LENGTH(a) (sizeof(a)/sizeof(a[0]))
#endif

/* the following works around a problem with Boost 1.32 where std::fixed
   and similar manipulators could not be sent to the Boost streams */
#if defined(QL_WORKING_BOOST_STREAMS)
#define QL_FIXED std::fixed
#define QL_SCIENTIFIC std::scientific
#else
#define QL_FIXED ""
#define QL_SCIENTIFIC ""
#endif

#include <iostream>
#ifndef BOOST_MESSAGE
#define BOOST_MESSAGE(message) std::cout << "\n��" << message << std::endl;
#endif
#ifndef BOOST_ERROR
#define BOOST_ERROR(errormessage) std::cout << errormessage << std::endl;
#endif
#ifndef BOOST_CHECK_MESSAGE
#define BOOST_CHECK_MESSAGE(checkcondition,errmsg) if(!(checkcondition)) std::cout << "Check error." << std::endl
#endif
#ifndef BOOST_REQUIRE_MESSAGE
#define BOOST_REQUIRE_MESSAGE(requirecondition,rqrmsg) if(!(requirecondition)) std::cout << "Requirement violated." << std::endl
#endif
/*
#ifndef BOOST_CHECK_MESSAGE
#define BOOST_CHECK_MESSAGE(condition,errmsg) if(!(condition)) std::cout << (errmsg) << std::endl
#endif
#ifndef BOOST_REQUIRE_MESSAGE
#define BOOST_REQUIRE_MESSAGE(condition,errmsg) if(!(condition)) std::cout << (errmsg) << std::endl
#endif
*/

#ifndef LARGE_TITLE
#define LARGE_TITLE(title) \
	std::cout << std::endl << std::endl;\
	std::cout << "=======================================================================" << std::endl;\
	std::cout << title << std::endl;\
	std::cout << "=======================================================================" << std::endl;
#endif
#ifndef BOOST_FAIL
#define BOOST_FAIL(failmsg) std::cout << failmsg << std::endl;
#endif


namespace QuantLib {

	std::string payoffTypeToString(const boost::shared_ptr<Payoff>&);
	std::string exerciseTypeToString(const boost::shared_ptr<Exercise>&);


	boost::shared_ptr<YieldTermStructure>
		flatRate(const Date& today,
		const boost::shared_ptr<Quote>& forward,
		const DayCounter& dc);

	boost::shared_ptr<YieldTermStructure>
		flatRate(const Date& today,
		Rate forward,
		const DayCounter& dc);

	boost::shared_ptr<YieldTermStructure>
		flatRate(const boost::shared_ptr<Quote>& forward,
		const DayCounter& dc);

	boost::shared_ptr<YieldTermStructure>
		flatRate(Rate forward,
		const DayCounter& dc);


	boost::shared_ptr<BlackVolTermStructure>
		flatVol(const Date& today,
		const boost::shared_ptr<Quote>& volatility,
		const DayCounter& dc);

	boost::shared_ptr<BlackVolTermStructure>
		flatVol(const Date& today,
		Volatility volatility,
		const DayCounter& dc);

	boost::shared_ptr<BlackVolTermStructure>
		flatVol(const boost::shared_ptr<Quote>& volatility,
		const DayCounter& dc);

	boost::shared_ptr<BlackVolTermStructure>
		flatVol(Volatility volatility,
		const DayCounter& dc);


	Real relativeError(Real x1, Real x2, Real reference);

	//bool checkAbsError(Real x1, Real x2, Real tolerance){
	//    return std::fabs(x1 - x2) < tolerance;
	//};

	class Flag : public QuantLib::Observer {
	private:
		bool up_;
	public:
		Flag() : up_(false) {}
		void raise() { up_ = true; }
		void lower() { up_ = false; }
		bool isUp() const { return up_; }
		void update() { raise(); }
	};

	template<class Iterator>
	Real norm(const Iterator& begin, const Iterator& end, Real h) {
		// squared values
		std::vector<Real> f2(end-begin);
		std::transform(begin,end,begin,f2.begin(),
			std::multiplies<Real>());
		// numeric integral of f^2
		Real I = h * (std::accumulate(f2.begin(),f2.end(),0.0)
			- 0.5*f2.front() - 0.5*f2.back());
		return std::sqrt(I);
	}


	// this cleans up index-fixing histories when destroyed
	class IndexHistoryCleaner {
	public:
		IndexHistoryCleaner();
		~IndexHistoryCleaner();
	};

}

#endif //quantlib_test_custom_utilities_hpp