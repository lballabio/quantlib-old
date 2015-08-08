/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Klaus Spanderen

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

/*! \file datetime.hpp
    \brief wrapper for the boost local date time object to allow using
    the pimpl idiom
*/

#ifndef quantlib_date_time_wrapper_hpp
#define quantlib_date_time_wrapper_hpp

#include <boost/date_time/local_time/local_time.hpp>

namespace QuantLib {
	struct DateTimeWrapper {
		boost::local_time::local_date_time localTime_;
	};
}
#endif
