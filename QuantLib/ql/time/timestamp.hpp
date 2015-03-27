/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Bitquant Research Laboratories (Asia) Ltd.

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

/*! \file timestamp.hpp
    \brief Simple time stamp implementation
*/

#ifndef quantlib_timestamp_hpp
#define quantlib_timestamp_hpp

#include <ql/time/date.hpp>

namespace QuantLib {
    //! Hour number
    /*! \ingroup datetime */
    typedef Integer Hour;
    typedef Integer Minute;
    typedef Integer Second;
    typedef Integer Millisecond;


  class TimeStamp : Date  {
      public:
        //! \name constructors
        //@{
        //! Default constructor returning a null date.
        TimeStamp();
        //! More traditional constructor.
        TimeStamp(Day d, Month m, Year y, Hour h, Minute mt, Second s,
	      Millisecond ms = 0);
        TimeStamp(Date &d, Hour h=0, Minute m=0, Second s =0,
		  Millisecond ms = 0);

        Time dayFraction() const;
        //! \name static methods
        //@{
        //! now
        static TimeStamp now();
        Hour hour() const;
        Minute minute() const;
        Second second() const;
        Millisecond millisecond() const;
        BigInteger serialTimeStamp() const;
      private:
        void setSerial(Hour h, Minute mt, Second s, Millisecond ms);
        //! Milliseconds since 00:00:00 GMT
        BigInteger serialTimeStamp_;
    };
}


#endif
