/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2004, 2005, 2006 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 Toyin Akin
 Copyright (C) 2015 Klaus Spanderen

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

/*! \file date.hpp
    \brief Date- and time-related classes, typedefs and enumerations
*/

#ifndef quantlib_date_hpp
#define quantlib_date_hpp

#include <ql/time/period.hpp>
#include <ql/time/weekday.hpp>
#include <ql/utilities/null.hpp>
#include <utility>
#include <functional>

#include <boost/date_time/posix_time/ptime.hpp>

namespace QuantLib {

    //! Day number
    /*! \ingroup Date */
    typedef Integer Day;

    //! Month names
    /*! \ingroup Date */
    enum Month { January   = 1,
                 February  = 2,
                 March     = 3,
                 April     = 4,
                 May       = 5,
                 June      = 6,
                 July      = 7,
                 August    = 8,
                 September = 9,
                 October   = 10,
                 November  = 11,
                 December  = 12,
                 Jan = 1,
                 Feb = 2,
                 Mar = 3,
                 Apr = 4,
                 Jun = 6,
                 Jul = 7,
                 Aug = 8,
                 Sep = 9,
                 Oct = 10,
                 Nov = 11,
                 Dec = 12
    };

    /*! \relates Month */
    std::ostream& operator<<(std::ostream&, Month);

    //! Year number
    /*! \ingroup Date */
    typedef Integer Year;

    //! Concrete Date class
    /*! This class provides methods to inspect Dates as well as methods and
        operators which implement a limited Date algebra (increasing and
        decreasing Dates, and calculating their difference).

        \ingroup Date

        \test self-consistency of Dates, serial numbers, days of
              month, months, and weekdays is checked over the whole
              Date range.
    */

    class Date {
      public:
        //! \name constructors
        //@{
        //! Default constructor returning a null Date.
        Date();

        //! Constructor taking boost posix Date time object
        explicit Date(const boost::posix_time::ptime& localTime);
        //! More traditional constructor.
        Date(Day d, Month m, Year y);
        Date(Day d, Month m, Year y,
                 Size hours, Size minutes, Size seconds,
                 Size millisec = 0, Size microsec = 0);
        //! Constructor taking a serial number as given by Applix or Excel.
        explicit Date(BigInteger serialNumber);
        //@}

        //! \name inspectors
        //@{
        Weekday weekday() const;
        Day dayOfMonth() const;
        //! One-based (Jan 1st = 1)
        Day dayOfYear() const;
        Month month() const;
        Year year() const;
        Integer hours() const;
        Integer minutes() const;
        Integer seconds() const;
        Time fractionOfDay() const;
        Time fractionOfSecond() const;
        BigInteger serialNumber() const;
        //@}

        //! \name date algebra
        //@{
        //! increments date by the given number of days
        Date& operator+=(BigInteger days);
        //! increments date by the given period
        Date& operator+=(const Period&);
        //! decrement date by the given number of days
        Date& operator-=(BigInteger days);
        //! decrements date by the given period
        Date& operator-=(const Period&);
        //! 1-day pre-increment
        Date& operator++();
        //! 1-day post-increment
        Date operator++(int );
        //! 1-day pre-decrement
        Date& operator--();
        //! 1-day post-decrement
        Date operator--(int );
        //! returns a new date incremented by the given number of days
        Date operator+(BigInteger days) const;
        //! returns a new date incremented by the given period
        Date operator+(const Period&) const;
        //! returns a new date decremented by the given number of days
        Date operator-(BigInteger days) const;
        //! returns a new date decremented by the given period
        Date operator-(const Period&) const;
        //@}

        //! \name static methods
        //@{
        //! today's date.
        static Date todaysDate();
        //! local date time, based on the time zone settings of the computer
        static Date localDateTime();
        //! UTC date time
        static Date universalDateTime();
        //! earliest allowed date
        static Date minDate();
        //! latest allowed date
        static Date maxDate();
        //! whether the given year is a leap one
        static bool isLeap(Year y);
        //! last day of the month to which the given date belongs
        static Date endOfMonth(const Date& d);
        //! whether a date is the last day of its month
        static bool isEndOfMonth(const Date& d);
        //! next given weekday following or equal to the given date
        /*! E.g., the Friday following Tuesday, January 15th, 2002
            was January 18th, 2002.

            see http://www.cpearson.com/excel/DateWS.htm
        */
        static Date nextWeekday(const Date& d,
                                Weekday w);
        //! n-th given weekday in the given month and year
        /*! E.g., the 4th Thursday of March, 1998 was March 26th,
            1998.

            see http://www.cpearson.com/excel/DateWS.htm
        */
        static Date nthWeekday(Size n,
                                      Weekday w,
                                      Month m,
                                      Year y);
        //@}

        const boost::posix_time::ptime& dateTime() const;

      private:
        boost::posix_time::ptime dateTime_;
    };

    /*! \relates Date
        \brief Difference in days between dates
    */
    BigInteger operator-(const Date&, const Date&);
    /*! \relates Date
        \brief Difference in days (including fraction of days) between dates
    */
    Time daysBetween(const Date&, const Date&);

    /*! \relates Date */
    bool operator==(const Date&, const Date&);
    /*! \relates Date */
    bool operator!=(const Date&, const Date&);
    /*! \relates Date */
    bool operator<(const Date&, const Date&);
    /*! \relates Date */
    bool operator<=(const Date&, const Date&);
    /*! \relates Date */
    bool operator>(const Date&, const Date&);
    /*! \relates Date */
    bool operator>=(const Date&, const Date&);

    /*! \relates Date */
    std::ostream& operator<<(std::ostream&, const Date&);

    namespace detail {

        struct short_date_holder {
            short_date_holder(const Date d) : d(d) {}
            Date d;
        };
        std::ostream& operator<<(std::ostream&, const short_date_holder&);

        struct long_date_holder {
            long_date_holder(const Date& d) : d(d) {}
            Date d;
        };
        std::ostream& operator<<(std::ostream&, const long_date_holder&);

        struct iso_date_holder {
            iso_date_holder(const Date& d) : d(d) {}
            Date d;
        };
        std::ostream& operator<<(std::ostream&, const iso_date_holder&);

        struct formatted_date_holder {
            formatted_date_holder(const Date& d, const std::string& f)
            : d(d), f(f) {}
            Date d;
            std::string f;
        };
        std::ostream& operator<<(std::ostream&,
                                 const formatted_date_holder&);

    }

    namespace io {

        //! output Dates in short format (mm/dd/yyyy)
        /*! \ingroup manips */
        detail::short_date_holder short_date(const Date&);

        //! output Dates in long format (Month ddth, yyyy)
        /*! \ingroup manips */
        detail::long_date_holder long_date(const Date&);

        //! output Dates in ISO format (yyyy-mm-dd)
        /*! \ingroup manips */
        detail::iso_date_holder iso_date(const Date&);

        //! output Dates in user defined format using boost Date functionality
        /*! \ingroup manips */
        detail::formatted_date_holder formatted_Date(const Date&,
                                                     const std::string& fmt);

    }

    //! specialization of Null template for the Date class
    template <>
    class Null<Date> {
      public:
        Null() {}
        operator Date() const { return Date(); }
    };
}


#endif
