/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
 Copyright (C) 2004, 2005, 2006 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 Toyin Akin
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

#include <ql/errors.hpp>
#include <ql/time/datetime.hpp>
#include <ql/utilities/dataformatters.hpp>

#include <iomanip>

using namespace boost::posix_time;
using namespace boost::local_time;

namespace QuantLib {
	namespace {

		boost::gregorian::date serialNumberDateReference() {
			static const boost::gregorian::date dateReference(
				1899, boost::gregorian::Dec, 30);
			return dateReference;
		}

		BigInteger minimumSerialNumber() {
			return 367;       // Jan 1st, 1901
		}

		BigInteger maximumSerialNumber() {
			return 109574;    // Dec 31st, 2199
		}

	    Datetime minSerialNumberDatetime() {
	        static const Datetime minimumDatetime(minimumSerialNumber());
	        return minimumDatetime;
	    }

	    Datetime maxSerialNumberDatetime() {
	        static const Datetime maximumDatetime(maximumSerialNumber());
	        return maximumDatetime;
	    }

		void checkSerialNumber(BigInteger serialNumber) {
			QL_REQUIRE(serialNumber >= minimumSerialNumber() &&
					   serialNumber <= maximumSerialNumber(),
					   "Date's serial number (" << serialNumber << ") outside "
					   "allowed range [" << minimumSerialNumber() <<
					   "-" << maximumSerialNumber() << "], i.e. [" <<
					   minSerialNumberDatetime() << "-" <<
					   maxSerialNumberDatetime() << "]");
		}

		Weekday mapBoostDateType2QL(boost::gregorian::greg_weekday d) {
			switch (d) {
			  case boost::date_time::Monday   : return Monday;
			  case boost::date_time::Tuesday  : return Tuesday;
			  case boost::date_time::Wednesday: return Wednesday;
			  case boost::date_time::Thursday : return Thursday;
			  case boost::date_time::Friday   : return Friday;
			  case boost::date_time::Saturday : return Saturday;
			  case boost::date_time::Sunday   : return Sunday;
			  default:
				QL_FAIL("unknown boost date_time day of week given");
			}
		}


		void advance(local_date_time& dt, Integer n, TimeUnit units) {
			using boost::gregorian::gregorian_calendar;

			switch (units) {
	          case Days:
	        	dt += boost::gregorian::days(n);
	        	break;
	          case Weeks:
	        	dt += boost::gregorian::weeks(n);
	        	break;
	          case Months:
	          case Years : {
	        	  const boost::gregorian::date date = dt.date();
	        	  const Day eoM = gregorian_calendar::end_of_month_day(
	        		  date.year(), date.month());

	        	  if (units == Months) {
	        		  dt += boost::gregorian::months(n);
	        	  }
	        	  else {
	        		  dt += boost::gregorian::years(n);
	        	  }

	        	  if (date.day() == eoM) {
	        		  // avoid snap-to-end-of-month
	        		  // behavior of boost::date_time
	        		  const Day newEoM
	        		  	  = gregorian_calendar::end_of_month_day(
	        		  			dt.date().year(), dt.date().month());

	        		  if (newEoM > eoM) {
	        			  dt -= boost::gregorian::days(newEoM - eoM);
	        		  }
	        	  }
 	            }
	            break;
	          default:
	            QL_FAIL("undefined time units");
	       }
	    }
	}


	Datetime::Datetime()
	: localTime_(not_a_date_time) {}

    Datetime::Datetime(const boost::local_time::local_date_time& localTime)
    : localTime_(localTime) {}

    Datetime::Datetime(Day d, Month m, Year y)
    : localTime_(boost::gregorian::date(y, m, d),
    			 time_duration(),
    			 boost::shared_ptr<time_zone>(),
    			 local_date_time::NOT_DATE_TIME_ON_ERROR) {}

    Datetime::Datetime(Day d, Month m, Year y,
    		 	 	   Size h, Size min, Size s,
    		           Size milli, Size micro)
    : localTime_(boost::gregorian::date(y, m, d),
    		     hours(h) + minutes(min) + seconds(s) +
    		     milliseconds(milli) + microseconds(micro),
    		     boost::shared_ptr<time_zone>(),
    			 local_date_time::NOT_DATE_TIME_ON_ERROR) {}

    Datetime::Datetime(BigInteger serialNumber)
    : localTime_(serialNumberDateReference()
    				+ boost::gregorian::days(serialNumber),
    			 time_duration(),
    		     boost::shared_ptr<time_zone>(),
    		     local_date_time::NOT_DATE_TIME_ON_ERROR) {
    	checkSerialNumber(serialNumber);
    }

    Weekday Datetime::weekday() const {
    	return mapBoostDateType2QL(localTime_.date().day_of_week());
    }

    Day Datetime::dayOfMonth() const {
    	return localTime_.date().day();
    }

    Day Datetime::dayOfYear() const {
    	return localTime_.date().day_of_year();
    }

    Month Datetime::month() const {
    	return Month(Integer(localTime_.date().month()));
    }

    Year Datetime::year() const {
    	return localTime_.date().year();
    }

    BigInteger Datetime::serialNumber() const {
    	const BigInteger n
    		= (localTime_.date() - serialNumberDateReference()).days();
    	checkSerialNumber(n);

    	return n;
    }

    const local_date_time& Datetime::localTime() const { return localTime_; }

    Datetime& Datetime::operator+=(BigInteger d) {
    	localTime_ += boost::gregorian::days(d);
    	return *this;
    }

    Datetime& Datetime::operator+=(const Period& p) {
    	advance(localTime_, p.length(), p.units());
    	return *this;
    }

    Datetime& Datetime::operator-=(BigInteger d) {
    	localTime_ -= boost::gregorian::days(d);
    	return *this;
    }
    Datetime& Datetime::operator-=(const Period& p) {
    	advance(localTime_, -p.length(), p.units());
    	return *this;
    }

    Datetime& Datetime::operator++() {
    	localTime_+=boost::gregorian::days(1);
    	return *this;
    }

    Datetime Datetime::operator++(int) {
    	Datetime retVal(*this);
    	++*this;
    	return retVal;
    }

    Datetime& Datetime::operator--() {
    	localTime_-=boost::gregorian::days(1);
    	return *this;
    }

    Datetime Datetime::operator--(int) {
    	Datetime retVal(*this);
    	--*this;
    	return retVal;
    }

    Datetime Datetime::operator+(BigInteger days) const {
    	Datetime retVal(*this);
    	retVal+=days;

    	return retVal;
    }

    Datetime Datetime::operator-(BigInteger days) const {
    	Datetime retVal(*this);
    	retVal-=days;

    	return retVal;
    }

    Datetime Datetime::operator+(const Period& p) const {
    	Datetime retVal(*this);
    	retVal+=p;

    	return retVal;
    }

    Datetime Datetime::operator-(const Period& p) const {
    	Datetime retVal(*this);
    	retVal-=p;

    	return retVal;
    }

    Datetime Datetime::todaysDate() {
        std::time_t t;

        if (std::time(&t) == std::time_t(-1)) // -1 means time() didn't work
            return Datetime();
        std::tm *lt = std::localtime(&t);
        return Datetime(Day(lt->tm_mday),
                    	Month(lt->tm_mon+1),
                    	Year(lt->tm_year+1900));
    }

    Datetime Datetime::minDate() {
    	return minSerialNumberDatetime();
//    	return Datetime(local_date_time(boost::gregorian::min_date_time));
    }

    Datetime Datetime::maxDate() {
    	return maxSerialNumberDatetime();
//    	return Datetime(local_date_time(boost::gregorian::max_date_time));
    }

    bool Datetime::isLeap(Year y) {
    	return boost::gregorian::gregorian_calendar::is_leap_year(y);
    }

    Datetime Datetime::endOfMonth(const Datetime& d) {
        const Month m = d.month();
        const Year y = d.year();
        const Day eoM = boost::gregorian::gregorian_calendar::end_of_month_day(
        	d.year(), d.month());

        return Datetime(eoM, m, y);
    }

    bool Datetime::isEndOfMonth(const Datetime& d) {
    	return d.dayOfMonth() ==
    		boost::gregorian::gregorian_calendar::end_of_month_day(
            	d.year(), d.month());
    }


    Datetime Datetime::nextWeekday(const Datetime& d, Weekday dayOfWeek) {
        Weekday wd = d.weekday();
        return d + ((wd>dayOfWeek ? 7 : 0) - wd + dayOfWeek);
    }

    Datetime Datetime::nthWeekday(Size nth, Weekday dayOfWeek,
                          	  	  Month m, Year y) {
        QL_REQUIRE(nth>0,
                   "zeroth day of week in a given (month, year) is undefined");
        QL_REQUIRE(nth<6,
                   "no more than 5 weekday in a given (month, year)");
        Weekday first = Datetime(1, m, y).weekday();
        Size skip = nth - (dayOfWeek>=first ? 1 : 0);
        return Datetime((1 + dayOfWeek + skip*7) - first, m, y);
    }


    BigInteger operator-(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime().date() - d2.localTime().date()).days();
    }

    bool operator==(const Datetime& d1, const Datetime& d2) {
        return (d1.localTime() == d2.localTime());
    }

    bool operator!=(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime() != d2.localTime());
    }

    bool operator<(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime() < d2.localTime());
    }

    bool operator<=(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime() <= d2.localTime());
    }

    bool operator>(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime() > d2.localTime());
    }

    bool operator>=(const Datetime& d1, const Datetime& d2) {
    	return (d1.localTime() >= d2.localTime());
    }

    std::ostream& operator<<(std::ostream& out, Month m) {
        switch (m) {
          case January:
            return out << "January";
          case February:
            return out << "February";
          case March:
            return out << "March";
          case April:
            return out << "April";
          case May:
            return out << "May";
          case June:
            return out << "June";
          case July:
            return out << "July";
          case August:
            return out << "August";
          case September:
            return out << "September";
          case October:
            return out << "October";
          case November:
            return out << "November";
          case December:
            return out << "December";
          default:
            QL_FAIL("unknown month (" << Integer(m) << ")");
        }
    }

    std::ostream& operator<<(std::ostream& out, const Datetime& d) {
        return out << io::long_datetime(d);
    }

    namespace detail {

        std::ostream& operator<<(std::ostream& out,
                                 const short_datetime_holder& holder) {
            const Datetime& d = holder.d;
            if (d == Datetime()) {
                out << "null date";
            } else {
                Integer dd = d.dayOfMonth(), mm = Integer(d.month()),
                        yyyy = d.year();
                char filler = out.fill();
                out << std::setw(2) << std::setfill('0') << mm << "/";
                out << std::setw(2) << std::setfill('0') << dd << "/";
                out << yyyy;
                out.fill(filler);
            }
            return out;
        }

        std::ostream& operator<<(std::ostream& out,
                                 const long_datetime_holder& holder) {
            const Datetime& d = holder.d;
            if (d == Datetime()) {
                out << "null date";
            } else {
                out << d.month() << " ";
                out << io::ordinal(d.dayOfMonth()) << ", ";
                out << d.year();
            }
            return out;
        }

        std::ostream& operator<<(std::ostream& out,
                                 const iso_datetime_holder& holder) {
            const Datetime& d = holder.d;
            if (d == Datetime()) {
                out << "null date";
            } else {
                Integer dd = d.dayOfMonth(), mm = Integer(d.month()),
                        yyyy = d.year();
                char filler = out.fill();
                out << yyyy << "-";
                out << std::setw(2) << std::setfill('0') << mm << "-";
                out << std::setw(2) << std::setfill('0') << dd;
                out.fill(filler);
            }
            return out;
        }

        std::ostream& operator<<(std::ostream& out,
                                 const formatted_datetime_holder& holder) {
            using namespace boost::gregorian;
            const Datetime& d = holder.d;
            if (d == Datetime()) {
                out << "null date";
            } else {
                date boostDate(d.year(), d.month(), d.dayOfMonth());
                out.imbue(std::locale(std::locale(),
                                      new date_facet(holder.f.c_str())));
                out << boostDate;
            }
            return out;
        }
    }

    namespace io {

        detail::short_datetime_holder short_datetime(const Datetime& d) {
            return detail::short_datetime_holder(d);
        }

        detail::long_datetime_holder long_datetime(const Datetime& d) {
            return detail::long_datetime_holder(d);
        }

        detail::iso_datetime_holder iso_datetime(const Datetime& d) {
            return detail::iso_datetime_holder(d);
        }

        detail::formatted_datetime_holder formatted_datetime(
        	const Datetime& d, const std::string& f) {
            return detail::formatted_datetime_holder(d, f);
        }
    }
}
