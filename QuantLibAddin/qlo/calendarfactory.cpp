
/*
 Copyright (C) 2006 The QuantLib Group

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/objhandlerdefines.hpp>
#include <boost/regex.hpp>
#include <qlo/typefactory.hpp>
#include <set>

namespace QuantLibAddin {

    /* 
    Calendar factory - accept a string, and return either a
    QuantLib::Calendar or a QuantLib::JointCalendar as appropriate
    */

    QuantLib::Calendar Create<QuantLib::Calendar>::operator()(const std::string &id) {
        idOriginal = id;
        // Is this an ID for a Calendar or a JointCalendar?
        if (testID()) {
            // It's a JointCalendar.  Parse the ID.
            parseID();
            // Does the requested JointCalendar already exist?
            if (checkType(idFull)) {
                // It does - return it.
                return *(static_cast<QuantLib::Calendar*>(this->getType(idFull)));
            } else {
                // It doesn't - create it, add it to the registry, and return it.
                QuantLib::Calendar *jointCalendar = makeJointCalendar(calendarIDs.size());
                registerType(idFull, jointCalendar);
                return *jointCalendar;
            }
        } else {
            // the ID is for a Calendar - return it
            return *(static_cast<QuantLib::Calendar*>(this->getType(id)));
        }
    }

    /*
    Test whether the ID is that of a joint calendar.
    If it is, then later we'll parse it with a boost::regex, but that
    operation is expensive so for this initial test we just do a case
    insensitive test whether the first 4 characters are "JOIN".
    */
    bool Create<QuantLib::Calendar>::testID() {
        idUpper = boost::algorithm::to_upper_copy(idOriginal);
        static std::string join("JOIN");
        return idUpper.compare(0, 4, join) == 0;
    }

    /*
    Parse the joint calendar ID.
    A JointCalendar ID is in a format such as
        JoinHolidays(UnitedStates::Settlement, UnitedKingdom::Exchange)
    - the initial string is either "JoinHolidays" or "JoinBusinessDays"
    - the parentheses contain a comma-delimited list of 2, 3 or 4 calendar IDs
    - a calendar ID can be in format "abcd" or "abcd::efgh"
    */
    void Create<QuantLib::Calendar>::parseID() {
        // strip out whitespace (NB ~50% of elapsed time spent here)
        static boost::regex regex_whitespace("\\s");
        std::string idStrip = boost::regex_replace(idUpper, regex_whitespace, "");

        // parse the ID (the other ~50%).
        static boost::regex jointCalendarID(
            "((?:JOINHOLIDAYS)|(?:JOINBUSINESSDAYS))\\((.+?),(.+?)(?:,(.+?))?(?:,(.+))?\\)");
        boost::smatch m; 
        QL_REQUIRE(boost::regex_match(idStrip, m, jointCalendarID), 
            "the string '" << idOriginal << "' is not a valid joint calendar identifier");

        // Derive the inputs to the JointCalendar constructor.  

        // Add the calendar IDs to a set where they will be uniquely sorted.
        std::set<std::string> calendarIdSet;

        // Given that the regex succeeded, we're guaranteed :-) to have
        // m[1] - "JOINHOLIDAYS"/"JOINBUSINESSDAYS"
        // m[2] - the ID of the first calendar
        // m[3] - the ID of the second calendar
        if (m[1] == "JOINHOLIDAYS")
            jointCalendarRule = QuantLib::JointCalendarRule(QuantLib::JoinHolidays);
        else // "JOINBUSINESSDAYS"
            jointCalendarRule = QuantLib::JointCalendarRule(QuantLib::JoinBusinessDays);
        calendarIdSet.insert(m[2]);
        calendarIdSet.insert(m[3]);

        // we may have a 3rd or a 3rd & 4th calendar
        if (m[5].matched) {
            calendarIdSet.insert(m[4]);
            calendarIdSet.insert(m[5]);
        } else if (m[4].matched) {
            calendarIdSet.insert(m[4]);
        }

        // if the list of calendars contained duplicates, 
        // we may end up with just one value, which is invalid
        QL_REQUIRE(calendarIdSet.size() > 1, "the string '" << idOriginal << 
            "' is not a valid joint calendar identifier");

        /*
        1) transfer the IDs from the set to a vector
        2) in the same loop, format a unique key "idFull" for the object,
           this will be the same as the ID provided by the user e.g.
             JoinHolidays(UnitedStates::Settlement, UnitedKingdom::Exchange)
           -> uppercase, no whitespace, IDs sorted alphabetically e.g.
             JOINHOLIDAYS(UNITEDKINGDOM::EXCHANGE,UNITEDSTATES::SETTLEMENT)
        */
        std::set<std::string>::const_iterator i = calendarIdSet.begin();
        std::ostringstream s;
        s << m[1] << "(" << *i;
        calendarIDs.push_back(*i);
        i++;
        while (i != calendarIdSet.end()) {
            calendarIDs.push_back(*i);
            s << "," << *i;
            i++;
        }
        s << ")";
        idFull = s.str();

    }

    /*
    Wrappers for the various QuantLib::JointCalendar constructors.
    */

    QuantLib::Calendar *Create<QuantLib::Calendar>::makeJointCalendar(
        const unsigned int &numCals) {
        switch (numCals) {
            case 2:
                return makeJointCalendar2();
            case 3:
                return makeJointCalendar3();
            case 4:
                return makeJointCalendar4();
            // if the end user provided the wrong #/calendars, the error
            // would have been detected earlier in parseID(), and the case
            // below is just a sanity check.
            default:
                QL_FAIL("JointCalendar constructor expects 2, 3, or 4 "
                    "calendars - " << numCals << " were provided");
        }
    }

    QuantLib::Calendar *Create<QuantLib::Calendar>::makeJointCalendar2() {
        QuantLib::Calendar *calendar1 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[0]));
        QuantLib::Calendar *calendar2 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[1]));
        return new QuantLib::JointCalendar(
            *calendar1,
            *calendar2,
            jointCalendarRule);
    }

    QuantLib::Calendar *Create<QuantLib::Calendar>::makeJointCalendar3() {
        QuantLib::Calendar *calendar1 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[0]));
        QuantLib::Calendar *calendar2 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[1]));
        QuantLib::Calendar *calendar3 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[2]));
        return new QuantLib::JointCalendar(
            *calendar1,
            *calendar2,
            *calendar3,
            jointCalendarRule);
    }

    QuantLib::Calendar *Create<QuantLib::Calendar>::makeJointCalendar4() {
        QuantLib::Calendar *calendar1 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[0]));
        QuantLib::Calendar *calendar2 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[1]));
        QuantLib::Calendar *calendar3 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[2]));
        QuantLib::Calendar *calendar4 =
            static_cast<QuantLib::Calendar*>(this->getType(calendarIDs[3]));
        return new QuantLib::JointCalendar(
            *calendar1,
            *calendar2,
            *calendar3,
            *calendar4,
            jointCalendarRule);
    }

}

