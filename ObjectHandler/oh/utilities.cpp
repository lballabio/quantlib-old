/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov
 Copyright (C) 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2008 Nazcatech sprl Belgium

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
#include <oh/config.hpp>
#endif
#include <oh/utilities.hpp>
#include <oh/logger.hpp>
#include <oh/repository.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <sstream>
#include <ctime>
#include <sys/timeb.h>

#define            SECS_PER_DAY    (60 * 60 * 24)
#define            MILLISECS_PER_DAY    (1000 * SECS_PER_DAY)

using boost::algorithm::token_compress_off;
using boost::algorithm::token_compress_on;
using boost::algorithm::is_any_of;

namespace ObjectHandler {

    std::string boostVersion() {
        return BOOST_LIB_VERSION;
    }

    std::string version() {
        return OBJHANDLER_VERSION;
    }

    std::string logSetFile(const std::string &logFileName,
                           const int &logLevel) {
            Logger::instance().setFile(logFileName, logLevel);
            return logFileName;
    }

    DLL_API void logWriteMessage(const std::string &message,
                                 const int &level) {
            Logger::instance().writeMessage(message, level);
    }

    void logSetLevel(const int &logLevel) {
        Logger::instance().setLevel(logLevel);
    }

    const std::string logFile(){
        return Logger::instance().file();
    }

    const int logLevel(){
        return Logger::instance().level();
    }

    void logSetConsole(const int &console,
                      const int &logLevel) {
            Logger::instance().setConsole(console, logLevel);
    }

    void logObject(const std::string &objectID) {
        std::ostringstream msg;
        Repository::instance().dumpObject(objectID, msg);
        Logger::instance().writeMessage(msg.str());
    }

    void logAllObjects() {
        std::ostringstream msg;
        Repository::instance().dump(msg);
        Logger::instance().writeMessage(msg.str());
    }

    std::vector<std::string> split(const std::string& line,
        const std::string& delim,
        bool token_compress) {
            std::vector<std::string> ret;
            return boost::algorithm::split(ret, line, is_any_of(delim),
                token_compress ? token_compress_on : token_compress_off);
    }

    std::string concatenate(const std::vector<std::string>& symbols,
        const std::string& delim) {
            if (symbols.empty())
                return std::string();

            std::ostringstream ret;
            for (size_t i=0; i<symbols.size()-1; ++i)
                ret << symbols[i] << delim;
            ret << symbols[symbols.size()-1];
            return ret.str();
    }

    // the list of all December 31st in the preceding year
    // e.g. for 1901 yearOffset[1] is 366, that is, December 31 1900
    static unsigned long YearOffset[] = {
        // 1900-1909
        0,  366,  731, 1096, 1461, 1827, 2192, 2557, 2922, 3288,
        // 1910-1919
        3653, 4018, 4383, 4749, 5114, 5479, 5844, 6210, 6575, 6940,
        // 1920-1929
        7305, 7671, 8036, 8401, 8766, 9132, 9497, 9862,10227,10593,
        // 1930-1939
        10958,11323,11688,12054,12419,12784,13149,13515,13880,14245,
        // 1940-1949
        14610,14976,15341,15706,16071,16437,16802,17167,17532,17898,
        // 1950-1959
        18263,18628,18993,19359,19724,20089,20454,20820,21185,21550,
        // 1960-1969
        21915,22281,22646,23011,23376,23742,24107,24472,24837,25203,
        // 1970-1979
        25568,25933,26298,26664,27029,27394,27759,28125,28490,28855,
        // 1980-1989
        29220,29586,29951,30316,30681,31047,31412,31777,32142,32508,
        // 1990-1999
        32873,33238,33603,33969,34334,34699,35064,35430,35795,36160,
        // 2000-2009
        36525,36891,37256,37621,37986,38352,38717,39082,39447,39813,
        // 2010-2019
        40178,40543,40908,41274,41639,42004,42369,42735,43100,43465,
        // 2020-2029
        43830,44196,44561,44926,45291,45657,46022,46387,46752,47118,
        // 2030-2039
        47483,47848,48213,48579,48944,49309,49674,50040,50405,50770,
        // 2040-2049
        51135,51501,51866,52231,52596,52962,53327,53692,54057,54423,
        // 2050-2059
        54788,55153,55518,55884,56249,56614,56979,57345,57710,58075,
        // 2060-2069
        58440,58806,59171,59536,59901,60267,60632,60997,61362,61728,
        // 2070-2079
        62093,62458,62823,63189,63554,63919,64284,64650,65015,65380,
        // 2080-2089
        65745,66111,66476,66841,67206,67572,67937,68302,68667,69033,
        // 2090-2099
        69398,69763,70128,70494,70859,71224,71589,71955,72320,72685,
        // 2100-2109
        73050,73415,73780,74145,74510,74876,75241,75606,75971,76337,
        // 2110-2119
        76702,77067,77432,77798,78163,78528,78893,79259,79624,79989,
        // 2120-2129
        80354,80720,81085,81450,81815,82181,82546,82911,83276,83642,
        // 2130-2139
        84007,84372,84737,85103,85468,85833,86198,86564,86929,87294,
        // 2140-2149
        87659,88025,88390,88755,89120,89486,89851,90216,90581,90947,
        // 2150-2159
        91312,91677,92042,92408,92773,93138,93503,93869,94234,94599,
        // 2160-2169
        94964,95330,95695,96060,96425,96791,97156,97521,97886,98252,
        // 2170-2179
        98617,98982,99347,99713,100078,100443,100808,101174,101539,101904,
        // 2180-2189
        102269,102635,103000,103365,103730,104096,104461,104826,105191,105557,
        // 2190-2199
        105922,106287,106652,107018,107383,107748,108113,108479,108844,109209,
        // 2200
        109574
    };

    static const bool YearIsLeap[] = {
        // 1900 is leap in agreement with Excel's bug
        // 1900 is out of valid date range anyway
        // 1900-1909
        true,false,false,false, true,false,false,false, true,false,
        // 1910-1919
        false,false, true,false,false,false, true,false,false,false,
        // 1920-1929
        true,false,false,false, true,false,false,false, true,false,
        // 1930-1939
        false,false, true,false,false,false, true,false,false,false,
        // 1940-1949
        true,false,false,false, true,false,false,false, true,false,
        // 1950-1959
        false,false, true,false,false,false, true,false,false,false,
        // 1960-1969
        true,false,false,false, true,false,false,false, true,false,
        // 1970-1979
        false,false, true,false,false,false, true,false,false,false,
        // 1980-1989
        true,false,false,false, true,false,false,false, true,false,
        // 1990-1999
        false,false, true,false,false,false, true,false,false,false,
        // 2000-2009
        true,false,false,false, true,false,false,false, true,false,
        // 2010-2019
        false,false, true,false,false,false, true,false,false,false,
        // 2020-2029
        true,false,false,false, true,false,false,false, true,false,
        // 2030-2039
        false,false, true,false,false,false, true,false,false,false,
        // 2040-2049
        true,false,false,false, true,false,false,false, true,false,
        // 2050-2059
        false,false, true,false,false,false, true,false,false,false,
        // 2060-2069
        true,false,false,false, true,false,false,false, true,false,
        // 2070-2079
        false,false, true,false,false,false, true,false,false,false,
        // 2080-2089
        true,false,false,false, true,false,false,false, true,false,
        // 2090-2099
        false,false, true,false,false,false, true,false,false,false,
        // 2100-2109
        false,false,false,false, true,false,false,false, true,false,
        // 2110-2119
        false,false, true,false,false,false, true,false,false,false,
        // 2120-2129
        true,false,false,false, true,false,false,false, true,false,
        // 2130-2139
        false,false, true,false,false,false, true,false,false,false,
        // 2140-2149
        true,false,false,false, true,false,false,false, true,false,
        // 2150-2159
        false,false, true,false,false,false, true,false,false,false,
        // 2160-2169
        true,false,false,false, true,false,false,false, true,false,
        // 2170-2179
        false,false, true,false,false,false, true,false,false,false,
        // 2180-2189
        true,false,false,false, true,false,false,false, true,false,
        // 2190-2199
        false,false, true,false,false,false, true,false,false,false,
        // 2200
        false
    };

    double getTime(){

        struct timeb tp;
        struct tm    tm;

        ftime(&tp);
        tm = *localtime(&(tp.time ));

        long years = tm.tm_year + 1900; 
        OH_REQUIRE((years>= 1900 && years <= 2200), "year outside valid range");

        /*
        unsigned long long l = tp.millitm 
            + 1000 * (tm.tm_sec
            + 60 * ( tm.tm_min 
            + 60 * ( tm.tm_hour
            + 24 * ( (tm.tm_yday + 1)
            + days))));
            */
        unsigned long totalDays = 24 * ( (tm.tm_yday + 1) + YearOffset[tm.tm_year]);
        unsigned long totalHours = (60 * (tm.tm_hour + totalDays));
        unsigned long long totalMinutes = tm.tm_min + totalHours;
        totalMinutes *= 60;
        unsigned long long totalSeconds = tm.tm_sec;
        totalSeconds += totalMinutes;
        totalSeconds *= 1000;

        unsigned long long totalMSecond = tp.millitm;
        totalMSecond += totalSeconds;

        double d = totalMSecond / (double)(MILLISECS_PER_DAY);

        return d;

    }

    std::string formatTime(double tm){

        unsigned long long years, monthes, days, hours, minutes, seconds, milliseconds;
        unsigned long long totalMSecond = (unsigned long long)(tm *  MILLISECS_PER_DAY);

        unsigned long long tmp = totalMSecond / MILLISECS_PER_DAY;
        unsigned int yearOffset = 0;
        bool b = false;

        unsigned long size = (2200 - 1900);
        for(; yearOffset < size; ++yearOffset){

            if(tmp < YearOffset[yearOffset]){
                years = yearOffset - 1 + 1900;
                b = true;
                break;
            }
        }
        OH_REQUIRE(b, "year outside valid range");

        days = YearOffset[yearOffset - 1];
        days *= MILLISECS_PER_DAY;
        totalMSecond -= days;
        days = totalMSecond / MILLISECS_PER_DAY;

        static unsigned long MonthOffset[] = {
            0,  31,  59,  90, 120, 151,   // Jan - Jun
            181, 212, 243, 273, 304, 334,   // Jun - Dec
            365     // used in dayOfMonth to bracket day
        };
        static unsigned long MonthLeapOffset[] = {
            0,  31,  60,  91, 121, 152,   // Jan - Jun
            182, 213, 244, 274, 305, 335,   // Jun - Dec
            366     // used in dayOfMonth to bracket day
        };

        unsigned long* pMonth = YearIsLeap[yearOffset - 1] ? MonthLeapOffset : MonthOffset;
        if(YearIsLeap[yearOffset - 1])
            pMonth = MonthLeapOffset;
        else
            pMonth = MonthOffset;
        unsigned long long monthoffset = days/30;
        size = 13;
        b = false;
        OH_REQUIRE(days != 0, "day outside valid range");
        for(; monthoffset < size; ++monthoffset){

            if(days <= pMonth[monthoffset]){
                monthes = monthoffset;
                b = true;
                break;
            }
        }
        OH_REQUIRE(b, "month outside valid range");
        totalMSecond -= days * MILLISECS_PER_DAY;

        days -=  pMonth[monthoffset - 1];

        hours = totalMSecond / (3600 * 1000);
        totalMSecond -= (hours * 3600 * 1000) ;
        minutes =  totalMSecond / (60 * 1000);
        totalMSecond -= minutes * 60 * 1000;
        seconds = totalMSecond / 1000;
        milliseconds = totalMSecond - seconds * 1000;


        char buffer[80];
        sprintf(buffer, "%02llu/%02llu/%04llu %02llu:%02llu:%02llu:%03llu",  monthes, days, years,hours, minutes, seconds, milliseconds);

        return buffer;
    }

}

