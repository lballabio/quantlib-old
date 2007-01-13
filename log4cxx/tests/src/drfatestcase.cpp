/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <log4cxx/dailyrollingfileappender.h>
#include <time.h>
#include <log4cxx/helpers/timezone.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

/**
Test the configuration of the hierarchy-wide threshold.
*/
class DRFATestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(DRFATestCase);
		CPPUNIT_TEST(testComputeCheckPeriod);
		CPPUNIT_TEST(testRC1);
		CPPUNIT_TEST(testRC2);
	CPPUNIT_TEST_SUITE_END();


public:
	void setUp()
	{
	}

	void tearDown()
	{
	}

	void testComputeCheckPeriod()
	{
		RollingCalendar rc;
	   
		CPPUNIT_ASSERT_EQUAL(RollingCalendar::TOP_OF_DAY,
			rc.computeTriggeringPeriod(_T("%Y-%m-%d.log")));
	
		CPPUNIT_ASSERT_EQUAL(RollingCalendar::TOP_OF_MINUTE,
			rc.computeTriggeringPeriod(_T("%Y-%m-%d %M.log")));

		CPPUNIT_ASSERT_EQUAL(RollingCalendar::TOP_OF_HOUR,
			rc.computeTriggeringPeriod(_T("%Y-%m-%d %H.log")));
    
		CPPUNIT_ASSERT_EQUAL(RollingCalendar::TOP_OF_MONTH,
			rc.computeTriggeringPeriod(_T("%Y-%m.log")));
	
		CPPUNIT_ASSERT_EQUAL(RollingCalendar::TOP_OF_HOUR,
			rc.computeTriggeringPeriod(_T("log%Hlog")));
	}
	
	void testRC1()
	{
		RollingCalendar rc;
		rc.setType(RollingCalendar::TOP_OF_DAY);

		putenv("TZ=");
		tzset();
		
		// jan, mar, may, july, aug, oct, dec have 31 days
		int M31[] = { 0, 2, 4, 6, 7, 9, 11 };
		
		for (int i = 0; i < sizeof(M31)/sizeof(M31[0]); i++)
		{
			for (int d = 1; d <= 31; d++)
			{
				for (int h = 0; h < 23; h++)
				{
					struct tm tm;
					memset(&tm, 0, sizeof(tm));
					tm.tm_year = 80;
					tm.tm_mon = M31[i];
					tm.tm_mday = d;
					tm.tm_hour = h;
					tm.tm_min = 10;
					tm.tm_sec = 10;
					
					time_t buff = mktime(&tm);
					int64_t t = int64_t(buff) * 1000;
					t += 88;
					int64_t t1 = rc.getNextCheckMillis(t);
					
					time_t n = (time_t)(t1 / 1000);
					struct tm * nextTime = localtime(&n);
					if (d == 31)
					{
						CPPUNIT_ASSERT_EQUAL((M31[i] + 1) % 12, nextTime->tm_mon);
						CPPUNIT_ASSERT_EQUAL(1, nextTime->tm_mday);
					} 
					else
					{
						CPPUNIT_ASSERT_EQUAL(M31[i], nextTime->tm_mon);
						CPPUNIT_ASSERT_EQUAL(d + 1, nextTime->tm_mday);
					}
								
					CPPUNIT_ASSERT_EQUAL(0, nextTime->tm_hour);
					CPPUNIT_ASSERT_EQUAL(0, nextTime->tm_min);
					CPPUNIT_ASSERT_EQUAL(0, nextTime->tm_sec);
					CPPUNIT_ASSERT(0 == (t1 % 1000));
				}
			}
		}
	}
	
	void testRC2()
	{
		RollingCalendar rc;
		rc.setType(RollingCalendar::TOP_OF_HOUR);
		TimeZonePtr timeZone = rc.getTimeZone();
		
		putenv("TZ=");
		tzset();
		
		// jan, mar, may, july, aug, oct, dec have 31 days
		int M31[] = { 0, 2, 4, 6, 7, 9, 11 };
		
		for (int i = 0; i < sizeof(M31)/sizeof(M31[0]); i++)
		{
			for (int d = 1; d <= 31; d++)
			{
				for (int h = 0; h < 23; h++)
				{
					for (int m = 0; m <= 59; m++)
					{
						struct tm tm;
						memset(&tm, 0, sizeof(tm));
						tm.tm_year = 80;
						tm.tm_mon = M31[i];
						tm.tm_mday = d;
						tm.tm_hour = h;
						tm.tm_min = m;
						tm.tm_sec = 12;
						
						time_t buff = mktime(&tm);
						int64_t t = int64_t(buff) * 1000;
						t += 88;
						
						bool dltState0 = timeZone->inDaylightTime(t);
						
						t = rc.getNextCheckMillis(t);
            			bool dltState1 = timeZone->inDaylightTime(t);

						time_t n = (time_t)(t / 1000);
						struct tm * nextTime = localtime(&n);

						CPPUNIT_ASSERT(0 == (t % 1000));
						CPPUNIT_ASSERT_EQUAL(0, nextTime->tm_sec);
						CPPUNIT_ASSERT_EQUAL(0, nextTime->tm_min);
						
						if (dltState0 == dltState1)
						{
							CPPUNIT_ASSERT_EQUAL((tm.tm_hour + 1) % 24, nextTime->tm_hour);
						} 
						else 
						{
							// returning to standard time
							if (dltState0) 
							{
								CPPUNIT_ASSERT_EQUAL(tm.tm_hour, nextTime->tm_hour);
							} 
							else 
							{
								// switching to day light saving time
							}
						}

						if (tm.tm_hour == 23)
						{
							CPPUNIT_ASSERT_EQUAL(d % 31 + 1, nextTime->tm_mday);
							if (d == 31)
							{
								CPPUNIT_ASSERT_EQUAL((M31[i] + 1) % 12, nextTime->tm_mon);
							}
							else
							{
								CPPUNIT_ASSERT_EQUAL(M31[i], nextTime->tm_mon);
							}
						} 
						else
						{
							CPPUNIT_ASSERT_EQUAL(d, nextTime->tm_mday);
							CPPUNIT_ASSERT_EQUAL(M31[i], nextTime->tm_mon);
						}
					}
				}
			}
		}
	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(DRFATestCase);
