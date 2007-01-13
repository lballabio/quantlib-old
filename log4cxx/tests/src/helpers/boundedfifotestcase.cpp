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

#include <log4cxx/logmanager.h>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/boundedfifo.h>
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/strictmath.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

#define MAX 1000

class BoundedFIFOTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(BoundedFIFOTestCase);
		CPPUNIT_TEST(test1);
		CPPUNIT_TEST(test2);
		CPPUNIT_TEST(testResize1);
		CPPUNIT_TEST(testResize2);
		CPPUNIT_TEST(testResize3);
	CPPUNIT_TEST_SUITE_END();
	
	LoggerPtr logger;
	std::vector<LoggingEventPtr> e;
	
public:
	void setUp()
	{
  		logger = Logger::getLogger(_T("x"));
		e.reserve(1000);
		for (int i = 0; i < MAX; i++)
		{
			e.push_back(
			new LoggingEvent(_T(""), logger, Level::DEBUG, _T("e")));
		}
	}

	void tearDown()
	{
		LogManager::shutdown();
	}
	
	void test1()
	{
		for (int size = 1; size <= 128; size *= 2)
		{
			BoundedFIFO bf(size);

			CPPUNIT_ASSERT_EQUAL(size, bf.getMaxSize());
			CPPUNIT_ASSERT(bf.get() == 0);

			int i;
			int j;
			int k;

			for (i = 1; i < (2 * size); i++)
			{
				for (j = 0; j < i; j++)
				{
					//tcout << _T("Putting ") << e[j] << std::endl;
					bf.put(e[j]);
					CPPUNIT_ASSERT_EQUAL((j < size) ? (j + 1) : size, bf.length());
				}

				int max = (size < j) ? size : j;
				j--;

				for (k = 0; k <= j; k++)
				{
					//tcout << _T("max=") << max << _T(", j=") << j
					//	<< _T(", k="= << k << std::endl;
					CPPUNIT_ASSERT_EQUAL(((max - k) > 0) ? (max - k) : 0, bf.length());

					LoggingEventPtr r = bf.get();

					//tcout << _t("Got ") << r << std::endl;
					if (k >= size)
					{
						CPPUNIT_ASSERT(r == 0);
					} 
					else
					{
						CPPUNIT_ASSERT(r == e[k]);
					}
				}
			}

		//tcout << _T("Passed size=") << size << std::endl;
		}
	}
	
	void test2()
	{
		int size = 3;
		BoundedFIFO bf(size);

		bf.put(e[0]);
		CPPUNIT_ASSERT_EQUAL(e[0], bf.get());
		CPPUNIT_ASSERT(bf.get() == 0);

		bf.put(e[1]);
		CPPUNIT_ASSERT_EQUAL(1, bf.length());
		bf.put(e[2]);
		CPPUNIT_ASSERT_EQUAL(2, bf.length());
		bf.put(e[3]);
		CPPUNIT_ASSERT_EQUAL(3, bf.length());
		CPPUNIT_ASSERT_EQUAL(e[1], bf.get());
		CPPUNIT_ASSERT_EQUAL(2, bf.length());
		CPPUNIT_ASSERT_EQUAL(e[2], bf.get());
		CPPUNIT_ASSERT_EQUAL(1, bf.length());
		CPPUNIT_ASSERT_EQUAL(e[3], bf.get());
		CPPUNIT_ASSERT_EQUAL(0, bf.length());
		CPPUNIT_ASSERT(bf.get() == 0);
		CPPUNIT_ASSERT_EQUAL(0, bf.length());
	}

	void testResize1()
	{
		int size = 10;

		for (int n = 1; n < (size * 2); n++)
		{
			for (int i = 0; i < (size * 2); i++)
			{
				BoundedFIFO bf(size);

				for (int f = 0; f < i; f++)
				{
					bf.put(e[f]);
				}

				bf.resize(n);

				int expectedSize = StrictMath::minimum(n, StrictMath::minimum(i, size));
				CPPUNIT_ASSERT_EQUAL(expectedSize, bf.length());

				for (int c = 0; c < expectedSize; c++)
				{
					CPPUNIT_ASSERT_EQUAL(e[c], bf.get());
				}
			}
		}
	}

	void testResize2()
	{
		int size = 10;

		for (int n = 1; n < (size * 2); n++)
		{
			for (int i = 0; i < (size * 2); i++)
			{
				for (int d = 0; d < StrictMath::minimum(i, size); d++)
				{
					BoundedFIFO bf(size);

					for (int p = 0; p < i; p++)
					{
						bf.put(e[p]);
					}

					for (int g = 0; g < d; g++)
					{
						bf.get();
					}

					// x = the number of elems in 
					int x = bf.length();

					bf.resize(n);

					int expectedSize = StrictMath::minimum(n, x);
					CPPUNIT_ASSERT_EQUAL(expectedSize, bf.length());

					for (int c = 0; c < expectedSize; c++)
					{
						CPPUNIT_ASSERT_EQUAL(e[c + d], bf.get());
					}

					CPPUNIT_ASSERT(bf.get() == 0);
				}
			}
		}
	}

 	void testResize3()
	{
		int size = 10;

		for (int n = 1; n < (size * 2); n++)
		{
			for (int i = 0; i < size; i++)
			{
				for (int d = 0; d < i; d++)
				{
					for (int r = 0; r < d; r++)
					{
						BoundedFIFO bf(size);

						for (int p0 = 0; p0 < i; p0++)
							bf.put(e[p0]);

						for (int g = 0; g < d; g++)
							bf.get();

						for (int p1 = 0; p1 < r; p1++)
							bf.put(e[i + p1]);

						int x = bf.length();

						bf.resize(n);

						int expectedSize = StrictMath::minimum(n, x);
						CPPUNIT_ASSERT_EQUAL(expectedSize, bf.length());

						for (int c = 0; c < expectedSize; c++)
						{
						CPPUNIT_ASSERT_EQUAL(e[c + d], bf.get());
						}

						//CPPUNIT_ASSERT(bf.get() == 0);
					}
				}
			}
		}
	}

};

CPPUNIT_TEST_SUITE_REGISTRATION(BoundedFIFOTestCase);
