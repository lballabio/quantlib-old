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

#include <log4cxx/logger.h>
#include <log4cxx/propertyconfigurator.h>
#include "util/compare.h"
#include "xml/xlevel.h"

using namespace log4cxx;

/**
Test the configuration of the hierarchy-wide threshold.
*/
class HierarchyThresholdTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(HierarchyThresholdTestCase);
		CPPUNIT_TEST(test1);
		CPPUNIT_TEST(test2);
		CPPUNIT_TEST(test3);
		CPPUNIT_TEST(test4);
		CPPUNIT_TEST(test5);
		CPPUNIT_TEST(test6);
		CPPUNIT_TEST(test7);
		CPPUNIT_TEST(test8);
	CPPUNIT_TEST_SUITE_END();


public:
	void setUp()
	{
	}

	void tearDown()
	{
		logger->getLoggerRepository()->resetConfiguration();
	}

	void test1()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold1.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.1")));
	}

	void test2()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold2.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.2")));
	}

	void test3()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold3.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.3")));
	}

	void test4()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold4.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.4")));
	}

	void test5()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold5.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.5")));
	}

	void test6()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold6.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.6")));
	}

	void test7()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold7.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.7")));
	}

	void test8()
	{
		PropertyConfigurator::configure(_T("input/hierarchyThreshold8.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/hierarchyThreshold.8")));
	}

	static void common()
	{
		logger->log(XLevel::TRACE, _T("m0"));
		logger->debug(_T("m1"));
		logger->info(_T("m2"));
		logger->warn(_T("m3"));
		logger->error(_T("m4"));
		logger->fatal(_T("m5"));
	}

private:
	static String TEMP;
	static LoggerPtr logger;
};

String HierarchyThresholdTestCase::TEMP =
	 _T("output/temp");

LoggerPtr HierarchyThresholdTestCase::logger =
	Logger::getLogger(_T("HierarchyThresholdTestCase"));

CPPUNIT_TEST_SUITE_REGISTRATION(HierarchyThresholdTestCase);
