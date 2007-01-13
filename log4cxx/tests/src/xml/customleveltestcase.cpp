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

#include <log4cxx/config.h>

#ifdef HAVE_XML

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <log4cxx/logger.h>
#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/patternlayout.h>

#include "../util/compare.h"
#include "xlevel.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::xml;

#define TEMP _T("output/temp")

class CustomLevelTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(CustomLevelTestCase);
		CPPUNIT_TEST(test1);
		CPPUNIT_TEST(test2);
		CPPUNIT_TEST(test3);
		CPPUNIT_TEST(test4);
	CPPUNIT_TEST_SUITE_END();

	LoggerPtr root;
	LoggerPtr logger;

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		logger = Logger::getLogger(_T("xml.CustomLevelTestCase"));
	}

	void tearDown()
	{
		root->getLoggerRepository()->resetConfiguration();

		LoggerPtr logger = Logger::getLogger(_T("LOG4J"));
		logger->setAdditivity(false);
		logger->addAppender(
			new ConsoleAppender(new PatternLayout(_T("log4j: %-22c{2} - %m%n"))));
	}

	void test1()
	{
		DOMConfigurator::configure(_T("input/xml/customLevel1.xml"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/customLevel.1")));
	}

	void test2()
	{
		DOMConfigurator::configure(_T("input/xml/customLevel2.xml"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/customLevel.2")));
	}

	void test3()
	{
		DOMConfigurator::configure(_T("input/xml/customLevel3.xml"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/customLevel.3")));
	}

	void test4()
	{
		DOMConfigurator::configure(_T("input/xml/customLevel4.xml"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/customLevel.4")));
	}

	void common()
	{
		int i = 0;
		LOG4CXX_DEBUG(logger, _T("Message ") << ++i);
		LOG4CXX_INFO(logger, _T("Message ") << ++i);
		LOG4CXX_WARN(logger, _T("Message ") << ++i);
		LOG4CXX_ERROR(logger, _T("Message ") << ++i);
		LOG4CXX_LOG(logger, XLevel::TRACE, _T("Message ") << ++i);
	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(CustomLevelTestCase);

#endif //HAVE_XML
