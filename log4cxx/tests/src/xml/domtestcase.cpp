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

#include "../util/compare.h"
#include "xlevel.h"
#include "../util/controlfilter.h"
#include "../util/iso8601filter.h"
#include "../util/threadfilter.h"
#include "../util/transformer.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::xml;

#define TEMP_A1 _T("output/temp.A1")
#define TEMP_A2 _T("output/temp.A2")
#define FILTERED_A1 _T("output/filtered.A1")
#define FILTERED_A2 _T("output/filtered.A2")

#define TEST1_1A_PAT \
	_T("(DEBUG|INFO |WARN |ERROR|FATAL) \\w*\\.\\w* - Message \\d")

#define TEST1_1B_PAT _T("(DEBUG|INFO |WARN |ERROR|FATAL) root - Message \\d")

#define TEST1_2_PAT _T("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2},\\d{3} ") \
	_T("\\[\\d*]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* - Message \\d")

class DOMTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(DOMTestCase);
		CPPUNIT_TEST(test1);
	CPPUNIT_TEST_SUITE_END();

	LoggerPtr root;
	LoggerPtr logger;

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		logger = Logger::getLogger(_T("org.apache.log4j.xml.DOMTestCase"));
	}

	void tearDown()
	{
		root->getLoggerRepository()->resetConfiguration();
	}

	void test1()
	{
		DOMConfigurator::configure(_T("input/xml/DOMTestCase1.xml"));
		common();

		ControlFilter cf1;
		cf1 << TEST1_1A_PAT << TEST1_1B_PAT;

		ControlFilter cf2;
		cf2 << TEST1_2_PAT;

		ThreadFilter threadFilter;
		ISO8601Filter iso8601Filter;

		std::vector<Filter *> filters1;
		filters1.push_back(&cf1);

		std::vector<Filter *> filters2;
		filters2.push_back(&cf2);
		filters2.push_back(&threadFilter);
		filters2.push_back(&iso8601Filter);

		try
		{
			Transformer::transform(TEMP_A1, FILTERED_A1, filters1);
			Transformer::transform(TEMP_A2, FILTERED_A2, filters2);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}
 
		CPPUNIT_ASSERT(Compare::compare(FILTERED_A1, _T("witness/dom.A1.1")));
		CPPUNIT_ASSERT(Compare::compare(FILTERED_A2, _T("witness/dom.A2.1")));
	}

	void common()
	{
		int i = -1;

		LOG4CXX_DEBUG(logger, _T("Message ") << ++i);
		LOG4CXX_DEBUG(root, _T("Message ") << i);

		LOG4CXX_INFO(logger, _T("Message ") << ++i);
		LOG4CXX_INFO(root, _T("Message ") << i);

		LOG4CXX_WARN(logger, _T("Message ") << ++i);
		LOG4CXX_WARN(root, _T("Message ") << i);

		LOG4CXX_ERROR(logger, _T("Message ") << ++i);
		LOG4CXX_ERROR(root, _T("Message ") << i);

		LOG4CXX_FATAL(logger, _T("Message ") << ++i);
		LOG4CXX_FATAL(root, _T("Message ") << i);

	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(DOMTestCase);

#endif //HAVE_XML
