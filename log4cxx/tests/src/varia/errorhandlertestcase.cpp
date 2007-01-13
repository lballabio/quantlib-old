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

#include "../util/transformer.h"
#include "../util/compare.h"
#include "../util/controlfilter.h"
#include "../util/threadfilter.h"
#include "../util/linenumberfilter.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::xml;

#define TEMP _T("output/temp")
#define FILTERED _T("output/filtered")
#define TEST1_A_PAT _T("FALLBACK - test - Message \\d")
#define TEST1_B_PAT _T("FALLBACK - root - Message \\d")
#define TEST1_2_PAT \
	_T("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2},\\d{3} ") \
	_T("\\[main]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* - Message \\d")

class ErrorHandlerTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(ErrorHandlerTestCase);
		CPPUNIT_TEST(test1);
	CPPUNIT_TEST_SUITE_END();

	LoggerPtr root;
	LoggerPtr logger;

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		logger = Logger::getLogger(_T("test"));
	}

	void tearDown()
	{
		logger->getLoggerRepository()->resetConfiguration();
	}

	void test1()
	{
   		DOMConfigurator::configure(_T("input/xml/fallback1.xml"));
		common();
		
		ControlFilter cf;
		cf << TEST1_A_PAT << TEST1_B_PAT << TEST1_2_PAT;
		
		ThreadFilter threadFilter;
		LineNumberFilter lineNumberFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		filters.push_back(&lineNumberFilter);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/fallback")));
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

CPPUNIT_TEST_SUITE_REGISTRATION(ErrorHandlerTestCase);

#endif //HAVE_XML
