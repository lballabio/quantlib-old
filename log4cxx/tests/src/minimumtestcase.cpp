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
#include <log4cxx/simplelayout.h>
#include <log4cxx/ttcclayout.h>
#include <log4cxx/fileappender.h>
#include <log4cxx/helpers/absolutetimedateformat.h>

#include "util/compare.h"
#include "util/transformer.h"
#include "util/linenumberfilter.h"
#include "util/controlfilter.h"
#include "util/absolutedateandtimefilter.h"
#include "util/threadfilter.h"

using namespace log4cxx;
using namespace log4cxx::helpers;

#define FILTERED _T("output/filtered")

#define TTCC_PAT  \
	ABSOLUTE_DATE_AND_TIME_PAT \
	_T(" \\[\\d*]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* - Message \\d{1,2}")

#define TTCC2_PAT \
	ABSOLUTE_DATE_AND_TIME_PAT \
	_T(" \\[\\d*]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* - ") \
	_T("Messages should bear numbers 0 through 23\\.")


class MinimumTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(MinimumTestCase);
		CPPUNIT_TEST(simple);
		CPPUNIT_TEST(ttcc);
	CPPUNIT_TEST_SUITE_END();

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		root->removeAllAppenders();
	}

	void tearDown()
	{
		root->getLoggerRepository()->resetConfiguration();
	}

	void simple()
	{
		LayoutPtr layout = new SimpleLayout();
		AppenderPtr appender = new FileAppender(layout, _T("output/simple"), false);
		root->addAppender(appender);
		common();

		CPPUNIT_ASSERT(Compare::compare(_T("output/simple"), _T("witness/simple")));
	}

	void ttcc()
	{
		LayoutPtr layout =
			new TTCCLayout(AbsoluteTimeDateFormat::DATE_AND_TIME_DATE_FORMAT);
		AppenderPtr appender = new FileAppender(layout, _T("output/ttcc"), false);
		root->addAppender(appender);
		common();

		ControlFilter filter1;
		filter1 << TTCC_PAT << TTCC2_PAT;
		AbsoluteDateAndTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(_T("output/ttcc"), FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/ttcc")));
	}

	void common()
	{
		int i = 0;

		// In the lines below, the category names are chosen as an aid in
		// remembering their level values. In general, the category names
		// have no bearing to level values.
		LoggerPtr ERR = Logger::getLogger(_T("ERR"));
		ERR->setLevel(Level::ERROR);

		LoggerPtr INF = Logger::getLogger(_T("INF"));
		INF->setLevel(Level::INFO);

		LoggerPtr INF_ERR = Logger::getLogger(_T("INF.ERR"));
		INF_ERR->setLevel(Level::ERROR);

		LoggerPtr DEB = Logger::getLogger(_T("DEB"));
		DEB->setLevel(Level::DEBUG);

		// Note: categories with undefined level
		LoggerPtr INF_UNDEF = Logger::getLogger(_T("INF.UNDEF"));
		LoggerPtr INF_ERR_UNDEF = Logger::getLogger(_T("INF.ERR.UNDEF"));
		LoggerPtr UNDEF = Logger::getLogger(_T("UNDEF"));

		// These should all log.----------------------------
		LOG4CXX_FATAL(ERR, _T("Message ") << i);
		i++; //0
		LOG4CXX_ERROR(ERR, _T("Message ") << i);
		i++;

		LOG4CXX_FATAL(INF, _T("Message ") << i);
		i++; // 2
		LOG4CXX_ERROR(INF, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(INF, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(INF, _T("Message ") << i);
		i++;

		LOG4CXX_FATAL(INF_UNDEF, _T("Message ") << i);
		i++; //6
		LOG4CXX_ERROR(INF_UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(INF_UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(INF_UNDEF, _T("Message ") << i);
		i++;

		LOG4CXX_FATAL(INF_ERR, _T("Message ") << i);
		i++; // 10
		LOG4CXX_ERROR(INF_ERR, _T("Message ") << i);
		i++;

		LOG4CXX_FATAL(INF_ERR_UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_ERROR(INF_ERR_UNDEF, _T("Message ") << i);
		i++;

		LOG4CXX_FATAL(DEB, _T("Message ") << i);
		i++; //14
		LOG4CXX_ERROR(DEB, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(DEB, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(DEB, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(DEB, _T("Message ") << i);
		i++;

		// defaultLevel=DEBUG
		LOG4CXX_FATAL(UNDEF, _T("Message ") << i);
		i++; // 19
		LOG4CXX_ERROR(UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(UNDEF, _T("Message ") << i);
		i++;

		// -------------------------------------------------
		// The following should not log
		LOG4CXX_WARN(ERR, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(ERR, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(ERR, _T("Message ") << i);
		i++;

		LOG4CXX_DEBUG(INF, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(INF_UNDEF, _T("Message ") << i);
		i++;

		LOG4CXX_WARN(INF_ERR, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(INF_ERR, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(INF_ERR, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(INF_ERR_UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(INF_ERR_UNDEF, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(INF_ERR_UNDEF, _T("Message ") << i);
		i++;

		// -------------------------------------------------
		LOG4CXX_INFO(INF, _T("Messages should bear numbers 0 through 23."));
	}

	LoggerPtr root;
	LoggerPtr logger;
};


CPPUNIT_TEST_SUITE_REGISTRATION(MinimumTestCase);
