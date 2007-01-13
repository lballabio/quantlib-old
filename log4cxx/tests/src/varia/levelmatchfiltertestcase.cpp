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
#include <log4cxx/fileappender.h>
#include <log4cxx/level.h>
#include <log4cxx/varia/levelmatchfilter.h>
#include <log4cxx/varia/denyallfilter.h>

#include "../util/compare.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::varia;

#define ACCEPT_FILE _T("output/LevelMatchFilter_accept")
#define ACCEPT_WITNESS _T("witness/LevelMatchFilter_accept")
#define DENY_FILE _T("output/LevelMatchFilter_deny")
#define DENY_WITNESS _T("witness/LevelMatchFilter_deny")

class LevelMatchFilterTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(LevelMatchFilterTestCase);
		CPPUNIT_TEST(accept);
		CPPUNIT_TEST(deny);
	CPPUNIT_TEST_SUITE_END();

	LoggerPtr root;
	LoggerPtr logger;

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		root->removeAllAppenders();
		logger = Logger::getLogger(_T("test"));
	}

	void tearDown()
	{
		root->getLoggerRepository()->resetConfiguration();
	}

	void accept()
	{
		// set up appender
		LayoutPtr layout = new SimpleLayout();
		AppenderPtr appender = new FileAppender(layout, ACCEPT_FILE, false);

		// create LevelMatchFilter
		LevelMatchFilterPtr matchFilter = new LevelMatchFilter();

		// attach match filter to appender
		appender->addFilter(matchFilter);

		// attach DenyAllFilter to end of filter chain to deny neutral
		// (non matching) messages
		appender->addFilter(new DenyAllFilter());

		// set appender on root and set level to debug
		root->addAppender(appender);
		root->setLevel(Level::DEBUG);

		LevelPtr levelArray[] =
			{ Level::DEBUG, Level::INFO, Level::WARN, Level::ERROR, Level::FATAL };
			
		int length = sizeof(levelArray)/sizeof(levelArray[0]);

		for (int x = 0; x < length; x++)
		{
			// set the level to match
			matchFilter->setLevelToMatch(levelArray[x]->toString());
			StringBuffer sbuf;
			sbuf << _T("pass ") << x << _T("; filter set to accept only ")
				<< levelArray[x]->toString() << _T(" msgs");
			common(sbuf.str());
		}
		
		CPPUNIT_ASSERT(Compare::compare(ACCEPT_FILE, ACCEPT_WITNESS));
	}
	
	void deny()
	{
		// set up appender
		LayoutPtr layout = new SimpleLayout();
		AppenderPtr appender = new FileAppender(layout, DENY_FILE, false);

		// create LevelMatchFilter, set to deny matches
		LevelMatchFilterPtr matchFilter = new LevelMatchFilter();
		matchFilter->setAcceptOnMatch(false);

		// attach match filter to appender
		appender->addFilter(matchFilter);

		// set appender on root and set level to debug
		root->addAppender(appender);
		root->setLevel(Level::DEBUG);

		LevelPtr levelArray[] =
			{ Level::DEBUG, Level::INFO, Level::WARN, Level::ERROR, Level::FATAL };
			
		int length = sizeof(levelArray)/sizeof(levelArray[0]);

		for (int x = 0; x < length; x++)
		{
			// set the level to match
			matchFilter->setLevelToMatch(levelArray[x]->toString());
			StringBuffer sbuf;
			sbuf << _T("pass ") << x << _T("; filter set to deny only ")
				<< levelArray[x]->toString() << _T(" msgs");
			common(sbuf.str());
		}

 		CPPUNIT_ASSERT(Compare::compare(DENY_FILE, DENY_WITNESS));
 	}
	
	void common(const String& msg)
	{
		logger->debug(msg);
		logger->info(msg);
		logger->warn(msg);
		logger->error(msg);
		logger->fatal(msg);
	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(LevelMatchFilterTestCase);
