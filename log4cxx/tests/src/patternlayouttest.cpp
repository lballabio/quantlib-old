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
#include <log4cxx/mdc.h>
#include <log4cxx/patternlayout.h>
#include <log4cxx/fileappender.h>

#include "util/compare.h"
#include "util/transformer.h"
#include "util/absolutedateandtimefilter.h"
#include "util/iso8601filter.h"
#include "util/absolutetimefilter.h"
#include "util/relativetimefilter.h"
#include "util/controlfilter.h"
#include "util/threadfilter.h"
#include "util/linenumberfilter.h"

#define FILTERED _T("output/filtered")
#define TEMP _T("output/temp")
#define PAT0 _T("\\[\\d*]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* - Message \\d{1,2}")
#define PAT1 ISO8601_PAT _T(" ") PAT0
#define PAT2 ABSOLUTE_DATE_AND_TIME_PAT _T(" ") PAT0
#define PAT3 ABSOLUTE_TIME_PAT _T(" ") PAT0
#define PAT4 RELATIVE_TIME_PAT _T(" ") PAT0
#define PAT5 _T("\\[\\d*]\\ (DEBUG|INFO|WARN|ERROR|FATAL) .* : Message \\d{1,2}")
#define PAT6 _T("\\[\\d*]\\ (DEBUG|INFO |WARN |ERROR|FATAL) .*patternlayouttest.cpp\\(\\d{1,4}\\): Message \\d{1,3}")
#define PAT11a _T("^(DEBUG|INFO |WARN |ERROR|FATAL) \\[\\d*]\\ log4j.PatternLayoutTest: Message \\d{1,2}")
#define PAT11b _T("^(DEBUG|INFO |WARN |ERROR|FATAL) \\[\\d*]\\ root: Message \\d{1,2}")
#define PAT12 _T("^\\[\\d*]\\ (DEBUG|INFO |WARN |ERROR|FATAL) ")\
    _T(".*patternlayouttest.cpp\\(\\d{1,4}\\): ")\
    _T("Message \\d{1,2}")
#define PAT_MDC_1 _T("")

using namespace log4cxx;

class PatternLayoutTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(PatternLayoutTest);
		CPPUNIT_TEST(test1);
		CPPUNIT_TEST(test2);
		CPPUNIT_TEST(test3);
		CPPUNIT_TEST(test4);
		CPPUNIT_TEST(test5);
		CPPUNIT_TEST(test6);
		CPPUNIT_TEST(test7);
		CPPUNIT_TEST(test8);
		CPPUNIT_TEST(test9);
		CPPUNIT_TEST(test10);
		CPPUNIT_TEST(test11);
		CPPUNIT_TEST(test12);
		CPPUNIT_TEST(testMDC1);
		CPPUNIT_TEST(testMDC2);
	CPPUNIT_TEST_SUITE_END();
	
	LoggerPtr root;
	LoggerPtr logger;
	
public:
	void setUp()
	{
		root = Logger::getRootLogger();
		logger = Logger::getLogger(_T("java.org.apache.log4j.PatternLayoutTest"));
	}

	void tearDown()
	{
		root->getLoggerRepository()->resetConfiguration();
	}
	
	void test1()
	{
		PropertyConfigurator::configure(_T("input/patternLayout1.properties"));
		common();
		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/patternLayout.1")));
	}
	
	void test2()
	{
		PropertyConfigurator::configure(_T("input/patternLayout2.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT1;
		ISO8601Filter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.2")));
	}
	
	void test3()
	{
		PropertyConfigurator::configure(_T("input/patternLayout3.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT1;
		ISO8601Filter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.3")));
	}
	
	// Output format:
	// 06 avr. 2002 18:30:58,937 [12345] DEBUG atternLayoutTest - Message 0  
	void test4()
	{
		PropertyConfigurator::configure(_T("input/patternLayout4.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT2;
		AbsoluteDateAndTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.4")));
	}

	void test5()
	{
		PropertyConfigurator::configure(_T("input/patternLayout5.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT2;
		AbsoluteDateAndTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.5")));
	}
	
	void test6()
	{
		PropertyConfigurator::configure(_T("input/patternLayout6.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT3;
		AbsoluteTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.6")));
	}
	
	void test7()
	{
		PropertyConfigurator::configure(_T("input/patternLayout7.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT3;
		AbsoluteTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.7")));
	}
	
	void test8()
	{
		PropertyConfigurator::configure(_T("input/patternLayout8.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT4;
		RelativeTimeFilter filter2;
		ThreadFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.8")));
	}
	
	void test9()
	{
		PropertyConfigurator::configure(_T("input/patternLayout9.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT5;
		ThreadFilter filter2;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.9")));
	}
	
	void test10()
	{
		PropertyConfigurator::configure(_T("input/patternLayout10.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT6;
		ThreadFilter filter2;
		LineNumberFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.10")));
	}
	
	void test11()
	{
		PropertyConfigurator::configure(_T("input/patternLayout11.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT11a << PAT11b;
		ThreadFilter filter2;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.11")));
	}
	
	void test12()
	{
		PropertyConfigurator::configure(_T("input/patternLayout12.properties"));
		common();

		ControlFilter filter1;
		filter1 << PAT12;
		ThreadFilter filter2;
		LineNumberFilter filter3;

		std::vector<Filter *> filters;
		filters.push_back(&filter1);
		filters.push_back(&filter2);
		filters.push_back(&filter3);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/patternLayout.12")));
	}
	
	void testMDC1()
	{
		PropertyConfigurator::configure(_T("input/patternLayout.mdc.1.properties"));
		MDC::put(_T("key1"), _T("va11"));
		MDC::put(_T("key2"), _T("va12"));
		logger->debug(_T("Hello World"));
		MDC::clear();

		CPPUNIT_ASSERT(Compare::compare(TEMP, _T("witness/patternLayout.mdc.1")));
	}
	
	void testMDC2()
	{
		String OUTPUT_FILE   = _T("output/patternLayout.mdc.2");
		String WITNESS_FILE  = _T("witness/patternLayout.mdc.2");

		String mdcMsgPattern1 = _T("%m : %X%n");
		String mdcMsgPattern2 = _T("%m : %X{key1}%n");
		String mdcMsgPattern3 = _T("%m : %X{key2}%n");
		String mdcMsgPattern4 = _T("%m : %X{key3}%n");
		String mdcMsgPattern5 = _T("%m : %X{key1},%X{key2},%X{key3}%n");

		// set up appender
		PatternLayoutPtr layout = new PatternLayout(_T("%m%n"));
		AppenderPtr appender = new FileAppender(layout, OUTPUT_FILE, false);

		// set appender on root and set level to debug
		root->addAppender(appender);
		root->setLevel(Level::DEBUG);

		// output starting message
		root->debug(_T("starting mdc pattern test"));

		layout->setConversionPattern(mdcMsgPattern1);
		layout->activateOptions();
		root->debug(_T("empty mdc, no key specified in pattern"));

		layout->setConversionPattern(mdcMsgPattern2);
		layout->activateOptions();
		root->debug(_T("empty mdc, key1 in pattern"));

		layout->setConversionPattern(mdcMsgPattern3);
		layout->activateOptions();
		root->debug(_T("empty mdc, key2 in pattern"));

		layout->setConversionPattern(mdcMsgPattern4);
		layout->activateOptions();
		root->debug(_T("empty mdc, key3 in pattern"));

		layout->setConversionPattern(mdcMsgPattern5);
		layout->activateOptions();
		root->debug(_T("empty mdc, key1, key2, and key3 in pattern"));

		MDC::put(_T("key1"), _T("value1"));
		MDC::put(_T("key2"), _T("value2"));

		layout->setConversionPattern(mdcMsgPattern1);
		layout->activateOptions();
		root->debug(_T("filled mdc, no key specified in pattern"));

		layout->setConversionPattern(mdcMsgPattern2);
		layout->activateOptions();
		root->debug(_T("filled mdc, key1 in pattern"));

		layout->setConversionPattern(mdcMsgPattern3);
		layout->activateOptions();
		root->debug(_T("filled mdc, key2 in pattern"));

		layout->setConversionPattern(mdcMsgPattern4);
		layout->activateOptions();
		root->debug(_T("filled mdc, key3 in pattern"));

		layout->setConversionPattern(mdcMsgPattern5);
		layout->activateOptions();
		root->debug(_T("filled mdc, key1, key2, and key3 in pattern"));

		MDC::remove(_T("key1"));
		MDC::remove(_T("key2"));

		layout->setConversionPattern(_T("%m%n"));
		layout->activateOptions();
		root->debug(_T("finished mdc pattern test"));

		CPPUNIT_ASSERT(Compare::compare(OUTPUT_FILE, WITNESS_FILE));
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

CPPUNIT_TEST_SUITE_REGISTRATION(PatternLayoutTest);
