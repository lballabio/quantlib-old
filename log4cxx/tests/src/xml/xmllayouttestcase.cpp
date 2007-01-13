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
#include <log4cxx/xml/xmllayout.h>
#include <log4cxx/fileappender.h>
#include <log4cxx/mdc.h>

#include "../util/transformer.h"
#include "../util/compare.h"
#include "../util/xmltimestampfilter.h"
#include "../util/xmllineattributefilter.h"
#include "../util/xmlthreadfilter.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::xml;

#define FILTERED _T("output/filtered")
#define TEMP _T("output/temp")

class X 
{
public:
	X()
	{
		LoggerPtr logger = 
			Logger::getLogger(_T("org.apache.log4j.xml.XMLLayoutTestCase$X"));
		LOG4CXX_INFO(logger, _T("in X() constructor"));
	}
};

class XMLLayoutTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(XMLLayoutTestCase);
		CPPUNIT_TEST(basic);
		CPPUNIT_TEST(locationInfo);
		CPPUNIT_TEST(testCDATA);
		CPPUNIT_TEST(testNULL);
		CPPUNIT_TEST(testMDC);
	CPPUNIT_TEST_SUITE_END();

	LoggerPtr root;
	LoggerPtr logger;

public:
	void setUp()
	{
		root = Logger::getRootLogger();
		logger = Logger::getLogger(_T("org.apache.log4j.xml.XMLLayoutTestCase"));
	}

	void tearDown()
	{
		logger->getLoggerRepository()->resetConfiguration();
	}

	void basic()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		common();

		XMLTimestampFilter xmlTimestampFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlThreadFilter);

		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.1")));
	}
	
	void locationInfo()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		xmlLayout->setLocationInfo(true);
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		common();
		
		XMLTimestampFilter xmlTimestampFilter;
		XMLLineAttributeFilter xmlLineAttributeFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlLineAttributeFilter);
		filters.push_back(&xmlThreadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.2")));
	}
	
	void testCDATA()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		xmlLayout->setLocationInfo(true);
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		
		LOG4CXX_DEBUG(logger,
			_T("Message with embedded <![CDATA[<hello>hi</hello>]]>."));
		
		XMLTimestampFilter xmlTimestampFilter;
		XMLLineAttributeFilter xmlLineAttributeFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlLineAttributeFilter);
		filters.push_back(&xmlThreadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.3")));
	}

	void testNULL()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		
		LOG4CXX_DEBUG(logger, _T("hi"));
		LOG4CXX_DEBUG(logger, _T(""));
		
		XMLTimestampFilter xmlTimestampFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlThreadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.null")));
	}
	
	void testMDC()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		
		MDC::clear();
		MDC::put(_T("key1"), _T("val1"));
		MDC::put(_T("key2"), _T("val2"));

		LOG4CXX_DEBUG(logger, _T("Hello"));
		
		MDC::clear();

		XMLTimestampFilter xmlTimestampFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlThreadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.mdc.1")));
	}
	
	// not incuded in the tests for the moment !
	void holdTestMDCEscaped()
	{
		XMLLayoutPtr xmlLayout = new XMLLayout();
		root->addAppender(new FileAppender(xmlLayout, TEMP, false));
		
		MDC::clear();
		MDC::put(_T("blahAttribute"), _T("<blah value=\"blah\">"));
		MDC::put(_T("<blahKey value=\"blah\"/>"), _T("blahValue"));

		LOG4CXX_DEBUG(logger, _T("Hello"));
		
		MDC::clear();

		XMLTimestampFilter xmlTimestampFilter;
		XMLThreadFilter xmlThreadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&xmlTimestampFilter);
		filters.push_back(&xmlThreadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/xmlLayout.mdc.2")));
	}
	
	void common()
	{
		int i = -1;
		X x;

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

CPPUNIT_TEST_SUITE_REGISTRATION(XMLLayoutTestCase);
