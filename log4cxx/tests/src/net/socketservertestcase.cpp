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

#ifdef WIN32
	#include <windows.h>
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <log4cxx/logger.h>
#include <log4cxx/net/socketappender.h>
#include <log4cxx/helpers/thread.h>
#include <log4cxx/ndc.h>
#include <log4cxx/mdc.h>
#include <log4cxx/asyncappender.h>

#include "socketservertestcase.h"
#include "../util/compare.h"
#include "../util/transformer.h"
#include "../util/linenumberfilter.h"
#include "../util/controlfilter.h"
#include "../util/absolutedateandtimefilter.h"
#include "../util/threadfilter.h"
#include "../xml/xlevel.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;

#define TEMP _T("output/temp")
#define FILTERED _T("output/filtered")

// %5p %x [%t] %c %m%n
// DEBUG T1 [thread] org.apache.log4j.net.SocketAppenderTestCase Message 1
#define PAT1 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) T1 \\[\\d*]\\ ") \
	_T(".* Message \\d{1,2}")

// DEBUG T2 [thread] patternlayouttest.cpp(?) Message 1
#define PAT2 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) T2 \\[\\d*]\\ ") \
	_T(".*socketservertestcase.cpp\\(\\d{1,4}\\) Message \\d{1,2}")

// DEBUG T3 [thread] patternlayouttest.cpp(?) Message 1
#define PAT3 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) T3 \\[\\d*]\\ ") \
	_T(".*socketservertestcase.cpp\\(\\d{1,4}\\) Message \\d{1,2}")

// DEBUG some T4 MDC-TEST4 [thread] SocketAppenderTestCase - Message 1   
// DEBUG some T4 MDC-TEST4 [thread] SocketAppenderTestCase - Message 1 
#define PAT4 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) some T4 MDC-TEST4 \\[\\d*]\\") \
	_T(" (root|SocketServerTestCase) - Message \\d{1,2}")
#define PAT5 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) some5 T5 MDC-TEST5 \\[\\d*]\\") \
	_T(" (root|SocketServerTestCase) - Message \\d{1,2}")
#define PAT6 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) some6 T6 client-test6 MDC-TEST6") \
	_T(" \\[\\d*]\\ (root|SocketServerTestCase) - Message \\d{1,2}")
#define PAT7 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) some7 T7 client-test7 MDC-TEST7") \
	_T(" \\[\\d*]\\ (root|SocketServerTestCase) - Message \\d{1,2}")

// DEBUG some8 T8 shortSocketServer MDC-TEST7 [thread] SocketServerTestCase - Message 1
#define PAT8 \
	_T("^(DEBUG| INFO| WARN|ERROR|FATAL|LETHAL) some8 T8 shortSocketServer") \
	_T(" MDC-TEST8 \\[\\d*]\\ (root|SocketServerTestCase) - Message \\d{1,2}")

class ShortSocketServerLauncher
{
public: 
	ShortSocketServerLauncher()
	{
		if (!launched)
		{
#ifdef WIN32
			PROCESS_INFORMATION pi;
			STARTUPINFO si;
			ZeroMemory( &si, sizeof(si) );
			si.cb = sizeof(si);
			ZeroMemory( &pi, sizeof(pi) );
			String commandLine(_T("shortsocketserver 8 input/socketServer"));

			BOOL bResult = ::CreateProcess(NULL, (LPTSTR)commandLine.c_str(), NULL, NULL,
				TRUE, 0, NULL, NULL, &si, &pi);
#else
			if(!::fork())
			{
				::execl("src/shortsocketserver", "shortsocketserver",
					"8", "input/socketServer", 0);
				::perror("execl() failed"); 
				::exit(1);
			}
			else
			{
				sleep(1);
			}
#endif
			launched = true;
		}
	}

	static bool launched;

};

bool ShortSocketServerLauncher::launched = false;


class SocketServerTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(SocketServerTestCase);
		CPPUNIT_TEST(test1);
		CPPUNIT_TEST(test2);
		CPPUNIT_TEST(test3);
		CPPUNIT_TEST(test4);
		CPPUNIT_TEST(test5);
		CPPUNIT_TEST(test6);
		CPPUNIT_TEST(test7);
		CPPUNIT_TEST(test8);
	CPPUNIT_TEST_SUITE_END();
	
	SocketAppenderPtr socketAppender;
	LoggerPtr logger;
	LoggerPtr root;


public:
	void setUp()
	{
		ShortSocketServerLauncher();
  		logger = Logger::getLogger(_T("org.apache.log4j.net.SocketServerTestCase"));
		root = Logger::getRootLogger();
	}

	void tearDown()
	{
		socketAppender = 0;
		root->getLoggerRepository()->resetConfiguration();
		logger = 0;
		root = 0;
	}
	
	/**
	The pattern on the server side: %5p %x [%t] %c %m%n.
	
	We are testing NDC functionality across the wire.
	*/
	void test1()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		root->addAppender(socketAppender);
		common(_T("T1"), _T("key1"), _T("MDC-TEST1"));
		delay(1);

		ControlFilter cf;
		cf << PAT1;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.1")));
	}
	
	void test2()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		root->addAppender(socketAppender);
		common(_T("T2"), _T("key2"), _T("MDC-TEST2"));
		delay(1);

		ControlFilter cf;
		cf << PAT2;
		
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

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.2")));
	}
	
 	void test3()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		root->addAppender(socketAppender);
		common(_T("T3"), _T("key3"), _T("MDC-TEST3"));
		delay(1);

		ControlFilter cf;
		cf << PAT3;
		
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

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.3")));
	}
	
 	void test4()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		root->addAppender(socketAppender);
		NDC::push(_T("some"));
		common(_T("T4"), _T("key4"), _T("MDC-TEST4"));
		NDC::pop();
		delay(1);

		ControlFilter cf;
		cf << PAT4;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.4")));
	}
	
	void test5()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		AsyncAppenderPtr asyncAppender = new AsyncAppender();
		
		root->addAppender(socketAppender);
		root->addAppender(asyncAppender);
		
		NDC::push(_T("some5"));
		common(_T("T5"), _T("key5"), _T("MDC-TEST5"));
		NDC::pop();
		delay(2);

		ControlFilter cf;
		cf << PAT5;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.5")));
	}
	
	void test6()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		AsyncAppenderPtr asyncAppender = new AsyncAppender();
		
		root->addAppender(socketAppender);
		root->addAppender(asyncAppender);
		
		NDC::push(_T("some6"));
    	MDC::put(_T("hostID"), _T("client-test6"));
		common(_T("T6"), _T("key6"), _T("MDC-TEST6"));
		NDC::pop();
  		MDC::remove(_T("hostID"));
		delay(2);

		ControlFilter cf;
		cf << PAT6;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.6")));
	}
	
	void test7()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		AsyncAppenderPtr asyncAppender = new AsyncAppender();
		
		root->addAppender(socketAppender);
		root->addAppender(asyncAppender);
		
		NDC::push(_T("some7"));
    	MDC::put(_T("hostID"), _T("client-test7"));
		common(_T("T7"), _T("key7"), _T("MDC-TEST7"));
		NDC::pop();
  		MDC::remove(_T("hostID"));
		delay(2);

		ControlFilter cf;
		cf << PAT7;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.7")));
	}
	
	void test8()
	{
		SocketAppenderPtr socketAppender = 
			new SocketAppender(_T("localhost"), PORT);
		
		root->addAppender(socketAppender);
		
		NDC::push(_T("some8"));
 		common(_T("T8"), _T("key8"), _T("MDC-TEST8"));
		NDC::pop();
		delay(2);

		ControlFilter cf;
		cf << PAT8;
		
		ThreadFilter threadFilter;

		std::vector<Filter *> filters;
		filters.push_back(&cf);
		filters.push_back(&threadFilter);
		
		try
		{
			Transformer::transform(TEMP, FILTERED, filters);
		}
		catch(UnexpectedFormatException& e)
		{
			tcout << _T("UnexpectedFormatException :") << e.getMessage() << std::endl;
			throw;
		}

		CPPUNIT_ASSERT(Compare::compare(FILTERED, _T("witness/socketServer.8")));
	}
	
	void common(const String& dc, const String& key, const String& val) 
	{
		int i = -1;
		NDC::push(dc);
		MDC::put(key, val);

		i++;
		LOG4CXX_LOG(logger, XLevel::TRACE, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(logger, _T("Message ") << i);
		i++;
		LOG4CXX_DEBUG(root, _T("Message ") << i);
		i++;
		LOG4CXX_INFO(logger, _T("Message ") << i);
		i++;
		LOG4CXX_WARN(logger, _T("Message ") << i);
		i++;
		LOG4CXX_LOG(logger, XLevel::LETHAL, _T("Message ") << i); //5	
		
		NDC::pop();
		MDC::remove(key);
	}

	void delay(int secs) 
	{
		Thread::sleep(secs * 1000);
	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(SocketServerTestCase);
