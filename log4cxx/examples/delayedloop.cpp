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
 
#include <log4cxx/logger.h>
#include <log4cxx//helpers/stringhelper.h>
#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/helpers/thread.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

/**
This test program sits in a loop and logs things. Its logging is
configured by a configuration file. Changes to this configuration
file are monitored and when a change occurs, the config file is re-read.
*/
class DelayedLoop
{
	static LoggerPtr logger;

public:
	static void main(int argc, char **argv)
	{
		if(argc == 2) 
		{
			USES_CONVERSION;
			init(A2T(argv[1]));
		}
		else 
		{
			usage(argv[0], "Wrong number of arguments.");
		}

		test();
	}
	
	static void usage(const char * programName, const char * msg)
	{
		std::cout << msg << std::endl;
		std::cout << "Usage: " << programName <<
				" configFile" << std::endl;
		exit(1);
	}


	static void init(const String& configFile)
	{
#ifdef HAVE_XML
		if(StringHelper::endsWith(configFile, _T("xml")))
		{
			xml::DOMConfigurator::configureAndWatch(configFile, 3000);
		} 
		else
#endif
		{
			PropertyConfigurator::configureAndWatch(configFile, 3000);
		}
	}

	static void test()
	{
		int i = 0;
		while(true)
		{
			LOG4CXX_DEBUG(logger, _T("MSG ") << i++);
			try
			{
				Thread::sleep(1000);
			} 
			catch(Exception& e)
			{
			}
		}
	}
};

LoggerPtr DelayedLoop::logger = Logger::getLogger(_T("DelayedLoop"));

int main(int argc, char **argv)
{
    int result = EXIT_SUCCESS;
    try
    {
		DelayedLoop::main(argc, argv);
	}
	catch(Exception&)
	{
		result = EXIT_FAILURE;
	}

    return result;
}
