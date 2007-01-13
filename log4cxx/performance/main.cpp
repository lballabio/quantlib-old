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
 
#include <log4cxx/helpers/tchar.h>
#include <log4cxx/ndc.h>
#include <log4cxx/logmanager.h>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/xml/domconfigurator.h>
#include <log4cxx/helpers/system.h>
#include <log4cxx/helpers/thread.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

int runLength;
int delay = -1;
/*
 A delay is applied after every <code>burstLen</code> log
 requests.  The default value of this constant is 100.  */
int burstLen = 100;
int DELAY_MULT = 1000/burstLen;

LoggerPtr logger = Logger::getLogger(_T("A0123456789.B0123456789.C0123456789"));

void Usage(const String& processName, const String& msg)
{
	tcerr << msg << std::endl;
	tcerr <<
		_T("Usage: ") << processName 
		<< _T(" confFile runLength [delay] [burstLen]") << std::endl
		<< _T("       confFile is an XML configuration file and") << std::endl
		<< _T("       runLength (integer) is the length of test loop.") << std::endl
		<< _T("       delay is the time in millisecs to wait every burstLen log requests.") << std::endl;
	exit(EXIT_FAILURE);
}

void init(const String& configFile, const String& runLengthStr,
		  const String& delayStr, const String& burstLenStr)
{
	runLength = OptionConverter::toInt(runLengthStr, runLength);
	if (runLength < 1)
	{
		throw IllegalArgumentException(_T("run Length must be greater than 0"));
	}
	if (!delayStr.empty())
	{
		delay = OptionConverter::toInt(delayStr, delay);
	}
	if (!burstLenStr.empty())
	{
		burstLen = OptionConverter::toInt(burstLenStr, burstLen);
		DELAY_MULT = 1000/burstLen;
	}

#ifdef HAVE_XML	
	xml::DOMConfigurator::configure(configFile);
#endif
}

double NoDelayLoop(LoggerPtr logger, const String& msg)
{
	int64_t before = System::currentTimeMillis();
    for(int i = 0; i < runLength; i++)
	{
		logger->info(msg, __FILE__, __LINE__);
    }
	int64_t after = System::currentTimeMillis();
	return ((after - before)*1000.0)/runLength;
}

double DelayedLoop(LoggerPtr logger, const String& msg)
{

    int64_t before = System::currentTimeMillis();
    int j = 0;
    for(int i = 0; i < runLength; i++)
	{
		logger->info(msg);
		if(j++ == burstLen)
		{
			j = 0;
			try
			{
				Thread::sleep(delay);
			}
			catch(Exception&)
			{
			}
		}
		
    }
    double actualTime = ((System::currentTimeMillis()-before)*1000.0/runLength);
    tcout << "actual time: " << actualTime << std::endl;
    return (actualTime - delay*DELAY_MULT);
}

int main(int argc, char* argv[])
{
	int ret = EXIT_SUCCESS;
	
	try
	{
		USES_CONVERSION;

		if(argc == 3)
			init(A2T(argv[1]), A2T(argv[2]), String(), String());
		else if(argc == 5)
			init(A2T(argv[1]), A2T(argv[2]), A2T(argv[3]), A2T(argv[4]));
		else
			Usage(A2T(argv[0]), _T("Wrong number of arguments."));
		
		
		NDC::push(_T("some context"));

		double delta;
		String msg = _T("ABCDEGHIJKLMNOPQRSTUVWXYZabcdeghijklmnopqrstuvwxyz1234567890");
		if(delay <= 0)
		{
			delta = NoDelayLoop(logger, msg);
		}
		else
		{
			delta = DelayedLoop(logger, msg);
		}
		
		tcout << (int)delta << std::endl;
		
		LogManager::shutdown();
	}
	catch(Exception&)
	{
		ret = EXIT_FAILURE;
	}

	return ret;
}
