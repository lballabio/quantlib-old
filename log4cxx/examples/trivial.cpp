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
 
#include <stdlib.h>
#include <log4cxx/logger.h>
#include <log4cxx/basicconfigurator.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/ndc.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

int main()
{
    int result = EXIT_SUCCESS;
    try
    {
		BasicConfigurator::configure();
 		LoggerPtr rootLogger = Logger::getRootLogger();

		NDC::push(_T("trivial context"));

		rootLogger->debug(_T("debug message"));
		rootLogger->info(_T("info message"));
		rootLogger->warn(_T("warn message"));
		rootLogger->error(_T("error message"));
		rootLogger->fatal(_T("fatal message"));
	}
	catch(Exception&)
	{
		result = EXIT_FAILURE;
	}

    return result;
}
