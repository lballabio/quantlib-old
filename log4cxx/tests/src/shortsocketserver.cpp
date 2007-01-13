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
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/serversocket.h>
#include <log4cxx/mdc.h>
#include <log4cxx/propertyconfigurator.h>
#include <log4cxx/logmanager.h>
#include <log4cxx/helpers/inetaddress.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/net/socketnode.h>
#include <log4cxx/helpers/thread.h>

#include "net/socketservertestcase.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::net;
  
class ShortSocketServer
{
	static LoggerPtr logger;

public:
	static void main(int argc, char **argv)
	{
		int totalTests = 0;
		String prefix;

		if (argc == 3)
		{
			USES_CONVERSION
			totalTests = OptionConverter::toInt(A2T(argv[1]), totalTests);
			prefix = A2T(argv[2]);
		} 
		else
		{
			usage(argv[0], "Wrong number of arguments."); 
		}


		LOGLOG_DEBUG(_T("Listening on port ") << PORT);
		ServerSocket serverSocket(PORT);

		MDC::put(_T("hostID"), _T("shortSocketServer"));

		for (int i = 1; i <= totalTests; i++)
		{
			StringBuffer sbuf;
			sbuf << prefix << i  << _T(".properties");
			PropertyConfigurator::configure(sbuf.str());
			LOGLOG_DEBUG(_T("Waiting to accept a new client."));
			SocketPtr socket = serverSocket.accept();
			LOGLOG_DEBUG(_T("Connected to client at ") << 
				socket->getInetAddress().toString());
			LOGLOG_DEBUG(_T("Starting new socket node."));	
			SocketNodePtr sn = new SocketNode(socket, LogManager::getLoggerRepository());
			ThreadPtr t = new Thread(sn);
			t->start(); 
			t->join();
		}
	}


	static void usage(const char * programName, const char * msg)
	{
		std::cout << msg << std::endl;
		std::cout << "Usage: " << programName;
		std::cout << " totalTests configFilePrefix" << std::endl;
		exit(1);
	}    
};

LoggerPtr ShortSocketServer::logger =
	Logger::getLogger(_T("org.apache.log4j.net.ShortSocketServer"));

int main(int argc, char **argv)
{
    int result = EXIT_SUCCESS;
    try
    {
		ShortSocketServer::main(argc, argv);
	}
	catch(Exception&)
	{
		result = EXIT_FAILURE;
	}

    return result;
}
