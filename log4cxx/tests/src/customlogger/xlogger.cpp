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

#include "xlogger.h"
#include <log4cxx/level.h>
#include <log4cxx/logmanager.h>

using namespace log4cxx;

IMPLEMENT_LOG4CXX_OBJECT(XLogger)
IMPLEMENT_LOG4CXX_OBJECT(XFactory)

String XLogger::FQCN = XLogger::getStaticClass().getName() + _T(".");
XFactoryPtr XLogger::factory = new XFactory();

void XLogger::debug(const String& message, const char* file, int line)
{
	if (repository->isDisabled(Level::DEBUG_INT))
	{
		return;
	}

	if (XLevel::LETHAL->isGreaterOrEqual(this->getEffectiveLevel()))
	{
		forcedLog(FQCN, Level::DEBUG, message + _T(" ") + suffix, file,line);
	}
}

void XLogger::lethal(const String& message, const char* file, int line)
{
	if (repository->isDisabled(XLevel::LETHAL_INT))
	{
		return;
	}

	if (XLevel::LETHAL->isGreaterOrEqual(this->getEffectiveLevel()))
	{
		forcedLog(FQCN, XLevel::LETHAL, message, file,line);
	}
}

void XLogger::lethal(const String& message)
{
	if (repository->isDisabled(XLevel::LETHAL_INT))
	{
		return;
	}

	if (XLevel::LETHAL->isGreaterOrEqual(this->getEffectiveLevel()))
	{
		forcedLog(FQCN, XLevel::LETHAL, message);
	}
}

LoggerPtr XLogger::getLogger(const String& name)
{
	return LogManager::getLogger(name, factory);
}

LoggerPtr XLogger::getLogger(const helpers::Class& clazz)
{
	return XLogger::getLogger(clazz.getName());
}

void XLogger::trace(const String& message, const char* file, int line)
{
	if (repository->isDisabled(XLevel::TRACE_INT))
	{
		return;
	}

	if (XLevel::TRACE->isGreaterOrEqual(this->getEffectiveLevel()))
	{
		forcedLog(FQCN, XLevel::TRACE, message, file, line);
	}
}

void XLogger::trace(const String& message)
{
	if (repository->isDisabled(XLevel::TRACE_INT))
	{
		return;
	}

	if (XLevel::TRACE->isGreaterOrEqual(this->getEffectiveLevel()))
	{
		forcedLog(FQCN, XLevel::TRACE, message);
	}
}

XFactory::XFactory()
{
}

LoggerPtr XFactory::makeNewLoggerInstance(const String& name)
{
	return new XLogger(name);
}
