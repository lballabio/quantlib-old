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

#include "xlevel.h"
#include <log4cxx/helpers/stringhelper.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_LEVEL(XLevel)

#define TRACE_STR _T("TRACE")
#define LETHAL_STR _T("LETHAL")

const LevelPtr XLevel::TRACE = new XLevel(XLevel::TRACE_INT, TRACE_STR, 7);
const LevelPtr XLevel::LETHAL = new XLevel(XLevel::LETHAL_INT, LETHAL_STR, 0);

XLevel::XLevel(int level, const String& levelStr, int syslogEquivalent)
: Level(level, levelStr, syslogEquivalent)
{
}

const LevelPtr& XLevel::toLevel(const String& sArg)
{
	return toLevel(sArg, TRACE);
}

const LevelPtr& XLevel::toLevel(int val)
{
	return toLevel(val, TRACE);
}

const LevelPtr& XLevel::toLevel(int val, const LevelPtr& defaultLevel)
{
	switch(val)
	{
		case TRACE_INT: return TRACE;
		case LETHAL_INT: return LETHAL;
		default: return defaultLevel;
	}
}

const LevelPtr& XLevel::toLevel(const String& sArg, const LevelPtr& defaultLevel)
{
   if (sArg.empty())
    {
       return defaultLevel;
    }

    String s = StringHelper::toUpperCase(sArg);

    if(s == (TRACE_STR)) return TRACE;
    if(s == (LETHAL_STR)) return LETHAL;

    return defaultLevel;
}
