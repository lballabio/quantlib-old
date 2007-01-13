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

#include <log4cxx/level.h>

namespace log4cxx
{
	class XLevel : public Level
	{
		DECLARE_LOG4CXX_LEVEL(XLevel)

	public:
        enum
        {
            TRACE_INT = Level::DEBUG_INT - 1,
			LETHAL_INT = Level::FATAL_INT + 1
        };

		static const LevelPtr TRACE;
		static const LevelPtr LETHAL;

		XLevel(int level, const String& levelStr, int syslogEquivalent);
		/**
		Convert the string passed as argument to a level. If the
		conversion fails, then this method returns #DEBUG.
		*/
		static const LevelPtr& toLevel(const String& sArg);

		/**
		Convert an integer passed as argument to a level. If the
		conversion fails, then this method returns #DEBUG.

		*/
		static const LevelPtr& toLevel(int val);

		/**
		Convert an integer passed as argument to a level. If the
		conversion fails, then this method returns the specified default.
		*/
		static const LevelPtr& toLevel(int val, const LevelPtr& defaultLevel);


		/**
		Convert the string passed as argument to a level. If the
		conversion fails, then this method returns the value of
		<code>defaultLevel</code>.
		*/
		static const LevelPtr& toLevel(const String& sArg,
			const LevelPtr& defaultLevel);
	};
} 
