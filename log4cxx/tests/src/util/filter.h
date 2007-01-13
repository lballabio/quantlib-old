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

#ifndef _LOG4CXX_TESTS_UTIL_FILTER_H
#define _LOG4CXX_TESTS_UTIL_FILTER_H

#include <log4cxx/helpers/tchar.h>
#include <log4cxx/helpers/exception.h>

#define BASIC_PAT _T("\\[\\d*\\] (FATAL|ERROR|WARN|INFO|DEBUG)")
#define ISO8601_PAT _T("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2},\\d{3}")
#define ABSOLUTE_DATE_AND_TIME_PAT \
	_T("^\\d{1,2} .{2,6}\\.? 200\\d \\d{2}:\\d{2}:\\d{2},\\d{3}")
#define ABSOLUTE_TIME_PAT _T("^\\d{2}:\\d{2}:\\d{2},\\d{3}")
#define RELATIVE_TIME_PAT _T("^\\d{1,10}")

namespace log4cxx
{
	class UnexpectedFormatException : public helpers::Exception
	{
	public:
		UnexpectedFormatException(const String& message)
		: message(message) {}

		virtual String getMessage()
			{ return message; }

	protected:
		String message;
	};

	class Filter
	{
	public:
		virtual String filter(const String& in)
			const throw(UnexpectedFormatException) = 0;
			
		static String merge(const String& pattern, const String& in, const String& fmt);
		static bool match(const String& pattern, const String& in);
	};
} 

#endif //_LOG4CXX_TESTS_UTIL_FILTER_H
