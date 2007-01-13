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

#ifndef _LOG4CXX_PERFORMANCE_NULL_APPENDER_H
#define _LOG4CXX_PERFORMANCE_NULL_APPENDER_H

#include <log4cxx/config.h>
#include <log4cxx/helpers/tchar.h>
#include <log4cxx/appenderskeleton.h>


namespace log4cxx
{
	class Layout;
	typedef helpers::ObjectPtrT<Layout> LayoutPtr;

	namespace performance
	{
		class NullAppender;
		typedef helpers::ObjectPtrT<NullAppender> NullAppenderPtr;

		/**
		* A bogus appender which calls the format method of its layout object
		* but does not write the result anywhere.
		* */
		class NullAppender : public AppenderSkeleton
		{
		public:
			StringBuffer sbuf;

			DECLARE_LOG4CXX_OBJECT(NullAppender)
			BEGIN_LOG4CXX_CAST_MAP()
				LOG4CXX_CAST_ENTRY(NullAppender)
				LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
			END_LOG4CXX_CAST_MAP()

			NullAppender();
			NullAppender(const LayoutPtr& layout);
			void close();
			void doAppend(const spi::LoggingEventPtr& event);
			void append(const spi::LoggingEventPtr& event);

			/**
			This is a bogus appender but it still uses a layout.
			*/
			bool requiresLayout() const;
		}; // NullAppender
	}  // namespace performance
}; // namespace log4cxx

#endif //_LOG4CXX_PERFORMANCE_NULL_APPENDER_H
