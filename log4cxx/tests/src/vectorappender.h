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

#include <log4cxx/appenderskeleton.h>
#include <vector>
#include <log4cxx/spi/loggingevent.h>

namespace log4cxx
{
	class VectorAppender;
	typedef helpers::ObjectPtrT<VectorAppender> VectorAppenderPtr;


	/**
	An appender that appends logging events to a vector.
	*/
	class VectorAppender : public AppenderSkeleton
	{
	public:
		DECLARE_LOG4CXX_OBJECT(VectorAppender)

		std::vector<spi::LoggingEventPtr> vector;

		/**
		Does nothing.
		*/
		void activateOptions() {}

		/**
		This method is called by the AppenderSkeleton#doAppend
		method.
		*/
		void append(const spi::LoggingEventPtr& event);

		const std::vector<spi::LoggingEventPtr>& getVector() const
			{ return vector; }

		void close();

		bool isClosed() const
			{ return closed; }

		bool requiresLayout() const
			{ return false;	}
	};
} 
