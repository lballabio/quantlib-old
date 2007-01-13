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

#include "nullappender.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace log4cxx::performance;


IMPLEMENT_LOG4CXX_OBJECT(NullAppender)

NullAppender::NullAppender()
{
}

NullAppender::NullAppender(const LayoutPtr& layout)
{
	this->layout = layout;
}

void NullAppender::close()
{
}

void NullAppender::doAppend(const LoggingEventPtr& event)
{
	if (layout != 0)
	{
		sbuf.seekp(0);
		layout->format(sbuf, event);
	}
}

void NullAppender::append(const LoggingEventPtr& event)
{
}

/**
This is a bogus appender but it still uses a layout.
*/
bool NullAppender::requiresLayout() const
{
	return true;
}
