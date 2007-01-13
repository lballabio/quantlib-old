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

#ifndef _LOG4CXX_TESTS_UTIL_TRANSFORMER_H
#define _LOG4CXX_TESTS_UTIL_TRANSFORMER_H

#include "filter.h"
#include <vector>

namespace log4cxx
{
	class Transformer
	{
	public:
		static void transform(const String& in, const String& out,
			const std::vector<Filter *>& filters) throw(UnexpectedFormatException);

		static void transform(const String& in, const String& out,
			const Filter& filter) throw(UnexpectedFormatException);
	};
} 

#endif //_LOG4CXX_TESTS_UTIL_TRANSFORMER_H
