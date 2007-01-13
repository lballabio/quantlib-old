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

#include <boost/regex.hpp>
#include "filter.h"

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace boost;

String Filter::merge(const String& pattern, const String& in, const String& fmt)
{
	USES_CONVERSION;
	std::string convPattern = T2A(pattern.c_str());
	std::string convIn = T2A(in.c_str());
	std::string convFmt = T2A(fmt.c_str());
	
	std::string result = RegEx(convPattern).Merge(convIn, convFmt);
	return A2T(result.c_str());
	
}

bool Filter::match(const String& pattern, const String& in)
{
	USES_CONVERSION;
	std::string convPattern = T2A(pattern.c_str());
	std::string convIn = T2A(in.c_str());

	return RegEx(convPattern).Match(convIn);
}

