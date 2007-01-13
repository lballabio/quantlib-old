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

#include "transformer.h"
#include <fstream>

using namespace log4cxx;

typedef std::basic_ifstream<TCHAR> ifstream;
typedef std::basic_ofstream<TCHAR> ofstream;


void Transformer::transform(const String& in, const String& out,
	const std::vector<Filter *>& filters) throw(UnexpectedFormatException)
{
	String line;
	USES_CONVERSION
	ifstream input(T2A(in.c_str()));
	ofstream output(T2A(out.c_str()));

	while (!std::getline(input, line).fail())
	{
		for (std::vector<Filter *>::size_type i = 0; i < filters.size(); i++)
		{
			line = filters[i]->filter(line);
		}
		if (!line.empty())
		{
			output << line << std::endl;
		}
	}
}

void Transformer::transform(const String& in, const String& out,
	const Filter& filter) throw(UnexpectedFormatException)
{
	String line;
	USES_CONVERSION
	ifstream input(T2A(in.c_str()));
	ofstream output(T2A(out.c_str()));

	while (!std::getline(input, line).fail())
	{
		line = filter.filter(line);
		output << line << std::endl;
	}

}
