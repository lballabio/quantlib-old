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

#include "compare.h"
#include <fstream>

typedef std::basic_ifstream<TCHAR> ifstream;

using namespace log4cxx;

bool Compare::compare(const String& file1, const String& file2)
{
	USES_CONVERSION
	ifstream in1(T2A(file1.c_str()));
	ifstream in2(T2A(file2.c_str()));

	String s1;
	int lineCounter = 0;

	while (!std::getline(in1, s1).fail())
	{
		lineCounter++;

		String s2;
		std::getline(in2, s2);

		if (s1 != s2)
		{
			tcout << _T("Files [") << file1 << _T("] and [") << file2
				<< _T("] differ on line ") << lineCounter << std::endl;
			tcout << _T("One reads:  [") << s1 << _T("].") << std::endl;
			tcout << _T("Other reads:[") << s2 << _T("].") << std::endl;
			outputFile(file1);
			outputFile(file2);

			return false;
		}
	}

	// the second file is longer
	if (in2.get() != ifstream::traits_type::eof())
	{
		tcout << _T("File [") << file2 << _T("] longer than file [") << file1 << _T("].")
		<< std::endl;
		outputFile(file1);
		outputFile(file2);

		return false;
	}

	return true;
}

void Compare::outputFile(const String& file)
{
	USES_CONVERSION;
	ifstream in1(T2A(file.c_str()));

	String s1;
	int lineCounter = 0;
	tcout << _T("--------------------------------") << std::endl;
	tcout << _T("Contents of ") << file << _T(":") << std::endl;

	while (!std::getline(in1, s1).fail())
	{
		lineCounter++;
		tcout << lineCounter;

		if (lineCounter < 10)
		{
			tcout << _T("   : ");
		}
		else if (lineCounter < 100)
		{
			tcout << _T("  : ");
		}
		else if (lineCounter < 1000)
		{
			tcout << _T(" : ");
		}
		else
		{
			tcout << _T(": ");
		}

		tcout << s1 << std::endl;
	}
}
