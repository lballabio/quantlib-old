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

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/properties.h>
#include <log4cxx/helpers/system.h>


using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;

#define MAX 1000

class OptionConverterTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(OptionConverterTestCase);
		CPPUNIT_TEST(varSubstTest1);
		CPPUNIT_TEST(varSubstTest2);
		CPPUNIT_TEST(varSubstTest3);
		CPPUNIT_TEST(varSubstTest4);
		CPPUNIT_TEST(varSubstTest5);
	CPPUNIT_TEST_SUITE_END();
	
	Properties props;
	Properties nullProperties;
	
public:
	void setUp()
	{
		props.setProperty(_T("TOTO"), _T("wonderful"));
		props.setProperty(_T("key1"), _T("value1"));
		props.setProperty(_T("key2"), _T("value2"));
		System::setProperties(props);
	}

	void tearDown()
	{
	}
  
	void varSubstTest1()
	{
		String r;
		
		r = OptionConverter::substVars(_T("hello world."), nullProperties);
		CPPUNIT_ASSERT(r == _T("hello world."));

		r = OptionConverter::substVars(_T("hello ${TOTO} world."), nullProperties);

		CPPUNIT_ASSERT(r == _T("hello wonderful world."));
	}

  
	void varSubstTest2()
	{
		String r;
		
		r = OptionConverter::substVars(_T("Test2 ${key1} mid ${key2} end."),
			nullProperties);
		CPPUNIT_ASSERT(r == _T("Test2 value1 mid value2 end."));
	}
	
	
	void varSubstTest3() 
	{
		String r;
		
		r = OptionConverter::substVars(
			_T("Test3 ${unset} mid ${key1} end."), nullProperties);
		CPPUNIT_ASSERT(r == _T("Test3  mid value1 end."));
	}
	
	
	void varSubstTest4()
	{
		String res;
		String val = _T("Test4 ${incomplete ");
		try 
		{
			res = OptionConverter::substVars(val, nullProperties);
		}
		catch(IllegalArgumentException& e)
		{
			String witness = String(_T("\""))+val
				+ _T("\" has no closing brace. Opening brace at position 6.");
			String errorMsg = e.getMessage();
			CPPUNIT_ASSERT(errorMsg == witness);
		}
	}
	
	
	void varSubstTest5()
	{
		Properties props;
		props.setProperty(_T("p1"), _T("x1"));
		props.setProperty(_T("p2"), _T("${p1}"));
		String res = OptionConverter::substVars(_T("${p2}"), props);
		CPPUNIT_ASSERT(res == _T("x1"));
	}
};

CPPUNIT_TEST_SUITE_REGISTRATION(OptionConverterTestCase);
