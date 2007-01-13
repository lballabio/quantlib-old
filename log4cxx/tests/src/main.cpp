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

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include <stdexcept>
#include <iostream>

int main( int argc, char **argv)
{
	CppUnit::TextUi::TestRunner runner;

	CppUnit::TestFactoryRegistry &registry =
		CppUnit::TestFactoryRegistry::getRegistry();

	runner.addTest(registry.makeTest());
	
	bool wasSuccessful = true;
	if (argc > 1)
	{
		for (int n = 1; n < argc; n++)
		{
			try
			{
				wasSuccessful = runner.run(argv[n], false) && wasSuccessful;
			}
			catch(std::exception& e)
			{
				std::cout << e.what() << std::endl;
			}
		}
	}
	else
	{
		bool wasSuccessful = runner.run("", false);
	}

	return wasSuccessful ? EXIT_SUCCESS : EXIT_FAILURE;
}
