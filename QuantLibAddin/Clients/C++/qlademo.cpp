/*
 Copyright (C) 2004 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifdef WIN32
#pragma warning(disable: 4786)
#pragma warning(disable: 4503)
#endif

#include <Addins/C++/qladdin.hpp>
#include <iostream>

using namespace std;
using namespace ObjHandler;

int main() {
	try {
		cout << "hi" << endl;

		QL_LOGFILE("quantlib.log");
		QL_LOGMESSAGE("begin example program");

	    double dividendYield = 0.00;
	    double riskFreeRate = 0.06;
	    double volatility = 0.20;
	    double underlying = 36;
	    double strike = 40;
	    long timeSteps = 801;
		long exerciseDate = 36297; 		// (17, May, 1999);
		long settlementDate = 35932; 	// (17, May, 1998);
	    long todaysDate = 35930; 		// (15, May, 1998);
	
		Properties bsProperties = QL_BLACKSCHOLES("my_blackscholes", dividendYield, 
			riskFreeRate, volatility, underlying, todaysDate, settlementDate);
		Properties opProperties = QL_OPTION("my_option", "my_blackscholes", "PUT", 
			strike, timeSteps, exerciseDate, settlementDate);
	
		cout << endl << "High-level interrogation: after QL_OPTION" << endl;
		Properties::const_iterator i;
        for (i = opProperties.begin();
            i != opProperties.end(); i++) {
			ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                QL_ANY2STRING(any) << endl;
        } 

		QL_OPTION_SETENGINE("my_option", "Additive Equiprobabilities", 801);

		cout << endl << "High-level interrogation: after QL_OPTION_SETENGINE" << endl;
        for (i = opProperties.begin();
            i != opProperties.end(); i++) {
			ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                QL_ANY2STRING(any) << endl;
        } 

/*
	low-level interrogation no longer supported in standard #includes -
	will still be possible with additional #includes/links

		cout << endl << "Low-level interrogation: NPV of underlying option object" << endl;
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption> 
			(ObjectHandler::instance().retrieveObject("my_option"));
	    boost::shared_ptr<VanillaOption> const vanillaOption =
			boost::static_pointer_cast<VanillaOption>
			(objectOption->getReference());
		cout << "underlying option NPV() = " 
			<< vanillaOption->NPV() << endl;
*/

		QL_LOGMESSAGE("end example program");
		cout << endl << "bye" << endl;
		return 0;
	} catch (const exception &e) {
		cout << "Error: " << e.what() << endl;
		QL_LOGMESSAGE(e.what());
		return 1;
	} catch (...) {
		cout << "unknown error" << endl;
		QL_LOGMESSAGE("unknown error");
		return 1;
	}
}
