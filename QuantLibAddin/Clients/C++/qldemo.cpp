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

#include "Addins/C++/qladdincpp.hpp"
#include <iostream>
using namespace std;
using namespace ObjHandler;
using namespace QuantLib;

int main() {
	try {
		cout << "hi" << endl;

		QL_LOGFILE("quantlib.log");
		QL_LOGMESSAGE("begin example program");

	    Spread dividendYield = 0.00;
	    Rate riskFreeRate = 0.06;
	    Volatility volatility = 0.20;
	    Real underlying = 36;
	    Real strike = 40;
	    Size timeSteps = 801;
		Date exerciseDate(17, May, 1999);
		Date settlementDate(17, May, 1998);
	    Date todaysDate(15, May, 1998);
	
		QL_BLACKSCHOLES("my_blackscholes", dividendYield, riskFreeRate, 
			volatility, underlying, todaysDate, settlementDate);
		QL_OPTION("my_option", "my_blackscholes", "PUT", strike, timeSteps,
			exerciseDate, settlementDate);
	
		cout << endl << "High-level interrogation: after QL_OPTION" << endl;
		// get object from handler and retrieve its properties -
		// (properties also returned by QL_OPTION)
		obj_ptr object = ObjectHandler::instance().retrieveObject("my_option");
        Properties properties = object->getProperties();
		Properties::const_iterator i;
        for (i = properties.begin();
            i != properties.end(); i++) {
			ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                AnyToString(any) << endl;
        } 

		QL_OPTION_SETENGINE("my_option", "Additive Equiprobabilities", 801);

		cout << endl << "High-level interrogation: after QL_OPTION_SETENGINE" << endl;
        for (i = properties.begin();
            i != properties.end(); i++) {
			ObjectProperty property = *i;
            any_ptr any = property();
            cout << "property = " << property.name() << "\tvalue = " <<
                AnyToString(any) << endl;
        } 

		cout << endl << "Low-level interrogation: NPV of underlying option object" << endl;
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption> 
			(ObjectHandler::instance().retrieveObject("my_option"));
	    boost::shared_ptr<VanillaOption> const vanillaOption =
			boost::static_pointer_cast<VanillaOption>
			(objectOption->getReference());
		cout << "underlying option NPV() = " 
			<< vanillaOption->NPV() << endl;

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
