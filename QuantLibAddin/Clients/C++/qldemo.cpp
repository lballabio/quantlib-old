#ifdef WIN32
#pragma warning(disable: 4786)
#pragma warning(disable: 4503)
#endif

#include "Addins/C++/qladdincpp.hpp"
#include <iostream>
using namespace std;

extern ObjectHandler objectHandler;

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
	
		cout << "High-level interrogation: after QL_OPTION" << endl;
		obj_ptr object = objectHandler.retrieveObject("my_option");
		vector < string > fieldNames = object->getFieldNames();
		vector < string >::iterator i;
		for (i = fieldNames.begin(); 
			i != fieldNames.end(); i++) {
			string fieldName = *i;
			any_ptr any = object->getValue(fieldName);
			cout << "field = " << fieldName << "\tvalue = " << 
				AnyToString(any) << endl;
		}

		QL_OPTION_SETENGINE("my_option", "Additive Equiprobabilities", 801);

		cout << "High-level interrogation: after QL_OPTION_SETENGINE" << endl;
		for (i = fieldNames.begin(); i != fieldNames.end(); i++) {
			string fieldName = *i;
			any_ptr any = object->getValue(fieldName);
			cout << "field = " << fieldName << "\tvalue = " << 
				AnyToString(any) << endl;
		}

		cout << "Low-level interrogation: NPV of underlying option object" << endl;
		boost::shared_ptr<ObjectOption> objectOption = 
			boost::dynamic_pointer_cast<ObjectOption> 
			(objectHandler.retrieveObject("my_option"));
	    const boost::shared_ptr<VanillaOption> vanillaOption =
			boost::static_pointer_cast<VanillaOption>
			(objectOption->getReference());
		cout << "underlying option NPV() = " 
			<< vanillaOption->NPV() << endl;

		QL_LOGMESSAGE("end example program");
		cout << "bye" << endl;
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
