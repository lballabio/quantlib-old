#ifndef ricserver_hpp
#define ricserver_hpp

#include <map>
#include <iostream>
#include <fstream>
#include <ql/quantlib.hpp>
#include <boost/tokenizer.hpp>

using namespace std;

namespace QuantLib {

	class RicServer {
		
		public:

			RicServer();
			double dblValue(const string& ric, const string& field="MID"); // return (BID+ASK)/2 as double
			string strValue(const string& ric, const string& field); // return value as string

			int load(const string& specFile, const string& contentsFile); // load rics from csv file (spec: ric;field1;field2... contents: ric;value1;value2), always return 0 (or throws exception)

		private:

			map<pair<string,string>,string> rics_; // maps (ric,field) to value pairs


	};


}



#endif