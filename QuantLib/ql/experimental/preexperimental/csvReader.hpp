#ifndef csvreader_hpp
#define csvreader_hpp

#include <map>
#include <iostream>
#include <fstream>
#include <ql/quantlib.hpp>
#include <boost/tokenizer.hpp>

using namespace std;

namespace QuantLib {

	class CsvReader {
		
		public:

			CsvReader();
			double dblValue(const string& key, const string& field); // return column identified by field in line identified by 1st column via key as double
			string strValue(const string& key, const string& field); // return value as string

			int load(const string& dataFile, const string& delimiter=";"); // load data from csv file (1st line contains field names), always return 0 (or throws exception)

			void setValue(const string& key, const string& field, const string& value); // set or modify a value

			void save(const string& dataFile, const string& delimiter=";"); // save data to csv file

			vector<string>& keys() { return keys_; }


		private:
			
			vector<string> fields_; // list of fields
			vector<string> keys_; // list of keys
			map<pair<string,string>,string> data_; // maps (key,field) to value


	};


}



#endif