#include <csvReader.hpp>

namespace QuantLib {

	CsvReader::CsvReader() { }

	string CsvReader::strValue(const string& key, const string& field) {
		map<pair<string,string>,string>::iterator i;
		i=data_.find(pair<string,string>(key,field));
		QL_REQUIRE(i!=data_.end(),"CvsReader: Can not find (key,field)=(" << key << "," << field << ")");
		return (*i).second;
	}

	double CsvReader::dblValue(const string& key, const string& field) {
		string fStr = strValue(key,field);
		return atof(fStr.c_str());
	}

	void CsvReader::setValue(const string &key, const string &field, const string &value) {
		map<pair<string,string>,string>::iterator i;
		i=data_.find(pair<string,string>(key,field));
		QL_REQUIRE(i!=data_.end(),"CsvReader: Can not find (key,field)=(" << key << "," << field << ")");
		(*i).second = value;
	}

	void CsvReader::save(const string& dataFile, const string& delimiter) {
		
		ofstream out;
		out.open(dataFile.c_str());
		QL_REQUIRE(out.is_open(),"CsvReader: Can not open file " << dataFile);

		bool first=true;
		for(vector<string>::iterator i = fields_.begin(); i != fields_.end(); i++) {
			out << (first ? "" : ";") << (*i);
			first=false;
		}
		out << endl;
		
		for(vector<string>::iterator i = keys_.begin(); i != keys_.end(); i++) {
			first=true;
			for(vector<string>::iterator j = fields_.begin(); j != fields_.end(); j++) {
				map<pair<string,string>,string>::iterator k = data_.find(pair<string,string>(*i,*j));
				QL_REQUIRE(k!=data_.end(),"CsvReader: Can not find (key,field)=(" << (*i) << "," << (*j) << ")");
				out << (first ? "" : ";") << (*k).second;
				first = false;
			}
			out << endl;
		}

		out.close();

	}
	
	int CsvReader::load(const string& dataFile, const string& delimiter) {
		
		ifstream in;
		in.open(dataFile.c_str());
		QL_REQUIRE(in.is_open(),"CsvReader: Can not open file " << dataFile);

		boost::char_separator<char> sep(delimiter.c_str());
		string line;

		bool first=true;

		fields_.clear();
		keys_.clear();

		while(getline(in,line)) {
			boost::tokenizer<boost::char_separator<char>> tokens(line,sep);
			boost::tokenizer<boost::char_separator<char>>::iterator it = tokens.begin();
			if(first) {
				while(it!=tokens.end()) {
					fields_.push_back(*it);
					it++;
				}
				first=false;
			}
			else {
				int i=0;
				string key;
				while(it!=tokens.end()) {
					if(i==0) { key=*it; keys_.push_back(key); }
					QL_REQUIRE(fields_.size() > i,"CsvReader: line " << line << " contains more data than number of defined fields (" << fields_.size() << ")");
					data_.insert(pair<pair<string,string>,string>(pair<string,string>(key,fields_[i]),*it));
					i++;
					it++;
				}
			}
		}

		return 0;

	}


}










