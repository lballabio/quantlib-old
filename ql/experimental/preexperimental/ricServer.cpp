#include <ricServer.hpp>

namespace QuantLib {

	RicServer::RicServer() { }

	string RicServer::strValue(const string& ric, const string& field) {
		map<pair<string,string>,string>::iterator i;
		i=rics_.find(pair<string,string>(ric,field));
		QL_REQUIRE(i!=rics_.end(),"RicServer: Can not find (ric,field)=(" << ric << "," << field << ")");
		return (*i).second;
	}

	double RicServer::dblValue(const string& ric, const string& field) {
		
		if(field=="MID") {
			string bidStr = strValue(ric,"BID");
			string askStr = strValue(ric,"ASK");
			QL_REQUIRE(bidStr!="ERROR" && askStr!="ERROR","RicServer: Ric " << ric << " has error value in bid / ask field(s)");
			double bid = atof(bidStr.c_str());
			double ask = atof(askStr.c_str());
			return (bid+ask) / 2.0;
		}
		else {
			string fStr = strValue(ric,field);
			QL_REQUIRE(fStr!="ERROR","RicServer: (ric,field)=(" << ric << "," << field << ") has error value");
			return atof(fStr.c_str());
		}

	}
	
	int RicServer::load(const string& specFile, const string& contentsFile) {
		
		ifstream in1,in2;
		in1.open(specFile.c_str());
		QL_REQUIRE(in1.is_open(),"RicServer: Can not open spec file " << specFile);
		in2.open(contentsFile.c_str());
		QL_REQUIRE(in2.is_open(),"RicServer: Can not open content file " << contentsFile);

		boost::char_separator<char> sep(";");
		string line1,line2,ric,field,value;
		while(getline(in1,line1)) {
			if(!getline(in2,line2)) QL_FAIL("RicServer: Spec file does not match contents file");
			boost::tokenizer<boost::char_separator<char>> tokens1(line1,sep);
			boost::tokenizer<boost::char_separator<char>> tokens2(line2,sep);
			boost::tokenizer<boost::char_separator<char>>::iterator it1 = tokens1.begin();
			boost::tokenizer<boost::char_separator<char>>::iterator it2 = tokens2.begin();
			bool first=true;
			string ric,field,value;
			do {
				QL_REQUIRE(it1!=tokens1.end(),"RicServer: Spec file line " << line1 << " unexpected end");
				QL_REQUIRE(it2!=tokens2.end(),"RicServer: Content file line " << line2 << " unexpected end");
				if(first) {
					QL_REQUIRE((*it1)==(*it2),"RicServer: Spec file ric " << (*it1) << " does not match content file ric " << (*it2));
					ric=(*it1);	
					first=false;
				}
				else {
					field=*it1;
					value=*it2;
					rics_.insert(pair<pair<string,string>,string>(pair<string,string>(ric,field),value));
				}
				it1++;
				it2++;
			} while (it1!=tokens1.end());
		}

		return 0;

	}


}










