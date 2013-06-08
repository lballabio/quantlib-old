#include <swaptionSmileMxExport.hpp>

namespace QuantLib {

	bool swaptionSmileMurexExport(boost::shared_ptr<SwaptionVolatilityStructure> volCube,
				string outPath, string nickname, string date, string swap, 
				vector<string>& optionNames, vector<string>& swapNames,
				vector<Period>& optionTenors, vector<Period>& swapTenors,
				vector<double> strikeSpreads, string referenceAtmInPath) {

			QL_REQUIRE(outPath.size()>0,"Outpath not valid");
			QL_REQUIRE(nickname.size()>0,"Nickname not valid");
			QL_REQUIRE(date.size()==8 || date == "TODAY","Date (" << date << ") must be TODAY or YYYYMMDD");
			QL_REQUIRE(swap.size()>0,"Swap not valid");
			QL_REQUIRE(strikeSpreads.size()>0,"Strike spreads vector must be non empty");
			QL_REQUIRE(optionNames.size()==optionTenors.size(),"Option names vector (" << optionNames.size() << ") must have same length as option tenor vector (" << optionTenors.size());
			QL_REQUIRE(swapNames.size()==swapTenors.size(),"Swap names vector (" << swapNames.size() << ") must have same length as swap tenor vector (" << swapTenors.size());

			bool useExternalAtm=false;

			// input xml

			map<pair<string,string>,double> xmlAtmVols;
			map<pair<string,string>,double>::iterator xmlAtmVolsIt;

			if(referenceAtmInPath.size()>0) {
				useExternalAtm=true;
				// read file and copy to memory
				ifstream is;
				is.open(referenceAtmInPath.c_str(),ios::binary);
				if(!is.good()) QL_FAIL("input xml file (" << referenceAtmInPath << ") not found.");
				is.seekg(0,ios::end);
				int length=is.tellg();
				is.seekg(0,ios::beg);
				char* buffer = new char[length+1];
				is.read(buffer,length);
				is.close();
				buffer[length]=0; // terminate string with 0
				// parse xml
				using namespace boost::property_tree::detail::rapidxml;
				xml_document<> doc;
				xml_node<> *node,*node2,*node3;
				xml_attribute<> *attr;
				doc.parse<0>(buffer);
				node = doc.first_node();
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				// look up nickname
				attr = node->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
				while(strcmp(attr->value(),nickname.c_str())) {
					node=node->next_sibling();
					if(!node) QL_FAIL("Nickname " << nickname << " not found.");
					attr = node->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
				}
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				// look up swap (e.g. EUR SW VOL)
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				attr = node->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
				while(strcmp(attr->value(),swap.c_str())) {
					node=node->next_sibling();
					if(!node) QL_FAIL("SwapTemplate " << swap << " not found.");
					attr = node->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
				}
				// extract vols
				node=node->first_node(); if(!node) QL_FAIL("Unexpected xml structure");
				do {
					attr = node->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
					char* tenor = attr->value();
					node2 = node->first_node(); if(!node2) QL_FAIL("Unexpected xml structure");
					do {
						attr = node2->first_attribute("xc:value"); if(!attr) QL_FAIL("Unexpected xml structure");
						char* maturity = attr->value();
						node3 = node2->first_node("mp:bid"); if(!node3) QL_FAIL("Unexpected xml structure");
						char* val = node3->value();
						xmlAtmVols.insert(pair<pair<string,string>,double>(pair<string,string>(string(maturity),string(tenor)),atof(val)));;
						node2=node2->next_sibling();
					} while(node2);
					node=node->next_sibling(); 
				} while(node);
			}

			// output xml 

			ofstream out;
			out.open(outPath.c_str());

			out.setf(ios::fixed,ios::floatfield);
			out.precision(4);

			out << "<xc:XmlCache xmlns:xc=\"XmlCache\" xc:action=\"Update\">";
			out << "<xc:XmlCacheArea xc:value=\"MarketParameters\">";
			out << "<mp:nickName xmlns:mp=\"mx.MarketParameters\" xc:value=\"" << nickname << "\">";
			out << "<mp:date xc:value=\"" << date << "\">";
			out << "<rt:rate xmlns:rt=\"mx.MarketParameters.Rates\">";
			out << "<rtss:swaptionSmile xmlns:rtss=\"mx.MarketParameters.Rates.SwaptionSmile\">";
			out << "<rtss:swapTemplate xc:value=\"" << swap << "\">";

			vector<double> volOut(strikeSpreads.size());

			for(int i=0;i<swapTenors.size();i++) {
				out << "<rtss:swapTenor xc:value=\"" << swapNames[i] << "\">";
				for(int j=0;j<optionTenors.size();j++) {
					out << "<rtss:maturity xc:value=\"" << optionNames[j] << "\">";
					Date fixing = volCube->optionDateFromTenor(optionTenors[j]);
					boost::shared_ptr<SmileSection> sec=volCube->smileSection(fixing,swapTenors[i]);
					double atm=sec->atmLevel();
					double atmVol=sec->volatility(atm);
					// collect vols
					double volBefore=0.0;
					for(int k=0;k<strikeSpreads.size();k++) {
						double s=atm+strikeSpreads[k];
						double vol=0.0;
						if(s>0.0) {
							vol=sec->volatility(s);
							if(vol==0.0) vol=volBefore; // flat right extrapolation in case of error (vol==0.0)
							volBefore=vol;
							if(useExternalAtm && vol>0.0) {
								double volSpread=vol-atmVol;
								xmlAtmVolsIt=xmlAtmVols.find(pair<string,string>(optionNames[j],swapNames[i]));
								if(xmlAtmVolsIt==xmlAtmVols.end()) QL_FAIL("Can not find (" << optionNames[j] << "/" << swapNames[i] << ") in xml file...");
								vol=volSpread+(*xmlAtmVolsIt).second/100.0;
								if(vol<0.0) vol=0.0; // make sure that vol does not get negative
							}
						}
						volOut[k]=vol;
					}
					// replace missing vols by flat extrapolated values
					for(int k=strikeSpreads.size()-1;k>=0;k--) {
						if(volOut[k]<=0.0) {
							if(k==strikeSpreads.size()-1) volOut[k]=0.0;
							else volOut[k]=volOut[k+1];
						}
					}
					// output vols
					for(int k=0;k<strikeSpreads.size();k++) {
						out << "<rtss:ordinate xc:value=\"" << strikeSpreads[k]*100.0 << "\" xc:type=\"Fields\">";
						out << "<mp:mid>" << volOut[k] *100.0 << "</mp:mid>";
						out << "</rtss:ordinate>";
					}
					//  legacy murex ascii format:
					//  out << "|" << swapTenors[i] << "|EUR|Swaption|" << optionTenors[j] << "|" << strikeSpreads[k] << "|SMRS|"
					//	<< volSpread << "|" << volSpread << "|" << endl;
					out << "</rtss:maturity>";
				}
				out << "</rtss:swapTenor>";
			}

			out << "</rtss:swapTemplate>";
			out << "</rtss:swaptionSmile>";
			out << "</rt:rate>";
			out << "</mp:date>";
			out << "</mp:nickName>";
			out << "</xc:XmlCacheArea>";
			out << "</xc:XmlCache>";

			out.close();

			return true;

	}

	
}
