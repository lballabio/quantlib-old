#include <sabrRbsSmile.hpp>

namespace QuantLib {

	SabrRbsSmile::SabrRbsSmile(boost::shared_ptr<SwaptionVolatilityCube>& volCube,
				const vector<Period>& optionTenors, const vector<Period>& swapTenors,
				const Matrix& alpha, const Matrix& nu, const Matrix& rho,
				double leftBound, double rightBound, 
				const vector<Period>& optionPillars, const vector<Period>& underlyingPillars,
				const Matrix& pMu, const Matrix& pNu, const Matrix& beta,
				vector<double>& strikeSpreads, bool vegaWeighted, double acceptRmse, double rejectRmse,
				int haltonIterations, 
				const boost::shared_ptr<EndCriteria>& endCriteria,
                const boost::shared_ptr<OptimizationMethod>& optMethod,
				double aBLACKACCURACY,
				double hDiff) :
					volCube_(volCube), optionTenors_(optionTenors), swapTenors_(swapTenors),
					alpha_(alpha), nu_(nu), rho_(rho),
					leftBound_(leftBound), rightBound_(rightBound),
					optPillars_(optionPillars), undPillars_(underlyingPillars),
					pMu_(pMu), pNu_(pNu), beta_(beta),
					strikeSpreads_(strikeSpreads), vegaWeighted_(vegaWeighted),
					acceptRmse_(acceptRmse), rejectRmse_(rejectRmse),
					haltonIterations_(haltonIterations), 
					endCriteria_(endCriteria), optMethod_(optMethod),
					BLACKACCURACY(aBLACKACCURACY),
					h_(hDiff)
			{
				// check input
				QL_REQUIRE(optionTenors.size()==alpha.columns(),"Alpha matrix has " << alpha.columns() << " columns, but there are " << optionTenors.size() << " option tenors.");
				QL_REQUIRE(optionTenors.size()==nu.columns(),"Nu matrix has " << nu.columns() << " columns, but there are " << optionTenors.size() << " option tenors.");
				QL_REQUIRE(optionTenors.size()==rho.columns(),"Rho matrix has " << rho.columns() << " columns, but there are " << optionTenors.size() << " option tenors.");
				QL_REQUIRE(swapTenors.size()==alpha.rows(),"Alpha matrix has " << alpha.rows() << " rows, but there are " << swapTenors.size() << " swap tenors.");
				QL_REQUIRE(swapTenors.size()==nu.rows(),"Nu matrix has " << nu.rows() << " rows, but there are " << swapTenors.size() << " swap tenors.");
				QL_REQUIRE(swapTenors.size()==rho.rows(),"Rho matrix has " << rho.rows() << " rows, but there are " << swapTenors.size() << " swap tenors.");
				QL_REQUIRE(optPillars_.size()==pMu.columns(),"pMu matrix has " << pMu.columns() << " columns, but there are " << optPillars_.size() << " option pillars in the cms calibration termstructure.");
				QL_REQUIRE(optPillars_.size()==pNu.columns(),"pNu matrix has " << pNu.columns() << " columns, but there are " << optPillars_.size() << " option pillars in the cms calibration termstructure.");
				QL_REQUIRE(optPillars_.size()==beta.columns(),"Beta matrix has " << beta.columns() << " columns, but there are " << optPillars_.size() << " option pillars in the cms calibration termstructure.");
				QL_REQUIRE(undPillars_.size()==pMu.rows(),"pMu matrix has " << pMu.rows() << " rows, but there are " << undPillars_.size() << " underlying pillars in the cms calibration termstructure.");
				QL_REQUIRE(undPillars_.size()==pNu.rows(),"pNu matrix has " << pNu.rows() << " rows, but there are " << undPillars_.size() << " underlying pillars in the cms calibration termstructure");
				QL_REQUIRE(undPillars_.size()==beta.rows(),"Beta matrix has " << beta.rows() << " rows, but there are " << undPillars_.size() << " underlying pillars in the cms calibration termstructure");
				
				// extract conventions from cube
				referenceDate_=volCube_->referenceDate();
				calendar_=volCube_->calendar();
				bdc_=volCube_->businessDayConvention();
				dc_=volCube_->dayCounter();

				// set up fixing dates from option tenors
				for(int i=0;i<optionTenors.size();i++) {
					//Date fixing=calendar_.advance(referenceDate_,optionTenors[i],bdc_);
					Date fixing=volCube_->optionDateFromTenor(optionTenors[i]);
					fixings_.push_back(fixing);
				}

				// set up interpolators for smile parameters
				for(int i=0;i<optPillars_.size();i++) {
					Date fixing=volCube_->optionDateFromTenor(optionPillars[i]);
					optTimes_.push_back(time(fixing));
				}
				for(int i=0;i<undPillars_.size();i++) {
					undTimes_.push_back(time(calendar_.advance(referenceDate_,undPillars_[i],bdc_)));
				}
				muInterpol_=FlatExtrapolator2D(
					boost::shared_ptr<Interpolation2D>(new BilinearInterpolation(optTimes_.begin(),optTimes_.end(),undTimes_.begin(),undTimes_.end(),pMu_)));
				nuInterpol_=FlatExtrapolator2D(
					boost::shared_ptr<Interpolation2D>(new BilinearInterpolation(optTimes_.begin(),optTimes_.end(),undTimes_.begin(),undTimes_.end(),pNu_)));
				betaInterpol_=FlatExtrapolator2D(
					boost::shared_ptr<Interpolation2D>(new BilinearInterpolation(optTimes_.begin(),optTimes_.end(),undTimes_.begin(),undTimes_.end(),beta_)));
				muInterpol_.update();
				nuInterpol_.update();
				betaInterpol_.update();

				// set up internal matrices
				sabrRmse_=Matrix(alpha.rows(),alpha.columns(),-1.0);

				// set up sabr models
				setupSabrModels();
				
				// set up rbs smiles
				setupRbsSmiles();

			}

	bool SabrRbsSmile::setPillarMu(int optPillar, int undPillar, double mu) {
		pMu_[undPillar][optPillar]=mu;
		muInterpol_.update();
		return true;
	}

	bool SabrRbsSmile::setPillarNu(int optPillar, int undPillar, double nu) {
		pNu_[undPillar][optPillar]=nu;
		nuInterpol_.update();
		return true;
	}

	bool SabrRbsSmile::setPillarBeta(int optPillar, int undPillar, double beta) {
		beta_[undPillar][optPillar]=beta;
		betaInterpol_.update();
		return true;
	}

	bool SabrRbsSmile::writeMurexFile(string path, string nickname, string date, string swap, vector<string>& optionNames, vector<string>& swapNames, 
				vector<Period>& optionTenors,vector<Period>& swapTenors,
				vector<double> strikeSpreads,string inPath) {

			bool useExternalAtm=false;

			// input xml

			map<pair<string,string>,double> xmlAtmVols;
			map<pair<string,string>,double>::iterator xmlAtmVolsIt;

			if(inPath.size()>0) {
				useExternalAtm=true;
				// read file and copy to memory
				ifstream is;
				is.open(inPath.c_str(),ios::binary);
				if(!is.good()) QL_FAIL("input xml file (" << inPath << ") not found.");
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
			out.open(path.c_str());

			out.setf(ios::fixed,ios::floatfield);
			out.precision(4);

			out << "<xc:XmlCache xmlns:xc=\"XmlCache\" xc:action=\"Update\">";
			out << "<xc:XmlCacheArea xc:value=\"MarketParameters\">";
			out << "<mp:nickName xmlns:mp=\"mx.MarketParameters\" xc:value=\"" << nickname << "\">";
			out << "<mp:date xc:value=\"" << date << "\">";
			out << "<rt:rate xmlns:rt=\"mx.MarketParameters.Rates\">";
			out << "<rtss:swaptionSmile xmlns:rtss=\"mx.MarketParameters.Rates.SwaptionSmile\">";
			out << "<rtss:swapTemplate xc:value=\"" << swap << "\">";

			for(int i=0;i<swapTenors.size();i++) {
				out << "<rtss:swapTenor xc:value=\"" << swapNames[i] << "\">";
				for(int j=0;j<optionTenors.size();j++) {
					out << "<rtss:maturity xc:value=\"" << optionNames[j] << "\">";
					Date fixing = volCube_->optionDateFromTenor(optionTenors[j]);
					double atm=volCube_->atmStrike(fixing,swapTenors[i]);
					try {
						setFastVolatility(fixing,swapTenors[i]);
						double atmVol=getFastVolatility(atm);
						for(int k=0;k<strikeSpreads.size();k++) {
							double s=atm+strikeSpreads[k];
							if(s<MINSTRIKE) s=MINSTRIKE;
							double vol=getFastVolatility(s);
							if(useExternalAtm) {
								double volSpread=vol-atmVol;
								xmlAtmVolsIt=xmlAtmVols.find(pair<string,string>(optionNames[j],swapNames[i]));
								if(xmlAtmVolsIt==xmlAtmVols.end()) QL_FAIL("Can not find (" << optionNames[j] << "/" << swapNames[i] << ") in xml file...");
								vol=volSpread+(*xmlAtmVolsIt).second/100.0; // external atm vol here!
							}
							out << "<rtss:ordinate xc:value=\"" << strikeSpreads[k]*100.0 << "\" xc:type=\"Fields\">";
							out << "<mp:mid>" << vol *100.0 << "</mp:mid>";
							out << "</rtss:ordinate>";
							//out << "|" << swapTenors[i] << "|EUR|Swaption|" << optionTenors[j] << "|" << strikeSpreads[k] << "|SMRS|"
							//	<< volSpread << "|" << volSpread << "|" << endl;
						}
					} catch(QuantLib::Error e) { // fall back underlying cube
						double atmVol=volCube_->volatility(fixing,swapTenors[i],atm,true);
						for(int k=0;k<strikeSpreads.size();k++) {
							double vol=volCube_->volatility(fixing,swapTenors[i],atm+strikeSpreads[k],true);
							double volSpread=vol-atmVol;
							if(useExternalAtm) {
								double volSpread=vol-atmVol;
								xmlAtmVolsIt=xmlAtmVols.find(pair<string,string>(optionNames[j],swapNames[i]));
								if(xmlAtmVolsIt==xmlAtmVols.end()) QL_FAIL("Can not find (" << optionNames[j] << "/" << swapNames[i] << ") in xml file...");
								vol=volSpread+(*xmlAtmVolsIt).second/100.0; // external atm vol here!
							}
							out << "<rtss:ordinate xc:value=\"" << strikeSpreads[k]*100.0 << "\" xc:type=\"Fields\">";
							out << "<mp:mid>" << vol *100.0 << "</mp:mid>";
							out << "</rtss:ordinate>";
						}
					}
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

	double SabrRbsSmile::time(const Date& date) {
		return dc_.yearFraction(referenceDate_,date,referenceDate_,date);
	}

	double SabrRbsSmile::volatility(const Period& option, const Period& tenor, const double& strike0, bool spread, bool market) {
		Date fixing=volCube_->optionDateFromTenor(option);
		return volatility(fixing,tenor,strike0,spread,market);
	}

	double SabrRbsSmile::volatility(const Date& fixing, const Period& tenor, const double& strike0, bool spread, bool market) {
		
		double atm=volCube_->atmStrike(fixing,tenor);
		
		double strike,strikeSpread;
		if(spread) {
			strike=strike0+atm;
			strikeSpread=strike0;
		}
		else {
			strike=strike0;
			strikeSpread=strike0-atm;
		}
		if(strike<MINSTRIKE) strike=MINSTRIKE;

		if(market) return volCube_->volatility(fixing,tenor,strike,true);

		double maturity = time(fixing);
		double undMaturity = time(calendar_.advance(fixing,tenor,bdc_))-maturity;

		// interpolation / extrapolation necessary?
		// option tenor interpolation
		int ind=0;
		while(ind < fixings_.size() && fixings_[ind] != fixing) {
			ind++;
		}
		if(ind==fixings_.size()) {
			ind=0;
			while(ind < fixings_.size() && maturity > time(fixings_[ind]))  {
				ind++;
			}
			// left flat extrapolation
			if(ind==0) {
				return volatility(fixings_[0],tenor,strikeSpread,true,false);
			}
			// right flat extrapolation
			if(ind==fixings_.size()) {
				return volatility(fixings_[ind-1],tenor,strikeSpread,true,false);
			}
			// interpolation
			double vol1=volatility(fixings_[ind-1],tenor,strikeSpread,true,false);
			double vol2=volatility(fixings_[ind],tenor,strikeSpread,true,false);
			double mat1=time(fixings_[ind-1]);
			double mat2=time(fixings_[ind]);
			return vol1+(vol2-vol1)/(mat2-mat1)*(maturity-mat1);
		}
		// swap tenor interpolation
		ind =0;
		while(ind < swapTenors_.size() && swapTenors_[ind] != tenor) {
			ind++;
		}
		if(ind == swapTenors_.size()) {
			ind=0;
			while(ind < swapTenors_.size() && undMaturity > time(calendar_.advance(fixing,swapTenors_[ind],bdc_))-maturity)  {
				ind++;
			}
			// left flat extrapolation
			if(ind==0) {
				return volatility(fixing,swapTenors_[0],strikeSpread,true,false);
			}
			// right flat extrapolation
			if(ind==swapTenors_.size()) {
				return volatility(fixing,swapTenors_[ind-1],strikeSpread,true,false);
			}
			// interpolation
			double vol1=volatility(fixing,swapTenors_[ind-1],strikeSpread,true,false);
			double vol2=volatility(fixing,swapTenors_[ind],strikeSpread,true,false);
			double mat1 = time(calendar_.advance(fixing,swapTenors_[ind-1],bdc_))-maturity;
			double mat2 = time(calendar_.advance(fixing,swapTenors_[ind],bdc_))-maturity;
			return vol1+(vol2-vol1)/(mat2-mat1)*(undMaturity-mat1);
		}

		// no interpolation necessary:
		
		// sabr oder rbs region?
		if(strike >= atm+leftBound_ && strike <= atm+rightBound_) {
			sabrValidIter_ = sabrValid_.find(pair<Date,Period>(fixing,tenor));
			if(sabrValidIter_ == sabrValid_.end()) QL_FAIL("no valid flag found for sabr model on pillar (should not happen)");
			if((*sabrValidIter_).second) {
				boost::shared_ptr<SabrModel> sabr = sabrModel(fixing,tenor);
				return sabr->impliedVola(atm,strike,maturity);
			}
			else { // fall back in case sabr model is not valid
				return volCube_->volatility(fixing,tenor,strike,true);
			}
		}
		else {
			// rbs extrapolation:
			double v0=volCube_->volatility(fixing,tenor,strike,true); // inital guess for inverse black calculation
			boost::shared_ptr<RbsSmile> rbs = rbsSmile(fixing,tenor);
			double p = rbs->price(strike);
			double vol;
			Option::Type type = strike > atm ? Option::Call : Option::Put;
			try {
				vol=blackFormulaImpliedStdDev(type,strike,atm,p,1.0,0.0,v0,BLACKACCURACY,100)/sqrt(maturity);
			} catch(QuantLib::Error er) {
				vol = v0; //volCube_->volatility(fixing,tenor,strike,true); // fall back
			}
			return vol;
		}
	}

	bool SabrRbsSmile::setFastVolatility(const Date& fixing, const Period& tenor) {
		fastValid_=true;
		fastFixing_=fixing;
		fastTenor_=tenor;
		Date optionDate1, optionDate2;
		Period tenor1, tenor2;
		// compute interpolation parameters
		double maturity = time(fixing);
		double undMaturity = time(calendar_.advance(fixing,tenor,bdc_))-maturity;
		fastOptionTime_=maturity;
		fastSwapTime_=undMaturity;
		// option tenor interpolation
		int ind=0;
		while(ind < fixings_.size() && fixings_[ind] != fixing) {
			ind++;
		}
		if(ind==fixings_.size()) {
			ind=0;
			while(ind < fixings_.size() && maturity > time(fixings_[ind]))  {
				ind++;
			}
			// left flat extrapolation
			if(ind==0) {
				optionDate1=fixings_[0];
				optionDate2=fixings_[0];
				fastOptEq_=true;
			}
			else {
				// right flat extrapolation
				if(ind==fixings_.size()) {
					optionDate1=fixings_[ind-1];
					optionDate2=fixings_[ind-1];
					fastOptEq_=true;
				}
				else {
					// interpolation
					optionDate1=fixings_[ind-1];
					optionDate2=fixings_[ind];
					fastOptEq_=false;
				}
			}
		}
		else {
			optionDate1=fixing;
			optionDate2=fixing;
			fastOptEq_=true;
		}
		// swap tenor interpolation
		ind =0;
		while(ind < swapTenors_.size() && swapTenors_[ind] != tenor) {
			ind++;
		}
		if(ind == swapTenors_.size()) {
			ind=0;
			while(ind < swapTenors_.size() && undMaturity > time(calendar_.advance(fixing,swapTenors_[ind],bdc_))-maturity)  {
				ind++;
			}
			// left flat extrapolation
			if(ind==0) {
				tenor1=swapTenors_[0];
				tenor2=swapTenors_[0];
				fastSwapEq_=true;
			}
			else {
				// right flat extrapolation
				if(ind==swapTenors_.size()) {
					tenor1=swapTenors_[ind-1];
					tenor2=swapTenors_[ind-1];
					fastSwapEq_=true;
				}
				else {
					// interpolation
					tenor1=swapTenors_[ind-1];
					tenor2=swapTenors_[ind];
					fastSwapEq_=false;
				}
			}
		}
		else {
			tenor1=tenor;
			tenor2=tenor;
			fastSwapEq_=true;
		}
		fastOptionTime1_=time(optionDate1);
		fastOptionTime2_=time(optionDate2);
		fastSwapTime1_=time(calendar_.advance(fixing,tenor1,bdc_))-maturity;
		fastSwapTime2_=time(calendar_.advance(fixing,tenor2,bdc_))-maturity;
		fastAtm11_=volCube_->atmStrike(optionDate1,tenor1);
		fastAtm21_=volCube_->atmStrike(optionDate2,tenor1);
		fastAtm12_=volCube_->atmStrike(optionDate1,tenor2);
		fastAtm22_=volCube_->atmStrike(optionDate2,tenor2);
		fastAtm_=volCube_->atmStrike(fixing,tenor);
		try {
			fastSabrModel11_=sabrModel(optionDate1,tenor1);
			fastSabrModel21_=sabrModel(optionDate2,tenor1);
			fastSabrModel12_=sabrModel(optionDate1,tenor2);
			fastSabrModel22_=sabrModel(optionDate2,tenor2);
			fastRbsModel11_=rbsSmile(optionDate1,tenor1);
			fastRbsModel21_=rbsSmile(optionDate2,tenor1);
			fastRbsModel12_=rbsSmile(optionDate1,tenor2);
			fastRbsModel22_=rbsSmile(optionDate2,tenor2);
		} catch(QuantLib::Error e) {
			fastValid_=false;
		}	
		return fastValid_;
	}

	double SabrRbsSmile::getFastVolatility(const double& strike) {

		if(!fastValid_) return volCube_->volatility(fastFixing_,fastTenor_,strike,true);

		double vol11,vol21,vol12,vol22;
		double spread=strike-fastAtm_;
		// 11
		if(spread >= leftBound_ && spread <= rightBound_) {
				vol11=fastSabrModel11_->impliedVola(fastAtm11_,fastAtm11_+spread,fastOptionTime1_);
		}
		else {
			// rbs extrapolation:
			double p11 = fastRbsModel11_->price(fastAtm11_+spread);
			Option::Type type = spread > 0.0 ? Option::Call : Option::Put;
			try {vol11=blackFormulaImpliedStdDev(type,fastAtm11_+spread,fastAtm11_,p11,1.0,0.0,0.3,BLACKACCURACY,100)/sqrt(fastOptionTime1_);
			} catch(QuantLib::Error er) { vol11 = volCube_->volatility(fastFixing_,fastTenor_,fastAtm11_+spread,true);} // fall back 
		}
		// 21
		if(fastOptEq_) { vol21=vol11; }
		else {
			if(spread >= leftBound_ && spread <= rightBound_) {
					vol21=fastSabrModel21_->impliedVola(fastAtm21_,fastAtm21_+spread,fastOptionTime2_);
			}
			else {
				// rbs extrapolation:
				double p21 = fastRbsModel21_->price(fastAtm21_+spread);
				Option::Type type = spread > 0.0 ? Option::Call : Option::Put;
				try {vol21=blackFormulaImpliedStdDev(type,fastAtm21_+spread,fastAtm21_,p21,1.0,0.0,0.3,BLACKACCURACY,100)/sqrt(fastOptionTime2_);
				} catch(QuantLib::Error er) { vol21 = volCube_->volatility(fastFixing_,fastTenor_,fastAtm21_+spread,true);} // fall back 
			}
		}
		// 12
		if(fastSwapEq_) { vol12=vol11; }
		else {
			if(spread >= leftBound_ && spread <= rightBound_) {
					vol12=fastSabrModel12_->impliedVola(fastAtm12_,fastAtm12_+spread,fastOptionTime1_);
			}
			else {
				// rbs extrapolation:
				double p12 = fastRbsModel12_->price(fastAtm12_+spread);
				Option::Type type = spread > 0.0 > fastAtm12_ ? Option::Call : Option::Put;
				try {vol12=blackFormulaImpliedStdDev(type,fastAtm12_+spread,fastAtm12_,p12,1.0,0.0,0.3,BLACKACCURACY,100)/sqrt(fastOptionTime1_);
				} catch(QuantLib::Error er) { vol12 = volCube_->volatility(fastFixing_,fastTenor_,fastAtm12_+spread,true);} // fall back 
			}
		}
		// 22
		if(fastOptEq_) { vol22=vol12; }
		else { if(fastSwapEq_) { vol22=vol21; }
			else {
			if(spread >= leftBound_ && spread <= rightBound_) {
						vol22=fastSabrModel22_->impliedVola(fastAtm22_,fastAtm22_+spread,fastOptionTime2_);
				}
				else {
					// rbs extrapolation:
					double p22 = fastRbsModel22_->price(fastAtm22_+spread);
					Option::Type type = spread > 0.0 ? Option::Call : Option::Put;
					try {vol22=blackFormulaImpliedStdDev(type,fastAtm22_+spread,fastAtm22_,p22,1.0,0.0,0.3,BLACKACCURACY,100)/sqrt(fastOptionTime2_);
					} catch(QuantLib::Error er) { vol22 = volCube_->volatility(fastFixing_,fastTenor_,fastAtm22_+spread,true);} // fall back 
				}
			}
		}
		double a = fastOptEq_ ? 0.0 : (fastOptionTime_-fastOptionTime1_)/(fastOptionTime2_-fastOptionTime1_);
		double b = fastSwapEq_ ? 0.0 : (fastSwapTime_-fastSwapTime1_)/(fastSwapTime2_-fastSwapTime1_);
		return vol11*(1.0-a)*(1.0-b)+vol21*(1.0-b)*a+vol12*b*(1.0-a)+vol22*a*b;
	}

	double SabrRbsSmile::optionPrice(const Period& option, const Period& tenor, const double& strike0, const Option::Type type, bool spread, bool market, bool forceSabr) {
		//Date fixing=calendar_.advance(referenceDate_,option,bdc_);
		Date fixing=volCube_->optionDateFromTenor(option);
		return optionPrice(fixing,tenor,strike0,type,spread,market,forceSabr);
	}

	double SabrRbsSmile::optionPrice(const Date& fixing, const Period& tenor, const double& strike0, const Option::Type type, bool spread, bool market, bool forceSabr) {

		double atm=volCube_->atmStrike(fixing,tenor);
		double strike = strike0;
		if(spread) strike=strike0+atm;
		if(strike<MINSTRIKE) strike=MINSTRIKE;

		double maturity = time(fixing);
		double undMaturity = time(calendar_.advance(fixing,tenor,bdc_))-maturity;

		if(market) {
			double vol=volCube_->volatility(fixing,tenor,strike,true);
			return blackFormula(type,strike,atm,vol*sqrt(maturity));			
		}

		// sabr oder rbs region?
		if(forceSabr || (strike >= atm+leftBound_ && strike <= atm+rightBound_)) {
			sabrValidIter_ = sabrValid_.find(pair<Date,Period>(fixing,tenor));
			if(sabrValidIter_ == sabrValid_.end()) QL_FAIL("no valid flag found for sabr model on pillar (should not happen)");
			if((*sabrValidIter_).second) {
				boost::shared_ptr<SabrModel> sabr = sabrModel(fixing,tenor);
				return sabr->optionPrice(atm,strike,maturity,type);
			}
			else { // fall back if sabr is not valid
				double vol=volCube_->volatility(fixing,tenor,strike,true);
				if(vol<0.0) vol=0.0001;
				return blackFormula(type,strike,atm,vol*sqrt(maturity));
			}
		}
		else {
			boost::shared_ptr<RbsSmile> rbs = rbsSmile(fixing,tenor);
			double p = rbs->price(strike);
			Option::Type type0 = strike > atm ? Option::Call : Option::Put;
			if(type0==type) return p;
			if(type==Option::Call) return p + atm - strike;
			else return p - atm + strike;
		}

	}

	vector<double> SabrRbsSmile::sabrParameters(const Date& fixing, const Period& tenor) {
		boost::shared_ptr<SabrModel> sabr = sabrModel(fixing,tenor);
		return sabr->modelParameters();		
	}

	vector<double> SabrRbsSmile::sabrParameters(const Period& option, const Period& swap) {
		boost::shared_ptr<SabrModel> sabr = sabrModel(option,swap);
		return sabr->modelParameters();		
	}

	vector<double> SabrRbsSmile::rbsParameters(const Date& fixing, const Period& tenor) {
		boost::shared_ptr<RbsSmile> rbs = rbsSmile(fixing,tenor);
		return rbs->modelParameters();		
	}

	vector<double> SabrRbsSmile::rbsParameters(const Period& option, const Period& swap) {
		boost::shared_ptr<RbsSmile> rbs = rbsSmile(option,swap);
		return rbs->modelParameters();		
	}


	boost::shared_ptr<SabrModel> SabrRbsSmile::sabrModel(const Period& option, const Period& swap) {
		//Date fixing=calendar_.advance(referenceDate_,option,bdc_);
		Date fixing=volCube_->optionDateFromTenor(option);
		return sabrModel(fixing,swap);
	}



	boost::shared_ptr<SabrModel> SabrRbsSmile::sabrModel(const Date& fixing, const Period& swap) {
		boost::shared_ptr<SabrModel> sabr;
		sabrValidIter_ = sabrValid_.find(pair<Date,Period>(fixing,swap));
		if(sabrValidIter_ == sabrValid_.end()) {
			QL_FAIL("option / swap pair is not a sabr model pillar in the matrix"); 
		}
		else {
			if(!(*sabrValidIter_).second)
				QL_FAIL("option / swap pair has no valid sabr model");
		}
		sabrModelsIter_ = sabrModels_.find(pair<Date,Period>(fixing,swap));
		if(sabrModelsIter_ == sabrModels_.end()) {
			QL_FAIL("sabr model not found (although marked valid, this should never happen)"); 
		}
		else {
			sabr=(*sabrModelsIter_).second;
		}
		return sabr;
	}

	boost::shared_ptr<RbsSmile> SabrRbsSmile::rbsSmile(const Period& option, const Period& swap) {
		//Date fixing=calendar_.advance(referenceDate_,option,bdc_);
		Date fixing=volCube_->optionDateFromTenor(option);
		return rbsSmile(fixing,swap);
	}

	boost::shared_ptr<RbsSmile> SabrRbsSmile::rbsSmile(const Date& fixing, const Period& swap) {
		boost::shared_ptr<RbsSmile> rbs;
		rbsSmilesIter_ = rbsSmiles_.find(pair<Date,Period>(fixing,swap));
		if(rbsSmilesIter_ == rbsSmiles_.end()) {
			QL_FAIL("rbs smile not found"); 
		}
		else {
			rbs=(*rbsSmilesIter_).second;
		}
		return rbs;
	}

	Matrix SabrRbsSmile::sabrBeta() {
		
		Matrix res(swapTenors_.size(), optionTenors_.size(),-1.0);

		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				//Date fixing=calendar_.advance(referenceDate_,optionTenors_[j],bdc_);
				Date fixing=volCube_->optionDateFromTenor(optionTenors_[j]);
				sabrValidIter_ = sabrValid_.find(pair<Date,Period>(fixing,swapTenors_[i]));
				if(sabrValidIter_ == sabrValid_.end()) QL_FAIL("no valid flag found for sabr model on pillar (should not happen)");
				if((*sabrValidIter_).second) {
					boost::shared_ptr<SabrModel> sabr = sabrModel(optionTenors_[j],swapTenors_[i]);
					res[i][j] = sabr->modelParameters()[1];
				}
			}
		}

		return res;

	}
	
	Matrix SabrRbsSmile::rbsMu() {
		
		Matrix res(swapTenors_.size(), optionTenors_.size(),-1.0);

		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				boost::shared_ptr<RbsSmile> rbs = rbsSmile(optionTenors_[j],swapTenors_[i]);
				res[i][j] = rbs->modelParameters()[0];
			}
		}

		return res;

	}
	
	Matrix SabrRbsSmile::rbsNu() {
		
		Matrix res(swapTenors_.size(), optionTenors_.size(),-1.0);

		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				boost::shared_ptr<RbsSmile> rbs = rbsSmile(optionTenors_[j],swapTenors_[i]);
				res[i][j] = rbs->modelParameters()[1];
			}
		}

		return res;
	}

	void SabrRbsSmile::setupSabrModels() {

		boost::shared_ptr<SabrModel> sabr;
		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				//Date fixing=calendar_.advance(referenceDate_,optionTenors_[j],bdc_);
				Date fixing=volCube_->optionDateFromTenor(optionTenors_[j]);
				Period tenor=swapTenors_[i];
				double atm=volCube_->atmStrike(fixing,tenor);
				double atmVol=volCube_->volatility(fixing,tenor,atm,true);
				double optTime = time(fixing);
				double undTime = time(calendar_.advance(fixing,tenor,bdc_))-optTime;
				double beta = betaInterpol_(optTime,undTime,true);
				sabr = boost::shared_ptr<SabrModel>(new SabrModel(alpha_[i][j],beta,nu_[i][j],rho_[i][j],MINSTRIKE,BLACKACCURACY));
				vector<double> mktvols,strikes;
				for(int k=0;k<strikeSpreads_.size();k++) {
					double str=atm+strikeSpreads_[k];
					if(str > MINSTRIKE) {
						strikes.push_back(str);
						mktvols.push_back(volCube_->volatility(fixing,tenor,str,true));
					}
				}
				//FILE *out=fopen("cost.log","a");
				//fprintf(out,"CALIBRATE #%d,%d\n",i,j);
				double err=sabr->calibrate(atm,optTime,strikes,mktvols,false,true,false,false,vegaWeighted_,acceptRmse_,haltonIterations_,
					endCriteria_,optMethod_); 
				double valid=false;
				sabrRmse_[i][j]=err;
				//fprintf(out,"\nRESULT %f\n",err);
				//fclose(out);
				if(err >= 0.0 && err < rejectRmse_) { // -1.0 is returned if no valid calibration could be done
					alpha_[i][j]=sabr->alpha();
					nu_[i][j]=sabr->nu();
					rho_[i][j]=sabr->rho();
					valid=true;
					sabrModels_.insert(pair<pair<Date,Period>,boost::shared_ptr<SabrModel>>(pair<Date,Period>(fixing,tenor),sabr));
				}
				sabrValid_.insert(pair<pair<Date,Period>,bool>(pair<Date,Period>(fixing,tenor),valid));
			}
		}

	}

	void SabrRbsSmile::setupRbsSmiles() {
		
		boost::shared_ptr<RbsSmile> rbs;
		boost::shared_ptr<SabrModel> sabr;
		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				//Date fixing=calendar_.advance(referenceDate_,optionTenors_[j],bdc_);
				Date fixing=volCube_->optionDateFromTenor(optionTenors_[j]);
				Period tenor=swapTenors_[i];
				double atm=volCube_->atmStrike(fixing,tenor);
				double optTime = time(fixing);
				double undTime = time(calendar_.advance(fixing,tenor,bdc_))-optTime;
				double mu = muInterpol_(optTime,undTime,true);
				double nu = nuInterpol_(optTime,undTime,true);
				// compute price derivatives at boundaries
				// sabr=sabrModel(fixing,tenor);
				double k0 = atm + leftBound_;
				double k1 = atm + rightBound_;
				double p00=0.0,p01=0.0,p02=0.0,p10=0.0,p11=0.0,p12=0.0,left1d=0.0,left2d=0.0,right1d=0.0,right2d=0.0;
				if(k0>=MINSTRIKE) {
					p00 = this->optionPrice(fixing,tenor,k0,Option::Put,false,false,true); // (forced) sabr price, if not valid, fall back price
					p01 = this->optionPrice(fixing,tenor,k0+h_,Option::Put,false,false,true);
					p02 = this->optionPrice(fixing,tenor,k0+2.0*h_,Option::Put,false,false,true);
					left1d = (p01-p00)/h_;
					left2d = (p02-2.0*p01+p00)/(h_*h_);
				}
				p10 = this->optionPrice(fixing,tenor,k1-2.0*h_,Option::Call,false,false,true);
				p11 = this->optionPrice(fixing,tenor,k1-h_,Option::Call,false,false,true);
				p12 = this->optionPrice(fixing,tenor,k1,Option::Call,false,false,true);
				right1d = (p11-p10)/h_;
				right2d = (p12-2.0*p11+p10)/(h_*h_);
				// setup rbs smile (k0 might be negative, but then the left wing will not be set up and a volatility will never retrieved in rbs smile)
				// check first and second derivatives
				//
				rbs = boost::shared_ptr<RbsSmile>(new RbsSmile(k0,k1,p00,p12,left1d,left2d,
					right1d,right2d,mu,nu));
				rbsSmiles_.insert(pair<pair<Date,Period>,boost::shared_ptr<RbsSmile>>(pair<Date,Period>(fixing,tenor),rbs));
			}
		}

	}

	bool SabrRbsSmile::recalibrate(const Period& option, const Period& tenor, bool calSabr) {
		
		//Date fixing=calendar_.advance(referenceDate_,option,bdc_);
		Date fixing=volCube_->optionDateFromTenor(option);
		double atm=volCube_->atmStrike(fixing,tenor);
		double atmVol=volCube_->volatility(fixing,tenor,atm,true);
		double optTime = time(fixing);
		double undTime = time(calendar_.advance(fixing,tenor,bdc_))-optTime;
		bool sabrValid=true;
		bool recalibratedSabrValid=true;
		// look up sabr model
		boost::shared_ptr<SabrModel> sabr;
		sabrValidIter_ = sabrValid_.find(pair<Date,Period>(fixing,tenor));
		if(sabrValidIter_ == sabrValid_.end()) {
			QL_FAIL("option / swap pair is not a sabr model pillar in the matrix"); 
		}
		else {
			if(!(*sabrValidIter_).second) {
				sabrValid=false;
			}
			else {
				sabrModelsIter_ = sabrModels_.find(pair<Date,Period>(fixing,tenor));
				if(sabrModelsIter_ == sabrModels_.end()) {
					QL_FAIL("sabr model not found (although marked valid, this should never happen)"); 
				}
				else {
					sabr=(*sabrModelsIter_).second;
				}
			}
		}
		// look up indices i (option) j (swap tenor)
		int i=0,j=0;
		while(optionTenors_[j] != option && j < optionTenors_.size()) j++;
		if(j==optionTenors_.size()) QL_FAIL("option tenor not found (should never happen)");
		while(swapTenors_[i] != tenor && i < swapTenors_.size()) i++;
		if(i==swapTenors_.size()) QL_FAIL("swap tenor not found (should never happen)");

		//FILE* out = fopen("recal.log","a");
		// recalibrate sabr model w.r.t. beta from termstructure
		if(calSabr) {
			if(sabrValid) {
				double beta = betaInterpol_(optTime,undTime,true);
				vector<double> mktvols,strikes;
				for(int k=0;k<strikeSpreads_.size();k++) {
					double str=atm+strikeSpreads_[k];
					if(str > MINSTRIKE) {
						strikes.push_back(str);
						mktvols.push_back(volCube_->volatility(fixing,tenor,str,true));
					}
				}
				vector<double> originalParams = sabr->modelParameters();
				sabr->recalibrate(originalParams[0],beta,originalParams[2],originalParams[3]);
				double err=sabr->calibrate(atm,optTime,strikes,mktvols,false,true,false,false,vegaWeighted_,acceptRmse_,haltonIterations_,
					endCriteria_,optMethod_); 
				double valid=false;
				if(err >= 0.0 && err < rejectRmse_) { // -1.0 is returned if no valid calibration could be done
					alpha_[i][j]=sabr->alpha();
					nu_[i][j]=sabr->nu();
					rho_[i][j]=sabr->rho();
					valid=true;
					sabrRmse_[i][j]=err;
					//fprintf("Recalibrated swap=%d,option=%d, %f %f %f %f\n",alpha_[i][j],beta
				}
				else { // reset model to its original values
					sabr->recalibrate(originalParams[0],originalParams[1],originalParams[2],originalParams[3]);
					recalibratedSabrValid=false;
				}
			}
		}
		// recalibrate rbs smile w.r.t. new sabr model and mu, nu from termstructure
		if(recalibratedSabrValid) {
			boost::shared_ptr<RbsSmile> rbs;
			rbsSmilesIter_ = rbsSmiles_.find(pair<Date,Period>(fixing,tenor));
			if(rbsSmilesIter_ == rbsSmiles_.end()) {
				QL_FAIL("rbs smile not found (this should never happen)"); 
			}
			else {
				rbs=(*rbsSmilesIter_).second;
			}
			double mu = muInterpol_(optTime,undTime,true);
			double nu = nuInterpol_(optTime,undTime,true);
			// compute price derivatives at boundaries
			// sabr=sabrModel(fixing,tenor);
			double k0 = atm + leftBound_;
			double k1 = atm + rightBound_;
			double p00,p01,p02,p10,p11,p12,left1d,left2d,right1d,right2d;
			if(k0>=MINSTRIKE) {
				p00 = this->optionPrice(fixing,tenor,k0,Option::Put,false,false,true); // (forced) sabr price, if not valid, fall back price
				p01 = this->optionPrice(fixing,tenor,k0+h_,Option::Put,false,false,true);
				p02 = this->optionPrice(fixing,tenor,k0+2.0*h_,Option::Put,false,false,true);
				left1d = (p01-p00)/h_;
				left2d = (p02-2.0*p01+p00)/(h_*h_);
			}
			p10 = this->optionPrice(fixing,tenor,k1-2.0*h_,Option::Call,false,false,true);
			p11 = this->optionPrice(fixing,tenor,k1-h_,Option::Call,false,false,true);
			p12 = this->optionPrice(fixing,tenor,k1,Option::Call,false,false,true);
			right1d = (p11-p10)/h_;
			right2d = (p12-2.0*p11+p10)/(h_*h_);
			// setup rbs smile (k0 might be negative, but then the left wing will not be set up and a volatility will never retrieved in rbs smile)
			// check first and second derivatives
			//
			rbs->recalibrate(mu,nu,p00,p12,left1d,left2d,right1d,right2d);
			return true;
		}
		return false; 
	}

	bool SabrRbsSmile::recalibrate(bool calSabr) {
		
		bool allValid=true;
		for(int i=0;i<swapTenors_.size();i++) {
			for(int j=0;j<optionTenors_.size();j++) {
				if(!recalibrate(optionTenors_[j],swapTenors_[i],calSabr))
					allValid=false;
			}
		}
		return allValid;

	}

	bool SabrRbsSmile::recalibrate(const int maxOptPillar, const int undPillar, bool calSabr) {
		
		bool allValid=true;
		Period optMax = optPillars_[maxOptPillar];
		Period und = undPillars_[undPillar];
		int i=0;
		while(i<optionTenors_.size() && optionTenors_[i] <= optMax) {
			allValid = allValid & recalibrate(optionTenors_[i],und,calSabr);
			i++;
		}
		return allValid;
	
	}



	
}
