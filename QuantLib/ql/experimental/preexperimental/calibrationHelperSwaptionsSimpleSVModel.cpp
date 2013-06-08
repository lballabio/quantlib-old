#include <calibrationHelperSwaptionsSimpleSVModel.hpp>

namespace QuantLib {

	CalibrationHelperSwaptionsSimpleSVModel::CalibrationHelperSwaptionsSimpleSVModel(const boost::shared_ptr<YieldTermStructure> yts, 
		const boost::shared_ptr<SwaptionVolatilityCube> swaptionVolCube,
		const vector<Period>& optionTenors,
		const vector<Period>& swapTenors,
		const vector<double>& strikeSpreads) : yts_(yts), swaptionVolCube_(swaptionVolCube), strikeSpreads_(strikeSpreads) {

			calibrationDone_ = false;
			optionTenors_=std::vector<Period>(0);
			swapTenors_=std::vector<Period>(0);
			for(int i=0;i<optionTenors.size();i++) {
				for(int j=0;j<swapTenors.size();j++) {
					optionTenors_.push_back(optionTenors[i]);
					swapTenors_.push_back(swapTenors[j]);
				}
			}
			result_=Matrix(optionTenors_.size(),8+strikeSpreads_.size()*2);
	}

	const Matrix& CalibrationHelperSwaptionsSimpleSVModel::stochVolParameters(double lambda, double b, 
		double theta, double eta, bool lambdaFixed, bool bFixed, bool thetaFixed, bool etaFixed,bool useImpliedVols) {

			if(!calibrationDone_) {

				//initGslQawo();

				for(int i=0;i<optionTenors_.size();i++) {
					double atm = swaptionVolCube_->atmStrike(optionTenors_[i],swapTenors_[i]);
					vector<double> strikes;
					vector<double> blackVols;
					for(int j=0;j<strikeSpreads_.size();j++) {
						double fwd = atm + strikeSpreads_[j];
						if(fwd>0.0) {
							strikes.push_back(fwd);
							double marketVol = swaptionVolCube_->volatility(optionTenors_[i],swapTenors_[i],fwd);
							blackVols.push_back(marketVol);
							result_[i][8+j]=marketVol;
						}
					}
					double maturity = swaptionVolCube_->timeFromReference(swaptionVolCube_->calendar().advance(swaptionVolCube_->referenceDate(),optionTenors_[i]));
					double swapLength = swaptionVolCube_->timeFromReference(swaptionVolCube_->calendar().advance(swaptionVolCube_->referenceDate(),optionTenors_[i]+swapTenors_[i]))-maturity;
					SimpleSVModel m(lambda,b,theta,eta,1.0,1.0);
					double rmse=1000.0;
					try {
						rmse=m.calibrate(atm, maturity, strikes, blackVols,lambdaFixed,bFixed,thetaFixed,etaFixed,useImpliedVols);
					} catch(QuantLib::Error er) {
						QL_FAIL("QL threw an exception atm=" << atm << " maturity=" << maturity << " : " << er.what());
					} catch(std::exception ex) {
						QL_FAIL("An non-QL exception is thrown:" << ex.what());
					}
					result_[i][0]=maturity;
					result_[i][1]=swapLength;
					result_[i][2]=atm;
					result_[i][3]=m.lambda();
					result_[i][4]=m.b();
					result_[i][5]=m.theta();
					result_[i][6]=m.eta();
					result_[i][7]=rmse;
					for(int j=0;j<strikeSpreads_.size();j++) result_[i][8+strikeSpreads_.size()+j] = m.modelImpliedVols()[j];
				}

				calibrationDone_=true;
			}

			//freeGslQawo();

			return result_;
	}


}