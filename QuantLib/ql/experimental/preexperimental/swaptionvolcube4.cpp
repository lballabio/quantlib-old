#include <swaptionvolcube4.hpp>

namespace QuantLib {

    //=======================================================================//
    //                        SwaptionVolCube4                               //
    //=======================================================================//

    SwaptionVolCube4::SwaptionVolCube4(
                const Handle<SwaptionVolatilityStructure>& atmVolStructure,
                const std::vector<Period>& optionTenors,
                const std::vector<Period>& swapTenors,
                const std::vector<Spread>& strikeSpreads,
                const std::vector<std::vector<Handle<Quote> > >& volSpreads,
                const boost::shared_ptr<SwapIndex>& swapIndexBase,
                const boost::shared_ptr<SwapIndex>& shortSwapIndexBase,
                bool vegaWeightedSmileFit,
                const std::vector<std::vector<Handle<Quote> > >& parametersGuess,
                const std::vector<bool>& isParameterFixed,
                bool isAtmCalibrated,
				const double leftCoreSpread, const double rightCoreSpread,
				const int checkAfGridPoints,
				const Real acceptRmse, const int haltonIterations,
                const Real rejectRmse,
				const Real h, const Real minStrike,
				const boost::shared_ptr<EndCriteria>& endCriteria,
                const boost::shared_ptr<OptimizationMethod>& optMethod
				)
    : SwaptionVolatilityCube(atmVolStructure, optionTenors, swapTenors,
                             strikeSpreads, volSpreads, swapIndexBase,
                             shortSwapIndexBase,
                             vegaWeightedSmileFit),
      parametersGuessQuotes_(parametersGuess),
      isParameterFixed_(isParameterFixed), isAtmCalibrated_(isAtmCalibrated),
      endCriteria_(endCriteria), optMethod_(optMethod),
	  leftCoreSpread_(leftCoreSpread), rightCoreSpread_(rightCoreSpread), checkAfGridPoints_(checkAfGridPoints),
	  acceptRmse_(acceptRmse), rejectRmse_(rejectRmse), haltonIterations_(haltonIterations), h_(h),
	  minStrike_(minStrike)
    {
	   QL_REQUIRE(isParameterFixed_.size()==6,"isParameterFixed must have length 6 (" << isParameterFixed_.size() << ")");	
	   QL_REQUIRE(parametersGuessQuotes_.size()==nSwapTenors_*nOptionTenors_,"Parameter guess must have " << nSwapTenors_*nOptionTenors_ << " lines (" << parametersGuessQuotes_.size() << ")");
	   for(int i=0;i<nSwapTenors_*nOptionTenors_;i++) {
		   QL_REQUIRE(parametersGuessQuotes_[i].size()==6,"Parameter guess line #" << (i+1) << " must contain 6 (" << parametersGuessQuotes_[i].size() << ") numbers."); 
	   }
       registerWithParametersGuess();
    }

    void SwaptionVolCube4::registerWithParametersGuess()
    {
        for (Size i=0; i<6; i++)
            for (Size j=0; j<nOptionTenors_; j++)
                for (Size k=0; k<nSwapTenors_; k++)
                    registerWith(parametersGuessQuotes_[j+k*nOptionTenors_][i]);
    }

    void SwaptionVolCube4::performCalculations() const {

        SwaptionVolatilityDiscrete::performCalculations();

        //! set parametersGuess_ by parametersGuessQuotes_
		parametersGuess_ = Cube(optionDates_, swapTenors_,
                                optionTimes_, swapLengths_, 6);
        Size i;
        for (i=0; i<6; i++)
            for (Size j=0; j<nOptionTenors_ ; j++)
                for (Size k=0; k<nSwapTenors_; k++) {
                    parametersGuess_.setElement(i, j, k,
                        parametersGuessQuotes_[j+k*nOptionTenors_][i]->value());
                }
        parametersGuess_.updateInterpolators();

        //! set marketVolCube_ by volSpreads_ quotes
        marketVolCube_ = Cube(optionDates_, swapTenors_,
                              optionTimes_, swapLengths_, nStrikes_);
        Rate atmForward;
        Volatility atmVol, vol;
        for (Size j=0; j<nOptionTenors_; ++j) {
            for (Size k=0; k<nSwapTenors_; ++k) {
                atmForward = atmStrike(optionDates_[j], swapTenors_[k]);
                atmVol = atmVol_->volatility(optionDates_[j], swapTenors_[k],
                                                              atmForward);
                for (Size i=0; i<nStrikes_; ++i) {
                    vol = atmVol + volSpreads_[j*nSwapTenors_+k][i]->value();
                    marketVolCube_.setElement(i, j, k, vol);
                }
            }
        }
        marketVolCube_.updateInterpolators();

        sparseParameters_ = sabrRbsCalibration(marketVolCube_);
        //parametersGuess_ = sparseParameters_;
        sparseParameters_.updateInterpolators();
        //parametersGuess_.updateInterpolators();
        volCubeAtmCalibrated_= marketVolCube_;

        if(isAtmCalibrated_){
            fillVolatilityCube();
            denseParameters_ = sabrRbsCalibration(volCubeAtmCalibrated_);
            denseParameters_.updateInterpolators();
        }
    }

	void SwaptionVolCube4::updateAfterRecalibration() {
		/*parametersGuess_.updateInterpolators();
		marketVolCube_.updateInterpolators();
        sparseParameters_ = sabrRbsCalibration(marketVolCube_);
        //parametersGuess_ = sparseParameters_;
        sparseParameters_.updateInterpolators();
        //parametersGuess_.updateInterpolators();
		*/
		volCubeAtmCalibrated_= marketVolCube_;
        if(isAtmCalibrated_){
            fillVolatilityCube();
            denseParameters_ = sabrRbsCalibration(volCubeAtmCalibrated_);
            denseParameters_.updateInterpolators();
        }
	}

    SwaptionVolCube4::Cube
    SwaptionVolCube4::sabrRbsCalibration(const Cube& marketVolCube) const {

        const std::vector<Time>& optionTimes = marketVolCube.optionTimes();
        const std::vector<Time>& swapLengths = marketVolCube.swapLengths();
        const std::vector<Date>& optionDates = marketVolCube.optionDates();
        const std::vector<Period>& swapTenors = marketVolCube.swapTenors();
        Matrix alphas(optionTimes.size(), swapLengths.size(),0.);
        Matrix betas(alphas);
        Matrix nus(alphas);
        Matrix rhos(alphas);
		Matrix pmus(alphas);
		Matrix pnus(alphas);
        Matrix forwards(alphas);
        Matrix errors(alphas);
		Matrix leftCoreSpreads(alphas);
		Matrix rightCoreSpreads(alphas);

        const std::vector<Matrix>& tmpMarketVolCube = marketVolCube.points();

        std::vector<Real> strikes;
        std::vector<Real> volatilities;

        for (Size j=0; j<optionTimes.size(); j++) {
            for (Size k=0; k<swapLengths.size(); k++) {
                Rate atmForward = atmStrike(optionDates[j], swapTenors[k]);
				strikes.clear();
				volatilities.clear();
                for (Size i=0; i<nStrikes_; i++){
					double strike = atmForward+strikeSpreads_[i];
					if(strike>minStrike_) {
						strikes.push_back(strike);
						volatilities.push_back(tmpMarketVolCube[i][j][k]);
					}
                }

                const std::vector<Real>& guess = parametersGuess_.operator()(
                    optionTimes[j], swapLengths[k]);

                SabrModel tmpSabr(guess[0], guess[1], guess[2], guess[3]);
				QL_ENSURE(atmForward>0.0,"swaptionVolCube4: atm forward (" << atmForward << ") is not positive. Option time = " << optionTimes[j] << " Swap length = " << swapLengths[k]);  
				Real rmsError=tmpSabr.calibrate(atmForward,optionTimes[j],strikes,volatilities,
					isParameterFixed_[0],isParameterFixed_[1],isParameterFixed_[2],isParameterFixed_[3],
					vegaWeightedSmileFit_,acceptRmse_,haltonIterations_,endCriteria_,optMethod_);
				
				QL_ENSURE(rmsError!=-1.0,"No valid calibration could be done: \n" <<
										 "option maturity = " << optionDates[j] << ", \n" <<
										 "swap tenor = " << swapTenors[k] << ", \n");

				QL_ENSURE(rmsError<=rejectRmse_,"Best rmse (" << rmsError << ") is above reject rmse (" << rejectRmse_ << "): \n" <<
										 "option maturity = " << optionDates[j] << ", \n" <<
										 "swap tenor = " << swapTenors[k] << ", \n");

				// determine af region as core region

				if(checkAfGridPoints_==0) { // do not check, take default values given in constructor
					leftCoreSpreads[j][k] = leftCoreSpread_;
					rightCoreSpreads[j][k] = rightCoreSpread_;
				}
				else {
					double hl = fabs(leftCoreSpread_) / (double)checkAfGridPoints_;
					double hr = fabs(rightCoreSpread_) / (double)checkAfGridPoints_;
					double optMat = timeFromReference(optionDates[j]);
					vector<double> afStrikes;
					vector<double> afVols;
					for(int i=0;i<checkAfGridPoints_;i++) {
						double tmpStrike=atmForward+leftCoreSpread_+i*hl;
						afStrikes.push_back(tmpStrike);
						afVols.push_back(tmpSabr.impliedVola(atmForward,tmpStrike,optMat));
					}
					afStrikes.push_back(atmForward);
					afVols.push_back(tmpSabr.impliedVola(atmForward,atmForward,optMat));
					for(int i=0;i<checkAfGridPoints_;i++) {
						double tmpStrike=atmForward+rightCoreSpread_+i*hl;
						afStrikes.push_back(tmpStrike);
						afVols.push_back(tmpSabr.impliedVola(atmForward,tmpStrike,optMat));
					}
					vector<long> afRegion = smileArbitrageFree(atmForward,optMat,afStrikes,afVols,checkAfGridPoints_);
					leftCoreSpreads[j][k] = max(afStrikes[afRegion[0]]-atmForward,leftCoreSpread_);
					rightCoreSpreads[j][k] = min(afStrikes[afRegion[1]]-atmForward,rightCoreSpread_);
					//FILE *out=fopen("sabrCal.log","a");
					//fprintf(out,"j=%d k=%d [%d,%d] = [%f;%f], fwd=%f\n",j,k,afRegion[0],afRegion[1],afStrikes[afRegion[0]],afStrikes[afRegion[1]],atmForward);
					//fclose(out);
				}

				// set data

                alphas     [j][k] = tmpSabr.alpha();
                betas      [j][k] = tmpSabr.beta();
                nus        [j][k] = tmpSabr.nu();
                rhos       [j][k] = tmpSabr.rho();
				pmus       [j][k] = guess[4];
				pnus       [j][k] = guess[5];
                forwards   [j][k] = atmForward;
                errors     [j][k] = rmsError;
            }
        }
        Cube sabrParametersCube(optionDates, swapTenors,
                                optionTimes, swapLengths, 10);
        sabrParametersCube.setLayer(0, alphas);
        sabrParametersCube.setLayer(1, betas);
        sabrParametersCube.setLayer(2, nus);
        sabrParametersCube.setLayer(3, rhos);
		sabrParametersCube.setLayer(4, pmus);
		sabrParametersCube.setLayer(5, pnus);
        sabrParametersCube.setLayer(6, forwards);
        sabrParametersCube.setLayer(7, errors);
		sabrParametersCube.setLayer(8, leftCoreSpreads);
		sabrParametersCube.setLayer(9, rightCoreSpreads);

        return sabrParametersCube;

    }

    void SwaptionVolCube4::sabrRbsCalibrationSection(
                                            const Cube& marketVolCube,
                                            Cube& parametersCube,
                                            const Period& swapTenor) const {

        const std::vector<Time>& optionTimes = marketVolCube.optionTimes();
        const std::vector<Time>& swapLengths = marketVolCube.swapLengths();
        const std::vector<Date>& optionDates = marketVolCube.optionDates();
        const std::vector<Period>& swapTenors = marketVolCube.swapTenors();

        Size k = std::find(swapTenors.begin(), swapTenors.end(),
                           swapTenor) - swapTenors.begin();
        QL_REQUIRE(k != swapTenors.size(), "swap tenor not found");

        std::vector<Real> calibrationResult(10,0.);
        const std::vector<Matrix>& tmpMarketVolCube = marketVolCube.points();

        std::vector<Real> strikes;
        std::vector<Real> volatilities;
		
        for (Size j=0; j<optionTimes.size(); j++) {
            Rate atmForward = atmStrike(optionDates[j], swapTenors[k]);
			strikes.clear();
			volatilities.clear();
            for (Size i=0; i<nStrikes_; i++){
                double strike = atmForward+strikeSpreads_[i];
				if(strike>minStrike_) {
					strikes.push_back(strike);
					volatilities.push_back(tmpMarketVolCube[i][j][k]);
				}
            }

            const std::vector<Real>& guess = parametersGuess_.operator()(
                optionTimes[j], swapLengths[k]);

			SabrModel tmpSabr(guess[0], guess[1], guess[2], guess[3]);
			Real rmsError=tmpSabr.calibrate(atmForward,optionTimes[j],strikes,volatilities,
				isParameterFixed_[0],isParameterFixed_[1],isParameterFixed_[2],isParameterFixed_[3],
				vegaWeightedSmileFit_,acceptRmse_,haltonIterations_,endCriteria_,optMethod_);

			QL_ENSURE(rmsError!=-1.0,"No valid calibration could be done: \n" <<
									 "option maturity = " << optionDates[j] << ", \n" <<
									 "swap tenor = " << swapTenors[k] << ", \n");

			QL_ENSURE(rmsError<=rejectRmse_,"Best rmse (" << rmsError << ") is above reject rmse (" << rejectRmse_ << "): \n" <<
									 "option maturity = " << optionDates[j] << ", \n" <<
									 "swap tenor = " << swapTenors[k] << ", \n");

			// determine af region as core region
			double leftCoreSpread,rightCoreSpread;
			if(checkAfGridPoints_==0) { // do not check, take default values given in constructor
				leftCoreSpread = leftCoreSpread_;
				rightCoreSpread = rightCoreSpread_;
			}
			else {
				double hl = fabs(leftCoreSpread_) / (double)checkAfGridPoints_;
				double hr = fabs(rightCoreSpread_) / (double)checkAfGridPoints_;
				double optMat = timeFromReference(optionDates[j]);
				vector<double> afStrikes;
				vector<double> afVols;
				for(int i=0;i<checkAfGridPoints_;i++) {
					double tmpStrike=atmForward+leftCoreSpread_+i*hl;
					afStrikes.push_back(tmpStrike);
					afVols.push_back(tmpSabr.impliedVola(atmForward,tmpStrike,optMat));
				}
				afStrikes.push_back(atmForward);
				afVols.push_back(tmpSabr.impliedVola(atmForward,atmForward,optMat));
				for(int i=0;i<checkAfGridPoints_;i++) {
					double tmpStrike=atmForward+rightCoreSpread_+i*hl;
					afStrikes.push_back(tmpStrike);
					afVols.push_back(tmpSabr.impliedVola(atmForward,tmpStrike,optMat));
				}
				vector<long> afRegion = smileArbitrageFree(atmForward,optMat,afStrikes,afVols,checkAfGridPoints_);
				leftCoreSpread = max(afStrikes[afRegion[0]]-atmForward,leftCoreSpread_);
				rightCoreSpread = min(afStrikes[afRegion[1]]-atmForward,rightCoreSpread_);
			}

			// set data


            calibrationResult[0]=tmpSabr.alpha();
            calibrationResult[1]=tmpSabr.beta();
            calibrationResult[2]=tmpSabr.nu();
            calibrationResult[3]=tmpSabr.rho();
			calibrationResult[4]=guess[4];
			calibrationResult[5]=guess[5];
            calibrationResult[6]=atmForward;
            calibrationResult[7]=rmsError;
			calibrationResult[8]=leftCoreSpread;
			calibrationResult[9]=rightCoreSpread;
           
            parametersCube.setPoint(optionDates[j], swapTenors[k],
                                    optionTimes[j], swapLengths[k],
                                    calibrationResult);
            parametersCube.updateInterpolators();
        }

    }

    void SwaptionVolCube4::fillVolatilityCube() const {

        const boost::shared_ptr<SwaptionVolatilityDiscrete> atmVolStructure =
            boost::dynamic_pointer_cast<SwaptionVolatilityDiscrete>(*atmVol_);

        std::vector<Time> atmOptionTimes(atmVolStructure->optionTimes());
        std::vector<Time> optionTimes(volCubeAtmCalibrated_.optionTimes());
        atmOptionTimes.insert(atmOptionTimes.end(),
                              optionTimes.begin(), optionTimes.end());
        std::sort(atmOptionTimes.begin(),atmOptionTimes.end());
        std::vector<Time>::iterator new_end =
            std::unique(atmOptionTimes.begin(), atmOptionTimes.end());
        atmOptionTimes.erase(new_end, atmOptionTimes.end());

        std::vector<Time> atmSwapLengths(atmVolStructure->swapLengths());
        std::vector<Time> swapLengths(volCubeAtmCalibrated_.swapLengths());
        atmSwapLengths.insert(atmSwapLengths.end(),
                              swapLengths.begin(), swapLengths.end());
        std::sort(atmSwapLengths.begin(),atmSwapLengths.end());
        new_end = std::unique(atmSwapLengths.begin(), atmSwapLengths.end());
        atmSwapLengths.erase(new_end, atmSwapLengths.end());

        std::vector<Date> atmOptionDates = atmVolStructure->optionDates();
        std::vector<Date> optionDates(volCubeAtmCalibrated_.optionDates());
        atmOptionDates.insert(atmOptionDates.end(),
                                optionDates.begin(), optionDates.end());
        std::sort(atmOptionDates.begin(),atmOptionDates.end());
        std::vector<Date>::iterator new_end_1 =
            std::unique(atmOptionDates.begin(), atmOptionDates.end());
        atmOptionDates.erase(new_end_1, atmOptionDates.end());

        std::vector<Period> atmSwapTenors = atmVolStructure->swapTenors();
        std::vector<Period> swapTenors(volCubeAtmCalibrated_.swapTenors());
        atmSwapTenors.insert(atmSwapTenors.end(),
                             swapTenors.begin(), swapTenors.end());
        std::sort(atmSwapTenors.begin(),atmSwapTenors.end());
        std::vector<Period>::iterator new_end_2 =
            std::unique(atmSwapTenors.begin(), atmSwapTenors.end());
        atmSwapTenors.erase(new_end_2, atmSwapTenors.end());

        createSparseSmiles();

        for (Size j=0; j<atmOptionTimes.size(); j++) {

            for (Size k=0; k<atmSwapLengths.size(); k++) {
                bool expandOptionTimes =
                    !(std::binary_search(optionTimes.begin(),
                                         optionTimes.end(),
                                         atmOptionTimes[j]));
                bool expandSwapLengths =
                    !(std::binary_search(swapLengths.begin(),
                                         swapLengths.end(),
                                         atmSwapLengths[k]));
                if(expandOptionTimes || expandSwapLengths){
                    Rate atmForward = atmStrike(atmOptionDates[j],
                                                atmSwapTenors[k]);
                    Volatility atmVol = atmVol_->volatility(
                        atmOptionDates[j], atmSwapTenors[k], atmForward);
                    std::vector<Real> spreadVols =
                        spreadVolInterpolation(atmOptionDates[j],
                                               atmSwapTenors[k]);
                    std::vector<Real> volAtmCalibrated;
                    volAtmCalibrated.reserve(nStrikes_);
                    for (Size i=0; i<nStrikes_; i++)
                        volAtmCalibrated.push_back(atmVol + spreadVols[i]);
                    volCubeAtmCalibrated_.setPoint(
                                    atmOptionDates[j], atmSwapTenors[k],
                                    atmOptionTimes[j], atmSwapLengths[k],
                                    volAtmCalibrated);
                }
            }
        }
        volCubeAtmCalibrated_.updateInterpolators();
    }


    void SwaptionVolCube4::createSparseSmiles() const {

        std::vector<Time> optionTimes(sparseParameters_.optionTimes());
        std::vector<Time> swapLengths(sparseParameters_.swapLengths());
        sparseSmiles_.clear();

        for (Size j=0; j<optionTimes.size(); j++) {
            std::vector<boost::shared_ptr<SmileSection> > tmp;
            Size n = swapLengths.size();
            tmp.reserve(n);
            for (Size k=0; k<n; ++k) {
                tmp.push_back(smileSection(optionTimes[j], swapLengths[k],
                                           sparseParameters_));
            }
            sparseSmiles_.push_back(tmp);
        }
    }


    std::vector<Real> SwaptionVolCube4::spreadVolInterpolation(
        const Date& atmOptionDate, const Period& atmSwapTenor) const {

        Time atmOptionTime = timeFromReference(atmOptionDate);
        Time atmTimeLength = swapLength(atmSwapTenor);

        std::vector<Real> result;
        const std::vector<Time>& optionTimes(sparseParameters_.optionTimes());
        const std::vector<Time>& swapLengths(sparseParameters_.swapLengths());
        const std::vector<Date>& optionDates =
            sparseParameters_.optionDates();
        const std::vector<Period>& swapTenors = sparseParameters_.swapTenors();

        std::vector<Real>::const_iterator optionTimesPreviousNode,
                                          swapLengthsPreviousNode;

        optionTimesPreviousNode = std::lower_bound(optionTimes.begin(),
                                                   optionTimes.end(),
                                                   atmOptionTime);
        Size optionTimesPreviousIndex =
            optionTimesPreviousNode - optionTimes.begin();
        if (optionTimesPreviousIndex >0)
            optionTimesPreviousIndex --;

        swapLengthsPreviousNode = std::lower_bound(swapLengths.begin(),
                                                   swapLengths.end(),
                                                   atmTimeLength);
        Size swapLengthsPreviousIndex = swapLengthsPreviousNode - swapLengths.begin();
        if (swapLengthsPreviousIndex >0)
            swapLengthsPreviousIndex --;

        std::vector< std::vector<boost::shared_ptr<SmileSection> > > smiles;
        std::vector<boost::shared_ptr<SmileSection> >  smilesOnPreviousExpiry;
        std::vector<boost::shared_ptr<SmileSection> >  smilesOnNextExpiry;

        QL_REQUIRE(optionTimesPreviousIndex+1 < sparseSmiles_.size(),
                   "optionTimesPreviousIndex+1 >= sparseSmiles_.size()");
        QL_REQUIRE(swapLengthsPreviousIndex+1 < sparseSmiles_[0].size(),
                   "swapLengthsPreviousIndex+1 >= sparseSmiles_[0].size()");
        smilesOnPreviousExpiry.push_back(
              sparseSmiles_[optionTimesPreviousIndex][swapLengthsPreviousIndex]);
        smilesOnPreviousExpiry.push_back(
              sparseSmiles_[optionTimesPreviousIndex][swapLengthsPreviousIndex+1]);
        smilesOnNextExpiry.push_back(
              sparseSmiles_[optionTimesPreviousIndex+1][swapLengthsPreviousIndex]);
        smilesOnNextExpiry.push_back(
              sparseSmiles_[optionTimesPreviousIndex+1][swapLengthsPreviousIndex+1]);

        smiles.push_back(smilesOnPreviousExpiry);
        smiles.push_back(smilesOnNextExpiry);

        std::vector<Real> optionsNodes(2);
        optionsNodes[0] = optionTimes[optionTimesPreviousIndex];
        optionsNodes[1] = optionTimes[optionTimesPreviousIndex+1];

        std::vector<Date> optionsDateNodes(2);
        optionsDateNodes[0] = optionDates[optionTimesPreviousIndex];
        optionsDateNodes[1] = optionDates[optionTimesPreviousIndex+1];

        std::vector<Real> swapLengthsNodes(2);
        swapLengthsNodes[0] = swapLengths[swapLengthsPreviousIndex];
        swapLengthsNodes[1] = swapLengths[swapLengthsPreviousIndex+1];

        std::vector<Period> swapTenorNodes(2);
        swapTenorNodes[0] = swapTenors[swapLengthsPreviousIndex];
        swapTenorNodes[1] = swapTenors[swapLengthsPreviousIndex+1];

        Rate atmForward = atmStrike(atmOptionDate, atmSwapTenor);

        Matrix atmForwards(2, 2, 0.0);
        Matrix atmVols(2, 2, 0.0);
        for (Size i=0; i<2; i++) {
            for (Size j=0; j<2; j++) {
                atmForwards[i][j] = atmStrike(optionsDateNodes[i],
                                              swapTenorNodes[j]);                
                // atmVols[i][j] = smiles[i][j]->volatility(atmForwards[i][j]);
                atmVols[i][j] = atmVol_->volatility(
                    optionsDateNodes[i], swapTenorNodes[j], atmForwards[i][j]);
                /* With the old implementation the interpolated spreads on ATM 
                   volatilities were null even if the spreads on ATM volatilities to be
                   interpolated were non-zero. The new implementation removes
                   this behaviour, but introduces a small ERROR in the cube:
                   even if no spreads are applied on any cube ATM volatility corresponding
                   to quoted smile sections (that is ATM volatilities in sparse cube), the
                   cube ATM volatilities corresponding to not quoted smile sections (that
                   is ATM volatilities in dense cube) are no more exactly the quoted values,
                   but that ones PLUS the linear interpolation of the fit errors on the ATM
                   volatilities in sparse cube whose spreads are used in the calculation.
                   A similar imprecision is introduced to the volatilities in dense cube
                   whith moneyness near to 1.
                   (See below how spreadVols are calculated). 
                   The extent of this error depends on the quality of the fit: in case of
                   good fits it is negligibile.                  
                */
            }
        }

        for (Size k=0; k<nStrikes_; k++){
			const Real strike = std::max(atmForward + strikeSpreads_[k] , minStrike_); // prevent strike of getting negative
            const Real moneyness = atmForward/strike;

            Matrix strikes(2,2,0.);
            Matrix spreadVols(2,2,0.);
            for (Size i=0; i<2; i++){
                for (Size j=0; j<2; j++){
                    strikes[i][j] = atmForwards[i][j]/moneyness;
                    spreadVols[i][j] =
                        smiles[i][j]->volatility(strikes[i][j]) - atmVols[i][j];
                }
            }
           Cube localInterpolator(optionsDateNodes, swapTenorNodes,
                                  optionsNodes, swapLengthsNodes, 1);
           localInterpolator.setLayer(0, spreadVols);
           localInterpolator.updateInterpolators();

           result.push_back(localInterpolator(atmOptionTime, atmTimeLength)[0]);
        }
        return result;
    }

    boost::shared_ptr<SmileSection>
    SwaptionVolCube4::smileSection(Time optionTime, Time swapLength,
                                   const Cube& sabrRbsParametersCube) const {

        Date optionDate = Date(static_cast<BigInteger>(optionInterpolator_(optionTime)));
        Rounding rounder(0);
        Period swapTenor(static_cast<Integer>(rounder(swapLength*12.0)), Months);
        return smileSection(optionDate, swapTenor,sabrRbsParametersCube);
    }

    boost::shared_ptr<SmileSection> 
    SwaptionVolCube4::smileSection(const Date& optionDate,
									   const Period& swapTenor,
									   const Cube& sabrRbsParametersCube) const {

 	    calculate();
		Time optionTime = timeFromReference(optionDate);
		Time swapLen = swapLength(swapTenor);
        const std::vector<Real> sabrRbsParameters =
            sabrRbsParametersCube(optionTime, swapLen);

		SabrModel tmpSabr(sabrRbsParameters[0],sabrRbsParameters[1],sabrRbsParameters[2],sabrRbsParameters[3],minStrike_);
		
		// compute price derivatives at boundaries
		double atm=/*sabrRbsParameters[6];*/ atmStrike(optionDate,swapTenor); // FIXME this is fast, but inaccurate
		double k0 = atm + sabrRbsParameters[8]; //leftCoreSpread_;
		double k1 = atm + sabrRbsParameters[9]; //rightCoreSpread_;
		double p00,p01,p02,p10,p11,p12,left1d,left2d,right1d,right2d;
		if(k0>=minStrike_) {
			p00 = tmpSabr.optionPrice(atm,k0,optionTime,Option::Put); 
			p01 = tmpSabr.optionPrice(atm,k0+h_,optionTime,Option::Put);
			p02 = tmpSabr.optionPrice(atm,k0+2.0*h_,optionTime,Option::Put);
			left1d = (p01-p00)/h_;
			left2d = (p02-2.0*p01+p00)/(h_*h_);
		}
		else {
			p00=p01=p02=left1d=left2d=0.0;
		}
		p10 = tmpSabr.optionPrice(atm,k1-2.0*h_,optionTime,Option::Call);
		p11 = tmpSabr.optionPrice(atm,k1-h_,optionTime,Option::Call);
		p12 = tmpSabr.optionPrice(atm,k1,optionTime,Option::Call);
		right1d = (p11-p10)/h_;
		right2d = (p12-2.0*p11+p10)/(h_*h_);
		
		
		// setup rbs smile (k0 might be negative, but then the left wing will not be set up and a volatility will never retrieved in rbs smile)
		RbsSmile tmpRbs(k0/*atm+leftCoreSpread_*/,k1/*atm+rightCoreSpread_*/,p00,p12,left1d,left2d,right1d,right2d,sabrRbsParameters[4],sabrRbsParameters[5],minStrike_);
		
		std::vector<Real> sabrParams;
		sabrParams.push_back(sabrRbsParameters[0]);
		sabrParams.push_back(sabrRbsParameters[1]);
		sabrParams.push_back(sabrRbsParameters[2]);
		sabrParams.push_back(sabrRbsParameters[3]);
		
		return boost::shared_ptr<SmileSection>(new
            SabrRbsSmileSection(optionTime, atm, k0/*atm+leftCoreSpread_*/, k1/*atm+rightCoreSpread_*/,sabrParams,tmpRbs.modelParameters())); 

	}
	
	boost::shared_ptr<SmileSection>
    SwaptionVolCube4::smileSectionImpl(Time optionTime,
                                       Time swapLength) const {
        if (isAtmCalibrated_)
            return smileSection(optionTime, swapLength, denseParameters_);
        else
            return smileSection(optionTime, swapLength, sparseParameters_);
    }

	boost::shared_ptr<SmileSection>
    SwaptionVolCube4::smileSectionImpl(const Date& optionDate,
									   const Period& swapTenor) const {
        if (isAtmCalibrated_)
            return smileSection(optionDate, swapTenor, denseParameters_);
        else
            return smileSection(optionDate, swapTenor, sparseParameters_);
    }

    Matrix SwaptionVolCube4::sparseSabrRbsParameters() const {
        calculate();
        return sparseParameters_.browse();
    }

    Matrix SwaptionVolCube4::denseSabrRbsParameters() const {
        calculate();
        return denseParameters_.browse();
    }

    Matrix SwaptionVolCube4::marketVolCube() const {
        calculate();
        return marketVolCube_.browse();
    }
    Matrix SwaptionVolCube4::volCubeAtmCalibrated() const {
        calculate();
        return volCubeAtmCalibrated_.browse();
    }

	void SwaptionVolCube4::recalibration(const std::vector<Real>& beta,
                                         const Period& swapTenor){
	    
		QL_REQUIRE(beta.size()==nOptionTenors_,"Beta vector (" << beta.size() << ") must have equal length as number of option tenors (" << nOptionTenors_ << ")");
        
		/*Matrix newBetaGuess(nOptionTenors_, nSwapTenors_, 0.0);
		for(int i=0;i<nOptionTenors_;i++) {
			for(int j=0;j<nSwapTenors_;j++) {
				newBetaGuess[i][j] = beta[i];
			}
		}
        parametersGuess_.setLayer(1, newBetaGuess);*/

		const std::vector<Period>& swapTenors = marketVolCube_.swapTenors();
        Size k = std::find(swapTenors.begin(), swapTenors.end(),
                           swapTenor) - swapTenors.begin();
        QL_REQUIRE(k != swapTenors.size(), "swap tenor not found");
		for(int i=0;i<nOptionTenors_;i++) {
			parametersGuess_.setElement(1,i,k,beta[i]);
		}

        parametersGuess_.updateInterpolators();

        sabrRbsCalibrationSection(marketVolCube_,sparseParameters_,swapTenor);

        if(isAtmCalibrated_){
            fillVolatilityCube();
            sabrRbsCalibrationSection(volCubeAtmCalibrated_,denseParameters_,swapTenor);
        }
    }

	void SwaptionVolCube4::recalibrationNu(const std::vector<Real>& nu,
                                         const Period& swapTenor){
	    
		QL_REQUIRE(nu.size()==nOptionTenors_,"Nu vector (" << nu.size() << ") must have equal length as number of option tenors (" << nOptionTenors_ << ")");
        /*Matrix newNuGuess(nOptionTenors_, nSwapTenors_, 0.0);
		for(int i=0;i<nOptionTenors_;i++) {
			for(int j=0;j<nSwapTenors_;j++) {
				newNuGuess[i][j] = nu[i];
			}
		}
        parametersGuess_.setLayer(5, newNuGuess);*/
		
		const std::vector<Period>& swapTenors = marketVolCube_.swapTenors();
        Size k = std::find(swapTenors.begin(), swapTenors.end(),
                           swapTenor) - swapTenors.begin();
        QL_REQUIRE(k != swapTenors.size(), "swap tenor not found");
		for(int i=0;i<nOptionTenors_;i++) {
			parametersGuess_.setElement(5,i,k,nu[i]);
		}

        parametersGuess_.updateInterpolators();

        sabrRbsCalibrationSection(marketVolCube_,sparseParameters_,swapTenor);

        if(isAtmCalibrated_){
            fillVolatilityCube();
            sabrRbsCalibrationSection(volCubeAtmCalibrated_,denseParameters_,swapTenor);
        }
    }

	//======================================================================//
    //                      SwaptionVolCube4::Cube                          //
    //======================================================================//


    SwaptionVolCube4::Cube::Cube(
                                    const std::vector<Date>& optionDates,
                                    const std::vector<Period>& swapTenors,
                                    const std::vector<Time>& optionTimes,
                                    const std::vector<Time>& swapLengths,
                                    Size nLayers,
                                    bool extrapolation)
    : optionTimes_(optionTimes), swapLengths_(swapLengths),
      optionDates_(optionDates), swapTenors_(swapTenors),
      nLayers_(nLayers), extrapolation_(extrapolation) {

        QL_REQUIRE(optionTimes.size()>1,"Cube::Cube(...): optionTimes.size()<2");
        QL_REQUIRE(swapLengths.size()>1,"Cube::Cube(...): swapLengths.size()<2");

        QL_REQUIRE(optionTimes.size()==optionDates.size(),
                   "Cube::Cube(...): optionTimes/optionDates mismatch");
        QL_REQUIRE(swapTenors.size()==swapLengths.size(),
                   "Cube::Cube(...): swapTenors/swapLengths mismatch");

        std::vector<Matrix> points(nLayers_, Matrix(optionTimes_.size(),
                                                    swapLengths_.size(), 0.0));

        for (Size k=0;k<nLayers_;k++) {
            transposedPoints_.push_back(transpose(points[k]));

            boost::shared_ptr<Interpolation2D> interpolation (new
                BilinearInterpolation (optionTimes_.begin(), optionTimes_.end(),
                                       swapLengths_.begin(), swapLengths_.end(),
                                       transposedPoints_[k]));
            interpolators_.push_back(boost::shared_ptr<Interpolation2D>(
                new FlatExtrapolator2D(interpolation)));
            interpolators_[k]->enableExtrapolation();
        }
        setPoints(points);
     }

    SwaptionVolCube4::Cube::Cube(const Cube& o) {
        optionTimes_ = o.optionTimes_;
        swapLengths_ = o.swapLengths_;
        optionDates_ = o.optionDates_;
        swapTenors_ = o.swapTenors_;
        nLayers_ = o.nLayers_;
        extrapolation_ = o.extrapolation_;
        transposedPoints_ = o.transposedPoints_;
        for (Size k=0; k<nLayers_; ++k) {
            boost::shared_ptr<Interpolation2D> interpolation (
                new BilinearInterpolation (optionTimes_.begin(), optionTimes_.end(),
                                           swapLengths_.begin(), swapLengths_.end(),
                                           transposedPoints_[k]));
            interpolators_.push_back(boost::shared_ptr<Interpolation2D>(
                new FlatExtrapolator2D(interpolation)));
            interpolators_[k]->enableExtrapolation();
        }
        setPoints(o.points_);
    }

    SwaptionVolCube4::Cube&
    SwaptionVolCube4::Cube::operator=(const Cube& o) {
        optionTimes_ = o.optionTimes_;
        swapLengths_ = o.swapLengths_;
        optionDates_ = o.optionDates_;
        swapTenors_ = o.swapTenors_;
        nLayers_ = o.nLayers_;
        extrapolation_ = o.extrapolation_;
        transposedPoints_ = o.transposedPoints_;
        for(Size k=0;k<nLayers_;k++){
            boost::shared_ptr<Interpolation2D> interpolation (
                new BilinearInterpolation (optionTimes_.begin(), optionTimes_.end(),
                                           swapLengths_.begin(), swapLengths_.end(),
                                           transposedPoints_[k]));
            interpolators_.push_back(boost::shared_ptr<Interpolation2D>(
                new FlatExtrapolator2D(interpolation)));
            interpolators_[k]->enableExtrapolation();
        }
        setPoints(o.points_);
        return *this;
    }

    void SwaptionVolCube4::Cube::setElement(Size IndexOfLayer,
                                                        Size IndexOfRow,
                                                        Size IndexOfColumn,
                                                        Real x) {
        QL_REQUIRE(IndexOfLayer<nLayers_,
            "Cube::setElement: incompatible IndexOfLayer ");
        QL_REQUIRE(IndexOfRow<optionTimes_.size(),
            "Cube::setElement: incompatible IndexOfRow");
        QL_REQUIRE(IndexOfColumn<swapLengths_.size(),
            "Cube::setElement: incompatible IndexOfColumn");
        points_[IndexOfLayer][IndexOfRow][IndexOfColumn] = x;
    }

    void SwaptionVolCube4::Cube::setPoints(
                                               const std::vector<Matrix>& x) {
        QL_REQUIRE(x.size()==nLayers_,
            "Cube::setPoints: incompatible number of layers ");
        QL_REQUIRE(x[0].rows()==optionTimes_.size(),
            "Cube::setPoints: incompatible size 1");
        QL_REQUIRE(x[0].columns()==swapLengths_.size(),
            "Cube::setPoints: incompatible size 2");

        points_ = x;
    }

    void SwaptionVolCube4::Cube::setLayer(Size i,
                                                      const Matrix& x) {
        QL_REQUIRE(i<nLayers_,
            "Cube::setLayer: incompatible number of layer ");
        QL_REQUIRE(x.rows()==optionTimes_.size(),
            "Cube::setLayer: incompatible size 1");
        QL_REQUIRE(x.columns()==swapLengths_.size(),
            "Cube::setLayer: incompatible size 2");

        points_[i] = x;
    }

    void SwaptionVolCube4::Cube::setPoint(
                            const Date& optionDate, const Period& swapTenor,
                            const Real optionTime, const Time swapLength,
                            const std::vector<Real>& point)
    {
        const bool expandOptionTimes =
            !(std::binary_search(optionTimes_.begin(),optionTimes_.end(),optionTime));
        const bool expandSwapLengths =
            !(std::binary_search(swapLengths_.begin(),swapLengths_.end(),swapLength));

        std::vector<Real>::const_iterator optionTimesPreviousNode,
                                          swapLengthsPreviousNode;

        optionTimesPreviousNode =
            std::lower_bound(optionTimes_.begin(),optionTimes_.end(),optionTime);
        Size optionTimesIndex = optionTimesPreviousNode - optionTimes_.begin();

        swapLengthsPreviousNode =
            std::lower_bound(swapLengths_.begin(),swapLengths_.end(),swapLength);
        Size swapLengthsIndex = swapLengthsPreviousNode - swapLengths_.begin();

        if (expandOptionTimes || expandSwapLengths)
            expandLayers(optionTimesIndex, expandOptionTimes,
                         swapLengthsIndex, expandSwapLengths);

        for (Size k=0; k<nLayers_; ++k)
            points_[k][optionTimesIndex][swapLengthsIndex] = point[k];

        optionTimes_[optionTimesIndex] = optionTime;
        swapLengths_[swapLengthsIndex] = swapLength;
        optionDates_[optionTimesIndex] = optionDate;
        swapTenors_[swapLengthsIndex] = swapTenor;
    }

    void SwaptionVolCube4::Cube::expandLayers(
                                                 Size i, bool expandOptionTimes,
                                                 Size j, bool expandSwapLengths) {
        QL_REQUIRE(i<=optionTimes_.size(),"Cube::expandLayers: incompatible size 1");
        QL_REQUIRE(j<=swapLengths_.size(),"Cube::expandLayers: incompatible size 2");

        if (expandOptionTimes) {
            optionTimes_.insert(optionTimes_.begin()+i,0.);
            optionDates_.insert(optionDates_.begin()+i, Date());
        }
        if (expandSwapLengths) {
            swapLengths_.insert(swapLengths_.begin()+j,0.);
            swapTenors_.insert(swapTenors_.begin()+j, Period());
        }

        std::vector<Matrix> newPoints(nLayers_,Matrix(optionTimes_.size(),
                                                      swapLengths_.size(), 0.));

        for (Size k=0; k<nLayers_; ++k) {
            for (Size u=0; u<points_[k].rows(); ++u) {
                 Size indexOfRow = u;
                 if (u>=i && expandOptionTimes) indexOfRow = u+1;
                 for (Size v=0; v<points_[k].columns(); ++v) {
                      Size indexOfCol = v;
                      if (v>=j && expandSwapLengths) indexOfCol = v+1;
                      newPoints[k][indexOfRow][indexOfCol]=points_[k][u][v];
                 }
            }
        }
        setPoints(newPoints);
    }

    const std::vector<Matrix>&
    SwaptionVolCube4::Cube::points() const {
        return points_;
    }

    std::vector<Real> SwaptionVolCube4::Cube::operator()(
                            const Time optionTime, const Time swapLength) const {
        std::vector<Real> result;
        for (Size k=0; k<nLayers_; ++k)
            result.push_back(interpolators_[k]->operator()(optionTime, swapLength));
        return result;
    }

    const std::vector<Time>&
    SwaptionVolCube4::Cube::optionTimes() const {
        return optionTimes_;
    }

    const std::vector<Time>&
    SwaptionVolCube4::Cube::swapLengths() const {
        return swapLengths_;
    }

    void SwaptionVolCube4::Cube::updateInterpolators() const {
        for (Size k=0; k<nLayers_; ++k) {
            transposedPoints_[k] = transpose(points_[k]);
            boost::shared_ptr<Interpolation2D> interpolation (
                new BilinearInterpolation (optionTimes_.begin(), optionTimes_.end(),
                                           swapLengths_.begin(), swapLengths_.end(),
                                           transposedPoints_[k]));
            interpolators_[k] = boost::shared_ptr<Interpolation2D>(
                new FlatExtrapolator2D(interpolation));
            interpolators_[k]->enableExtrapolation();
        }
    }

    Matrix SwaptionVolCube4::Cube::browse() const {
        Matrix result(swapLengths_.size()*optionTimes_.size(), nLayers_+2, 0.0);
        for (Size i=0; i<swapLengths_.size(); ++i) {
            for (Size j=0; j<optionTimes_.size(); ++j) {
                result[i*optionTimes_.size()+j][0] = swapLengths_[i];
                result[i*optionTimes_.size()+j][1] = optionTimes_[j];
                for (Size k=0; k<nLayers_; ++k)
                    result[i*optionTimes_.size()+j][2+k] = points_[k][j][i];
            }
        }
        return result;
    }
  
}
