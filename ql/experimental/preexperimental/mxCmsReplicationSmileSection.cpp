#include <mxCmsReplicationSmileSection.hpp>



namespace QuantLib {

    MxCmsReplSmileSection::MxCmsReplSmileSection(const boost::shared_ptr<SmileSection> source, const std::vector<Real>& strikeSpreads, const Real slope, const Real inputCutoff, const Real flatCutoff)
		: source_(source), SmileSection(*source), strikeSpreads_(strikeSpreads), slope_(slope), inputCutoff_(inputCutoff), flatCutoff_(flatCutoff),
		  logMoneynessStrikes_(strikeSpreads_.size()) {

		f_ = source_->atmLevel();
		QL_REQUIRE(f_ != Null<Real>(), "Source Smile Section must provide an atm level");
		QL_REQUIRE(strikeSpreads[0] <= 0.0, "Strike Spreads must start with non positve number");

		Size lastIdx=0;
		for(Size i=0; i < strikeSpreads.size(); i++) {
			if(i>0) QL_REQUIRE(strikeSpreads[i]>strikeSpreads[i-1],"Strike Spreads must be strictly increasing: " << strikeSpreads[i-1] << " >= " << strikeSpreads[i]);
			Real strike = f_ + strikeSpreads_[i];
			Real lm = std::log( strike / f_ ) ;
			if(lm <= 1.0) {
				lastIdx=i;
				logMoneynessStrikes_.push_back(lm);
				vols_.push_back(source_->volatility(strike));
			}
			else {
				if(lastIdx < 1) {
					lastIdx=1;
					logMoneynessStrikes_.push_back(1.0);
					vols_.push_back(source_->volatility(f_*exp(1.0)));
				}
			}
		}

		QL_REQUIRE(lastIdx >= 1, "MxCmsReplSmileSection: At least two points with log moneyness less or equal 1 must be given");

		// adjust the last point to match slope

		vols_[lastIdx] = vols_[lastIdx-1] + (logMoneynessStrikes_[lastIdx]-logMoneynessStrikes_[lastIdx-1]) * slope_;

		volI_ = boost::shared_ptr<Interpolation>(new LinearInterpolation(logMoneynessStrikes_.begin(),logMoneynessStrikes_.end(),vols_.begin()));

    }

    Real MxCmsReplSmileSection::volatilityImpl(Rate strike) const {
		return volI_->operator()( log( strike / f_ ) );
    }




}
