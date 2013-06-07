#include <sabrrbssmilesection.hpp>

namespace QuantLib {

    SabrRbsSmileSection::SabrRbsSmileSection(Time timeToExpiry,
                         Rate forward,
						 double leftBound,
						 double rightBound,
                         const std::vector<Real>& sabrParams,
						 const std::vector<Real>& rbsParams)
    : SmileSection(timeToExpiry), forward_(forward), leftBound_(leftBound), rightBound_(rightBound) {

        alpha_ = sabrParams[0];
        beta_ = sabrParams[1];
        nu_ = sabrParams[2];
        rho_ = sabrParams[3];

		pmu_= rbsParams[0];
		pnu_= rbsParams[1];
		la_= rbsParams[2];
		lb_= rbsParams[3];
		lc_= rbsParams[4];
		ra_= rbsParams[5];
		rb_= rbsParams[6];
		rc_= rbsParams[7];

    }

    SabrRbsSmileSection::SabrRbsSmileSection(const Date& d,
                                       Rate forward,
									   double leftBound,
									   double rightBound,
                                       const std::vector<Real>& sabrParams,
									   const std::vector<Real>& rbsParams,
                                       const DayCounter& dc)
    : SmileSection(d, dc), forward_(forward), leftBound_(leftBound), rightBound_(rightBound) {

        alpha_ = sabrParams[0];
        beta_ = sabrParams[1];
        nu_ = sabrParams[2];
        rho_ = sabrParams[3];

		pmu_= rbsParams[0];
		pnu_= rbsParams[1];
		la_= rbsParams[2];
		lb_= rbsParams[3];
		lc_= rbsParams[4];
		ra_= rbsParams[5];
		rb_= rbsParams[6];
		rc_= rbsParams[7];

    }

     Real SabrRbsSmileSection::volatilityImpl(Rate strike) const {
		 if(strike >= leftBound_ && strike <= rightBound_) {
			SabrModel sabr(alpha_,beta_,nu_,rho_);
			return sabr.impliedVola(forward_,strike,exerciseTime());
		 }
		 else {
			RbsSmile rbs(leftBound_,rightBound_,pmu_,pnu_,la_,lb_,lc_,ra_,rb_,rc_,true); // boolean is a dummy!
			return rbs.impliedVola(forward_,strike,exerciseTime());
		 }
     }

}
