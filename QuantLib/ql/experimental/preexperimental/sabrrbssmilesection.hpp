/*! \file sabrrbssmilesection.hpp
    \brief Smile section implementation for hybrid sabr / rbs smile
*/

#ifndef quantlib_sabrrbs_smile_section_hpp
#define quantlib_sabrrbs_smile_section_hpp

#include <ql/termstructures/volatility/smilesection.hpp>
#include <sabrModel.hpp>
#include <rbsSmile.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <vector>

namespace QuantLib {

    class SabrRbsSmileSection : public SmileSection {
      public:
        SabrRbsSmileSection(Time timeToExpiry,
                         Rate forward,
						 double leftBound,
						 double rightBound,
                         const std::vector<Real>& sabrParameters,
						 const std::vector<Real>& rbsParameters);
        SabrRbsSmileSection(const Date& d,
                         Rate forward,
						 double leftBound,
						 double rightBound,
                         const std::vector<Real>& sabrParameters,
						 const std::vector<Real>& rbsParameters,
                         const DayCounter& dc = Actual365Fixed());
        Real minStrike () const { return 0.0; }
        Real maxStrike () const { return QL_MAX_REAL; }
        Real atmLevel() const { return forward_; }
      protected:
        //Real varianceImpl(Rate strike) const;
        Volatility volatilityImpl(Rate strike) const;
      private:
        Real alpha_, beta_, nu_, rho_, forward_;
		Real leftBound_,rightBound_;
		Real pmu_,pnu_,la_,lb_,lc_,ra_,rb_,rc_;
    };


}

#endif
