/*! \file moneynessinterpolation.hpp
    \brief linear interpolation between discrete points w.r.t. moneyness in x i.e. in log(x/f), f is assumed to be the central point of x values
*/

#ifndef quantlib_moneyness_interpolation_hpp
#define quantlib_moneyness_interpolation_hpp

#include <ql/math/interpolation.hpp>
#include <vector>

namespace QuantLib {

    namespace detail {

        template <class I1, class I2>
        class MoneynessInterpolationImpl
            : public Interpolation::templateImpl<I1,I2> {
		  
          public:
            MoneynessInterpolationImpl(const I1& xBegin, const I1& xEnd,
                                    const I2& yBegin)
            : Interpolation::templateImpl<I1,I2>(xBegin, xEnd, yBegin),
              s_(xEnd-xBegin) {}
            void update() {
				atm_=this->xBegin_[(Size(this->xEnd_-this->xBegin_)-1)/2];
				xmin_=this->xBegin_[0];
				ymin_=this->yBegin_[0];
				Size e=Size(this->xEnd_-this->xBegin_);
				for (Size i=1; i<e; ++i) {
                    Real dx = logSafe(this->xBegin_[i]/atm_)-logSafe(this->xBegin_[i-1]/atm_);
                    s_[i-1] = (this->yBegin_[i]-this->yBegin_[i-1])/dx;
                }
				xmax_=this->xBegin_[e-1];
				ymax_=this->yBegin_[e-1];
			}
            Real value(Real x) const {
                Size i = this->locate(x);
				//FILE *out = fopen("moneyness.log","a");
				//for(int j=0;j<Size(this->xEnd_-this->xBegin_);j++) {
				//	fprintf(out,"%d;%f;%f\n",j,this->xBegin_[j],this->yBegin_[j]);
				//}
				//for(int j=0;j<s_.size();j++) {
				//	fprintf(out,"%d;%f;%f\n",j,this->xBegin_[j],s_[j]);
				//}
				Real y;
				//fprintf(out,"*;*;*;*\n");
				if(x<xmin_ || x>xmax_) {
					double yv = x<xmin_? ymin_: ymax_;
					double xv = x<xmin_? xmin_: xmax_;
					y= yv + logSafe(x/xv)*s_[i];
					//y=yv + (x-xv)*s_[i]*logSafe(x/atm_); //murex doc
				//	fprintf(out,"e;%d;%f;%f\n",i,x,y);
				}
				else {
					y=(this->yBegin_[i]) + s_[i]*(logSafe(x/atm_)-logSafe(this->xBegin_[i]/atm_));
				//	fprintf(out,"e;%d;%f;%f\n",i,x,y);
				}
				//fclose(out);
                return y;
            }
            Real primitive(Real x) const {
                return 0.0; // not implemented!
            }
            Real derivative(Real x) const {
                return 0.0; // not implemented!
            }
            Real secondDerivative(Real) const {
                return 0.0; // not implemented!
            }

		 private:
			const double logSafe(double x) const { if(x<1.0E-15) x=1.0E-15; return log(x); }
            std::vector<Real> s_;
			double atm_,xmin_,xmax_,ymin_,ymax_;
          
        };

    }

    //! %Linear interpolation between discrete points
    class MoneynessInterpolation : public Interpolation {
      public:
        /*! \pre the \f$ x \f$ values must be sorted. */
        template <class I1, class I2>
        MoneynessInterpolation(const I1& xBegin, const I1& xEnd,
                            const I2& yBegin) {
            impl_ = boost::shared_ptr<Interpolation::Impl>(new
                detail::MoneynessInterpolationImpl<I1,I2>(xBegin, xEnd,
                                                       yBegin));
            impl_->update();
        }
    };

    //! %Linear-interpolation factory and traits
    class Moneyness {
      public:
        template <class I1, class I2>
        Interpolation interpolate(const I1& xBegin, const I1& xEnd,
                                  const I2& yBegin) const {
            return MoneynessInterpolation(xBegin, xEnd, yBegin);
        }
        static const bool global = false;
        static const Size requiredPoints = 2;
    };

}

#endif
