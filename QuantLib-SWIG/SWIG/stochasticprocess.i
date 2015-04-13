
/*
 Copyright (C) 2004, 2005, 2007, 2008 StatPro Italia srl
 Copyright (C) 2010 Klaus Spanderen
 Copyright (C) 2015 Matthias Groncki

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef quantlib_stochastic_process_i
#define quantlib_stochastic_process_i

%include marketelements.i
%include termstructures.i
%include volatilities.i

%{
using QuantLib::StochasticProcess;
%}

%ignore StochasticProcess;
class StochasticProcess {};
%template(StochasticProcess) boost::shared_ptr<StochasticProcess>;
IsObservable(boost::shared_ptr<StochasticProcess>);

%{
using QuantLib::StochasticProcess1D;
typedef boost::shared_ptr<StochasticProcess> StochasticProcess1DPtr;
%}

%rename(StochasticProcess1D) StochasticProcess1DPtr;
class StochasticProcess1DPtr
    : public boost::shared_ptr<StochasticProcess> {
  public:
    %extend {
      Real x0() {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)->x0();
      }
      Real drift(Time t, Real x) {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->drift(t, x);
      }
      Real diffusion(Time t, Real x) {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->diffusion(t, x);
      }
      Real expectation(Time t0, Real x0, Time dt) {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->expectation(t0, x0, dt);
      }
      Real stdDeviation(Time t0, Real x0, Time dt) {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->stdDeviation(t0, x0, dt);
      }
      Real variance(Time t0, Real x0, Time dt)  {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->variance(t0, x0, dt);
      }
      Real evolve(Time t0, Real x0, Time dt, Real dw) {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->evolve(t0, x0, dt, dw);
      }
      Real apply(Real x0, Real dx)  {
          return boost::dynamic_pointer_cast<StochasticProcess1D>(*self)
              ->apply(x0, dx);
      }
    }
  private:
    StochasticProcess1DPtr();
};


%{
using QuantLib::GeneralizedBlackScholesProcess;
typedef boost::shared_ptr<StochasticProcess> GeneralizedBlackScholesProcessPtr;
%}

%rename(GeneralizedBlackScholesProcess) GeneralizedBlackScholesProcessPtr;
class GeneralizedBlackScholesProcessPtr : public StochasticProcess1DPtr {
  public:
    %extend {
      GeneralizedBlackScholesProcessPtr(
                             const Handle<Quote>& s0,
                             const Handle<YieldTermStructure>& dividendTS,
                             const Handle<YieldTermStructure>& riskFreeTS,
                             const Handle<BlackVolTermStructure>& volTS) {
          return new GeneralizedBlackScholesProcessPtr(
                       new GeneralizedBlackScholesProcess(s0, dividendTS,
                                                          riskFreeTS, volTS));
      }
      Handle<Quote> stateVariable() {
          return boost::dynamic_pointer_cast<
                      GeneralizedBlackScholesProcess>(*self)->stateVariable();
      }
      Handle<YieldTermStructure> dividendYield() {
          return boost::dynamic_pointer_cast<
                      GeneralizedBlackScholesProcess>(*self)->dividendYield();
      }
      Handle<YieldTermStructure> riskFreeRate() {
          return boost::dynamic_pointer_cast<
                      GeneralizedBlackScholesProcess>(*self)->riskFreeRate();
      }
      Handle<BlackVolTermStructure> blackVolatility() const {
          return boost::dynamic_pointer_cast<
                      GeneralizedBlackScholesProcess>(*self)->blackVolatility();
      }
    }
};

%{
using QuantLib::BlackScholesProcess;
typedef boost::shared_ptr<StochasticProcess> BlackScholesProcessPtr;
%}

%rename(BlackScholesProcess) BlackScholesProcessPtr;
class BlackScholesProcessPtr : public GeneralizedBlackScholesProcessPtr {
  public:
    %extend {
      BlackScholesProcessPtr(const Handle<Quote>& s0,
                               const Handle<YieldTermStructure>& riskFreeTS,
                               const Handle<BlackVolTermStructure>& volTS) {
          return new BlackScholesProcessPtr(
                            new BlackScholesProcess(s0, riskFreeTS, volTS));
      }
    }
};

%{
using QuantLib::BlackScholesMertonProcess;
typedef boost::shared_ptr<StochasticProcess> BlackScholesMertonProcessPtr;
%}

%rename(BlackScholesMertonProcess) BlackScholesMertonProcessPtr;
class BlackScholesMertonProcessPtr : public GeneralizedBlackScholesProcessPtr {
  public:
    %extend {
      BlackScholesMertonProcessPtr(
                             const Handle<Quote>& s0,
                             const Handle<YieldTermStructure>& dividendTS,
                             const Handle<YieldTermStructure>& riskFreeTS,
                             const Handle<BlackVolTermStructure>& volTS) {
          return new BlackScholesMertonProcessPtr(
                            new BlackScholesMertonProcess(s0, dividendTS,
                                                          riskFreeTS, volTS));
      }
    }
};

%{
using QuantLib::BlackProcess;
typedef boost::shared_ptr<StochasticProcess> BlackProcessPtr;
%}

%rename(BlackProcess) BlackProcessPtr;
class BlackProcessPtr : public GeneralizedBlackScholesProcessPtr {
  public:
    %extend {
      BlackProcessPtr(const Handle<Quote>& s0,
                      const Handle<YieldTermStructure>& riskFreeTS,
                      const Handle<BlackVolTermStructure>& volTS) {
          return new BlackProcessPtr(new BlackProcess(s0, riskFreeTS, volTS));
      }
    }
};

%{
using QuantLib::GarmanKohlagenProcess;
typedef boost::shared_ptr<StochasticProcess> GarmanKohlagenProcessPtr;
%}

%rename(GarmanKohlagenProcess) GarmanKohlagenProcessPtr;
class GarmanKohlagenProcessPtr : public GeneralizedBlackScholesProcessPtr {
  public:
    %extend {
      GarmanKohlagenProcessPtr(
                         const Handle<Quote>& s0,
                         const Handle<YieldTermStructure>& foreignRiskFreeTS,
                         const Handle<YieldTermStructure>& domesticRiskFreeTS,
                         const Handle<BlackVolTermStructure>& volTS) {
          return new GarmanKohlagenProcessPtr(
                        new GarmanKohlagenProcess(s0, foreignRiskFreeTS,
                                                  domesticRiskFreeTS, volTS));
      }
    }
};



%{
using QuantLib::Merton76Process;
typedef boost::shared_ptr<StochasticProcess> Merton76ProcessPtr;
%}

%rename(Merton76Process) Merton76ProcessPtr;
class Merton76ProcessPtr : public StochasticProcess1DPtr {
  public:
    %extend {
      Merton76ProcessPtr(const Handle<Quote>& stateVariable,
                         const Handle<YieldTermStructure>& dividendTS,
                         const Handle<YieldTermStructure>& riskFreeTS,
                         const Handle<BlackVolTermStructure>& volTS,
                         const Handle<Quote>& jumpIntensity,
                         const Handle<Quote>& meanLogJump,
                         const Handle<Quote>& jumpVolatility) {
            return new Merton76ProcessPtr(
                              new Merton76Process(stateVariable, dividendTS,
                                                  riskFreeTS, volTS,
                                                  jumpIntensity, meanLogJump,
                                                  jumpVolatility));
      }
    }
};

%{
using QuantLib::StochasticProcessArray;
typedef boost::shared_ptr<StochasticProcess> StochasticProcessArrayPtr;
%}

%rename(StochasticProcessArray) StochasticProcessArrayPtr;
class StochasticProcessArrayPtr : public boost::shared_ptr<StochasticProcess> {
  public:
    %extend {
      StochasticProcessArrayPtr(
               const std::vector<boost::shared_ptr<StochasticProcess> >&array,
               const Matrix &correlation) {
          std::vector<boost::shared_ptr<StochasticProcess1D> > in_array;
          for (Size j=0; j < array.size(); j++)
              in_array.push_back(
                  boost::dynamic_pointer_cast<StochasticProcess1D>(array[j]));
          return new StochasticProcessArrayPtr(
                           new StochasticProcessArray(in_array, correlation));
      }
    }
};


%{
using QuantLib::GeometricBrownianMotionProcess;
typedef boost::shared_ptr<StochasticProcess> GeometricBrownianMotionProcessPtr;
%}

%rename(GeometricBrownianMotionProcess) GeometricBrownianMotionProcessPtr;
class GeometricBrownianMotionProcessPtr : public StochasticProcess1DPtr {
  public:
    %extend {
      GeometricBrownianMotionProcessPtr(Real initialValue,
                                        Real mu,
                                        Real sigma) {
          return new GeometricBrownianMotionProcessPtr(
                 new GeometricBrownianMotionProcess(initialValue, mu, sigma));
      }
    }
};

%{
using QuantLib::VarianceGammaProcess;
typedef boost::shared_ptr<StochasticProcess> VarianceGammaProcessPtr;
%}

%rename(VarianceGammaProcess) VarianceGammaProcessPtr;
class VarianceGammaProcessPtr : public StochasticProcess1DPtr {
  public:
    %extend {
      VarianceGammaProcessPtr(const Handle<Quote>& s0,
            const Handle<YieldTermStructure>& dividendYield,
            const Handle<YieldTermStructure>& riskFreeRate,
            Real sigma, Real nu, Real theta) {
          return new VarianceGammaProcessPtr(
                 new VarianceGammaProcess(s0,dividendYield,riskFreeRate,sigma,nu,theta));
      }
    }
};


%{
using QuantLib::HestonProcess;
typedef boost::shared_ptr<StochasticProcess> HestonProcessPtr;
%}

%rename(HestonProcess) HestonProcessPtr;
class HestonProcessPtr : public boost::shared_ptr<StochasticProcess> {
  public:
    %extend {
      HestonProcessPtr(const Handle<YieldTermStructure>& riskFreeTS,
					   const Handle<YieldTermStructure>& dividendTS,
					   const Handle<Quote>& s0,
					   Real v0, Real kappa,
                       Real theta, Real sigma, Real rho) {
		return new HestonProcessPtr(
			new HestonProcess(riskFreeTS, dividendTS, s0, v0, 
						      kappa, theta, sigma, rho));	
      }
                       
      Handle<Quote> s0() {
          return boost::dynamic_pointer_cast<
                      HestonProcess>(*self)->s0();
      }
      Handle<YieldTermStructure> dividendYield() {
          return boost::dynamic_pointer_cast<
                      HestonProcess>(*self)->dividendYield();
      }
      Handle<YieldTermStructure> riskFreeRate() {
          return boost::dynamic_pointer_cast<
                      HestonProcess>(*self)->riskFreeRate();
      }
    }
};

%{
using QuantLib::BatesProcess;
typedef boost::shared_ptr<StochasticProcess> BatesProcessPtr;
%}

%rename(BatesProcess) BatesProcessPtr;
class BatesProcessPtr : public HestonProcessPtr {
  public:
    %extend {
      BatesProcessPtr(const Handle<YieldTermStructure>& riskFreeRate,
                      const Handle<YieldTermStructure>& dividendYield,
                      const Handle<Quote>& s0,
                      Real v0, Real kappa,
                      Real theta, Real sigma, Real rho,
                      Real lambda, Real nu, Real delta) {
		return new BatesProcessPtr(
			new BatesProcess(riskFreeRate, dividendYield, s0, v0, 
							 kappa, theta, sigma, rho, 
							 lambda, nu, delta));
	  }
    }
};

%{
using QuantLib::HullWhiteProcess;
typedef boost::shared_ptr<StochasticProcess> HullWhiteProcessPtr;
%}

%rename(HullWhiteProcess) HullWhiteProcessPtr;
class HullWhiteProcessPtr : public StochasticProcess1DPtr {
  public:
    %extend {
      HullWhiteProcessPtr(const Handle<YieldTermStructure>& riskFreeTS,
                          Real a, Real sigma) {
            return new HullWhiteProcessPtr(new HullWhiteProcess(riskFreeTS, a, sigma));	
        }
    }
};

%{
using QuantLib::GsrProcess;
typedef boost::shared_ptr<StochasticProcess> GsrProcessPtr;
%}

%rename(GsrProcess) GsrProcessPtr;
class GsrProcessPtr : public StochasticProcess1DPtr {
    public:
    %extend {
        GsrProcessPtr(const Array &times, const Array &vols,
                   const Array &reversions, const Real T = 60.0) {
            return new GsrProcessPtr(new GsrProcess(times, vols, reversions, T));
        }
        Real sigma(Time t) {
            return boost::dynamic_pointer_cast<GsrProcess>(*self)->sigma(t);
        }
        Real reversion(Time t){
            return boost::dynamic_pointer_cast<GsrProcess>(*self)->reversion(t);
        }
        Real y(Time t) {
            return boost::dynamic_pointer_cast<GsrProcess>(*self)->y(t);
        }
        Real G(Time t, Time T, Real x){
            return boost::dynamic_pointer_cast<GsrProcess>(*self)->G(t, T, x);
        }
        void setForwardMeasureTime(Time t) {
            boost::dynamic_pointer_cast<GsrProcess>(*self)->setForwardMeasureTime(t);
        }
    }
};

%inline %{
    GsrProcessPtr as_gsr_process(
                           const boost::shared_ptr<StochasticProcess>& proc) {
        return boost::dynamic_pointer_cast<GsrProcess>(proc);
    }
%}

// allow use of diffusion process vectors
#if defined(SWIGCSHARP)
SWIG_STD_VECTOR_ENHANCED( boost::shared_ptr<StochasticProcess> )
#endif
%template(StochasticProcessVector)
std::vector<boost::shared_ptr<StochasticProcess> >;

typedef std::vector<boost::shared_ptr<StochasticProcess> >
StochasticProcessVector;


#endif
