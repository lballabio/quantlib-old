
/*
 Copyright (C) 2003 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

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

%template(StochasticProcess) Handle<StochasticProcess>;

%{
using QuantLib::BlackScholesStochasticProcess;
using QuantLib::Merton76StochasticProcess;
typedef Handle<StochasticProcess> BlackScholesStochasticProcessHandle;
typedef Handle<StochasticProcess> Merton76StochasticProcessHandle;
%}

%rename(BlackScholesStochasticProcess) BlackScholesStochasticProcessHandle;
class BlackScholesStochasticProcessHandle : public Handle<StochasticProcess> {
  public:
    %extend {
      BlackScholesStochasticProcessHandle(
                       const RelinkableHandle<Quote>& stateVariable,
                       const RelinkableHandle<TermStructure>& dividendTS,
                       const RelinkableHandle<TermStructure>& riskFreeTS,
                       const RelinkableHandle<BlackVolTermStructure>& volTS) {
          return new BlackScholesStochasticProcessHandle(
                              new BlackScholesStochasticProcess(stateVariable,
                                                                dividendTS,
                                                                riskFreeTS,
                                                                volTS));
      }
    }
};

%rename(Merton76StochasticProcess) Merton76StochasticProcessHandle;
class Merton76StochasticProcessHandle : public Handle<StochasticProcess> {
  public:
    %extend {
      Merton76StochasticProcessHandle(
                       const RelinkableHandle<Quote>& stateVariable,
                       const RelinkableHandle<TermStructure>& dividendTS,
                       const RelinkableHandle<TermStructure>& riskFreeTS,
                       const RelinkableHandle<BlackVolTermStructure>& volTS,
                       double jumpIntensity, double meanLogJump,
                       double jumpVolatility) {
            return new Merton76StochasticProcessHandle(
                              new Merton76StochasticProcess(stateVariable,
                                                            dividendTS,
                                                            riskFreeTS,
                                                            volTS,
                                                            jumpIntensity,
                                                            meanLogJump,
                                                            jumpVolatility));
      }
    }
};


#endif
