
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

%template(StochasticProcess) boost::shared_ptr<StochasticProcess>;

%{
using QuantLib::BlackScholesStochasticProcess;
using QuantLib::Merton76StochasticProcess;
typedef boost::shared_ptr<StochasticProcess> BlackScholesStochasticProcessPtr;
typedef boost::shared_ptr<StochasticProcess> Merton76StochasticProcessPtr;
%}

%rename(BlackScholesStochasticProcess) BlackScholesStochasticProcessPtr;
class BlackScholesStochasticProcessPtr 
    : public boost::shared_ptr<StochasticProcess> {
  public:
    %extend {
      BlackScholesStochasticProcessPtr(
                       const RelinkableHandle<Quote>& stateVariable,
                       const RelinkableHandle<TermStructure>& dividendTS,
                       const RelinkableHandle<TermStructure>& riskFreeTS,
                       const RelinkableHandle<BlackVolTermStructure>& volTS) {
          return new BlackScholesStochasticProcessPtr(
                              new BlackScholesStochasticProcess(stateVariable,
                                                                dividendTS,
                                                                riskFreeTS,
                                                                volTS));
      }
    }
};

%rename(Merton76StochasticProcess) Merton76StochasticProcessPtr;
class Merton76StochasticProcessPtr 
    : public boost::shared_ptr<StochasticProcess> {
  public:
    %extend {
      Merton76StochasticProcessPtr(
                       const RelinkableHandle<Quote>& stateVariable,
                       const RelinkableHandle<TermStructure>& dividendTS,
                       const RelinkableHandle<TermStructure>& riskFreeTS,
                       const RelinkableHandle<BlackVolTermStructure>& volTS,
                       const RelinkableHandle<Quote>& jumpIntensity, 
                       const RelinkableHandle<Quote>& meanLogJump,
                       const RelinkableHandle<Quote>& jumpVolatility) {
            return new Merton76StochasticProcessPtr(
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
