
/*
 Copyright (C) 2004 StatPro Italia srl

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

#ifndef quantlib_diffusion_process_i
#define quantlib_diffusion_process_i

%include termstructures.i
%include volatilities.i

%{
using QuantLib::DiffusionProcess;
%}

%ignore DiffusionProcess;
class DiffusionProcess {};

%template(DiffusionProcess) boost::shared_ptr<DiffusionProcess>;

%{
using QuantLib::BlackScholesProcess;
typedef boost::shared_ptr<DiffusionProcess> BlackScholesProcessPtr;
%}

%rename(BlackScholesProcess) BlackScholesProcessPtr;
class BlackScholesProcessPtr 
    : public boost::shared_ptr<DiffusionProcess> {
  public:
    %extend {
      BlackScholesProcessPtr(
                       const RelinkableHandle<TermStructure>& riskFreeTS,
                       const RelinkableHandle<TermStructure>& dividendTS,
                       const RelinkableHandle<BlackVolTermStructure>& volTS,
                       double s0) {
          return new BlackScholesProcessPtr(
                              new BlackScholesProcess(riskFreeTS,
                                                      dividendTS,
                                                      volTS,
                                                      s0));
      }
    }
};


// allow use of diffusion process vectors
namespace std {
    %template(DiffusionProcessVector) 
        vector<boost::shared_ptr<DiffusionProcess> >;
}


#endif
