
/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2005 Dominic Thuillier

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

#ifndef quantlib_basket_options_i
#define quantlib_basket_options_i

%include date.i
%include options.i
%include payoffs.i
%{
using QuantLib::BasketOption;
using QuantLib::BasketOptionType;
using QuantLib::MinBasketOptionType;
using QuantLib::MaxBasketOptionType;
using QuantLib::AverageBasketOptionType;
typedef boost::shared_ptr<Instrument> BasketOptionPtr;
typedef boost::shared_ptr<BasketOptionType> BasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> MinBasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> MaxBasketOptionTypePtr;
typedef boost::shared_ptr<BasketOptionType> AverageBasketOptionTypePtr;
%}

%template(BasketOptionType) boost::shared_ptr<BasketOptionType>;

%rename(MinBasketOptionType) MinBasketOptionTypePtr;
class MinBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        MinBasketOptionTypePtr() {
            return new MinBasketOptionTypePtr(new MinBasketOptionType);
        }
    }
};

%rename(MaxBasketOptionType) MaxBasketOptionTypePtr;
class MaxBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        MaxBasketOptionTypePtr() {
            return new MaxBasketOptionTypePtr(new MaxBasketOptionType);
        }
    }
};

%rename(AverageBasketOptionType) AverageBasketOptionTypePtr;
class AverageBasketOptionTypePtr : 
      public boost::shared_ptr<BasketOptionType>  {
  public:
    %extend {
        AverageBasketOptionTypePtr(const Array &a) {
            return new AverageBasketOptionTypePtr(new 
	    AverageBasketOptionType(a));
        }
        AverageBasketOptionTypePtr(Size n) {
            return new AverageBasketOptionTypePtr(new 
	    AverageBasketOptionType(n));
        }
    }
};


%rename(BasketOption) BasketOptionPtr;
class BasketOptionPtr : public MultiAssetOptionPtr {
  public:
    %extend {
        BasketOptionPtr(
	        const boost::shared_ptr<BasketOptionType>& btype,
                const boost::shared_ptr<StochasticProcess>& process,
                const boost::shared_ptr<Payoff>& payoff,
                const boost::shared_ptr<Exercise>& exercise,
                const boost::shared_ptr<PricingEngine>& engine
                   = boost::shared_ptr<PricingEngine>()) {
            boost::shared_ptr<PlainVanillaPayoff> stPayoff =
                 boost::dynamic_pointer_cast<PlainVanillaPayoff>(payoff);
            QL_REQUIRE(stPayoff, "wrong payoff given");
            return new BasketOptionPtr(
                        new BasketOption(btype,
			process,stPayoff,exercise,
			engine));
        }
    }
};

#endif
