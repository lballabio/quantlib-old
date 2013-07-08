/**
   Copyright (C) 2008, 2009 Bojan Nikolic <bojan@bnikolic.co.uk>

   Simple bare-bones example of using QuantLib to price an FX option

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the license for more details.

   Comments welcome at bojan@bnikolic.co.uk
*/

#include <iostream>

#include <ql/quantlib.hpp>

struct OptionInputs
{
  QuantLib::Real S;
  QuantLib::Real K;
  /// Foreign rate
  QuantLib::Spread f;
  /// Domestic rate
  QuantLib::Rate r;
  QuantLib::Volatility vol;
  QuantLib::Date maturity;
  QuantLib::DayCounter dayCounter;
};

double
CRRPricing(QuantLib::VanillaOption &o,
		boost::shared_ptr<QuantLib::GarmanKohlagenProcess> process,
		QuantLib::Size timeSteps)
{
  using namespace QuantLib;

  boost::shared_ptr<PricingEngine> pe(new BinomialVanillaEngine<CoxRossRubinstein>(process,
																				timeSteps));
  o.setPricingEngine(pe);
  return o.NPV();
}

void FxOptEx(const OptionInputs &in,
		  const QuantLib::Date &todaysDate,
		  const QuantLib::Date &settlementDate)
{
  using namespace QuantLib;

  // set up dates
  Calendar calendar = TARGET();
  Settings::instance().evaluationDate() = todaysDate;

  boost::shared_ptr<Exercise>
	americanExercise(new AmericanExercise(settlementDate,
									   in.maturity));

  Handle<Quote>
	underlyingH(boost::shared_ptr<Quote>(new SimpleQuote(in.S)));

  Handle<YieldTermStructure>
	rTS(boost::shared_ptr<YieldTermStructure>(new FlatForward(settlementDate,
														   in.r,
														   in.dayCounter)));
  Handle<YieldTermStructure>
	fTS(boost::shared_ptr<YieldTermStructure>(new FlatForward(settlementDate,
														   in.f,
														   in.dayCounter)));
  Handle<BlackVolTermStructure>
	flatVolTS(boost::shared_ptr<BlackVolTermStructure>(new BlackConstantVol(settlementDate,
																		 calendar,
																		 in.vol,
																		 in.dayCounter)));

  boost::shared_ptr<StrikedTypePayoff>
	payoff(new PlainVanillaPayoff(Option::Call,
							   in.K));

  boost::shared_ptr<GarmanKohlagenProcess>
	process(new GarmanKohlagenProcess(underlyingH,
								   fTS,
								   rTS,
								   flatVolTS));

  VanillaOption amerOpt(payoff, americanExercise);

  double npv=CRRPricing(amerOpt,
					 process,
					 100);

  std::cout<<"Option value:"
		<<npv
		<<std::endl;
}


int main(int, char* [])
{
  using namespace QuantLib;

  OptionInputs in;
  in.S=78.2517;
  in.K=(in.S)*1.05;
  in.f=0.009;
  in.r=0.06;
  in.vol=0.40;
  in.maturity= Date(6, Apr, 2012); //Date(17, May, 1999);
  in.dayCounter = Actual365Fixed();

  FxOptEx(in,
	   Date(4, Jan, 2012),
	   Date(6, Jan, 2012));
	   //Date(15, May, 1998),
	   //Date(17, May, 1998));
}