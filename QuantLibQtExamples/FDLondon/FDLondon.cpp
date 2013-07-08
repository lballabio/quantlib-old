/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007, 2009 StatPro Italia srl

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

#ifdef BOOST_MSVC
/* Uncomment the following lines to unmask floating-point
 exceptions. Warning: unpredictable results can arise...

 See http://www.wilmott.com/messageview.cfm?catid=10&threadid=9481
 Is there anyone with a definitive word about this?
 */
// #include <float.h>
// namespace { unsigned int u = _controlfp(_EM_INEXACT, _MCW_EM); }
#endif

#include "customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

//using namespace QuantLib;
using namespace std;
using boost::shared_ptr;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

	Integer sessionId() {return 0;}

}
#endif

//****************************************************************************

// Generic finite difference model
class FiniteDifferenceMethod {
public:
    // constructor
    FiniteDifferenceMethod() {}
    //virtual ∼FiniteDifferenceMethod() {}

	enum MethodType {
		ExplicitDifference,
		ImplicitDifference,
		AlternatingDirectImplicit,
		SOR,
		ProjectedSOR,
		CrankNicolson
	};
	enum BoundaryConditions {
		Dirichlet, Neumann
	};
    virtual void solve() const {}

};

//****************************************************************************

class ExplicitDiffMethod: public FiniteDifferenceMethod {
public:
    ExplicitDiffMethod() {}
    ExplicitDiffMethod(long N, long M);
    //∼ExplicitDiffMethod() {}

    virtual void solve() {}
	double explicitDiffEuropean(double price, double strike, double rate,
			double div, double volatility, double T, int N, int M, char type,
			char bc);
	double explicitDiffAmerican(double price, double strike, double rate,
			double div, double volatility, double T, int N, int M, char type,
			char bc);
private:
	long N_; // number of time steps
	long M_; // number of space steps
};

/**********************************************************************************
 explicitDiffAmerican : values an American option using the explicit difference
 method
 [in]: double price : asset price
 double strike : strike price
 double vol : volatility
 double rate : risk-free rate
 double div : dividend yield
 double T : time to maturity
 int N : number of time steps
 int M : number of space steps
 char type : (C)all or (P)ut
 char bc : boundary conditions (D)irichlet or (N)eumann
 [out] double : option price
 **********************************************************************************/
double ExplicitDiffMethod::explicitDiffAmerican(double price, double strike,
		double vol, double rate, double div, double T, int N, int M, char type,
		char bc) {
	int i, j;
	double dt = T / N;
	double drift = rate - div - 0.5 * (vol * vol);
	double dx = vol * sqrt(3 * dt / 2);
	double pu, pm, pd;
	double C[150][150] = { 0.0 }; // stores option prices
	double S[150][150] = { 0.0 }; // stores asset prices
	pu = (vol * vol * dt) / (2 * dx * dx) + (drift * dt) / (2 * dx);
	pm = 1.0 - (vol * vol * dt) / (dx * dx);
	pd = (vol * vol * dt) / (2 * dx * dx) - (drift * dt) / (2 * dx);

    // initialize asset prices at maturity
	for (j = -M; j <= M; j++) {
		S[N][j] = price * exp(j * dx);
	}
    if (type == 'C') {
        // compute payoff at maturity
        for (j = -M; j <= M; j++) {
            C[N][j] = max(S[N][j] - strike, 0.0);
        }

        // boundary conditions for high and low asset prices
		for (i = 0; i < N; i++) {
            if (bc == 'D') {
				C[i][-M] = 0.0;
                C[i][M] = max(S[N][M] - strike, 0.0);
			} else {
				C[i][M] = C[i][M - 1] + (S[i][M] - S[i][M - 1]);
				C[i][-M] = C[i][-M + 1];
			}
		}
		for (i = N - 1; i >= 0; i--) {
			for (j = M - 1; j >= -(M - 1); j--) {
				C[i][j] = pu * C[i + 1][j + 1] + pm * C[i + 1][j]
						+ pd * C[i + 1][j - 1];
                C[i][j] = max(S[N][j] - strike, C[i][j]);
			}
		}
	} else //if (type == ‘P’)
	{
        // boundary conditions for high and low asset prices
		for (i = 0; i < N; i++) {
			C[i][0] = strike;
			C[i][M] = 0;
		}
		for (j = -M; j <= M; j++) {
            C[N][j] = max(strike - S[N][j], 0.0);
		}
		for (j = -M; j <= M; j++) {
            C[N][j] = max(strike - S[N][j], 0.0);
		}
        // boundary conditions for high and low asset prices
		for (i = 0; i < N; i++) {
            if (bc == 'D') {
				C[i][-M] = strike;
                C[i][M] = max(0.0, strike - S[N][j]);
			} else // Neumann bc
			{
				C[i][M] = C[i][M - 1];
				C[i][-M] = C[i][-M + 1] + (S[i][-M] - S[i][-M + 1]);
			}
		}
		for (i = N - 1; i >= 0; i--) {
			for (j = M - 1; j >= -M; j--) {
				C[i][j] = pu * C[i + 1][j + 1] + pm * C[i + 1][j]
						+ pd * C[i + 1][j - 1];
				C[i][j] = max(strike - S[N][j], C[i][j]);
			}
		}
	}
	return C[0][0];
}

//****************************************************************************

class ImplicitDiffMethod: public FiniteDifferenceMethod {
public:
    ImplicitDiffMethod() {}
    ImplicitDiffMethod(long N, long M);
    //∼ImplicitDiffMethod() {}
	double implicitDiffEuropean(double price, double strike, double vol,
			double rate, double div, double T, long N, long M, char type,
			char bc);
	double implicitDiffAmerican(double price, double strike, double vol,
			double rate, double div, double T, long N, long M, char type,
			char bc);
	void solveTridiagonalAmer(double strike, long N, long M, double pu,
			double pm, double pd, double *d, double *c1, double *d1, char type);
	void solveTridiagonalEuro(double strike, long N, long M, double pu,
			double pm, double pd, double *d, double *c1, double *d1, char type);
private:
	long N_; // number of time steps
	long M_; // number of state steps
	double C[200][200]; // stores option prices
	double S[200][200]; // stores asset prices
};

/**********************************************************************************
 implicitDiffAmerican: values an American option using the implicit difference
 method
 [in]: double price : asset price
 double strike : strike price
 double vol : volatility
 double rate : risk-free rate
 double div : dividend yield
 double T : maturity
 int N : number of time steps
 int M : number of space steps
 char type: (C)all or (P)ut
 char bc: boundary conditions (D)irichlet or (N)eumann
 [out]: option price
 **********************************************************************************/
double ImplicitDiffMethod::implicitDiffAmerican(double price, double strike,
		double vol, double rate, double div, double T, long N, long M,
		char type, char bc) {
	double c1[350] = { 0.0 }; // array to store values in
                              // tridiagonal system
	double d[350] = { 0.0 };
	double d1[350] = { 0.0 };
	double x[350] = { 0.0 };
	double dx = 0.0; // space size
	double drift = rate - div - vol * vol / 2; // drift
	double pu, pm, pd; // risk neutral probabilities
	int i, j;

    double dt = T / N;
	dx = vol * sqrt(3 * dt / 2);

    pu = -0.5 * dt * ((vol * vol) / (dx * dx) + drift / dx);
	pm = 1 + dt * ((vol * vol) / (dx * dx)) + rate * dt;
	pd = -0.5 * dt * ((vol * vol) / (dx * dx) - drift / dx);

    for (j = -M; j <= M; j++) {
		S[N][j] = price * exp(j * dx);
		S[0][j] = price;
	}
	for (i = 1; i < N; i++) {
		for (j = -M; j <= M; j++) {
			S[i][j] = S[i - 1][j] * exp(j * dx);
        }
	}

    // calculate payoffs
    if (type == 'P') {
		for (j = -M; j <= M; j++) {
            C[N][j] = max(strike - S[N][j], 0.0);
		}
        // calculate boundary conditions
		for (i = 0; i < N; i++) {
            if (bc == 'D') // Dirichlet boundary conditions
					{
				C[i][-M] = strike;
                C[i][M] = max(strike - S[i][M], 0.0);
			} else // Neumann boundary conditions
			{
				C[i][-M] = C[i][-M + 1] + (S[i][-M] - S[i][-M + 1]);
				C[i][M] = C[i][M - 1];
			}
		}
    } else // if type == 'C'
	{
        // calculate boundary condition at maturity
		for (j = -M; j <= M; j++) {
            C[N][j] = max(S[N][j] - strike, 0.0);
		}
        // calculate boundary conditions on grid
		for (i = 0; i < N; i++) {
            if (bc == 'D') // Dirichlet boundary conditions
					{
				C[i][-M] = 0;
                C[i][M] = max(S[i][M] - strike, 0.0);
			} else // Neumann boundary condition
			{
				C[i][-M] = C[i][-M + 1];
				C[i][M] = C[i][M - 1] + (S[i][M] - S[i][M - 1]);
			}
		}
	}
	solveTridiagonalAmer(strike, N, M, pu, pm, pd, d, c1, d1, type);
	return C[0][1];
}

/*********************************************************************************
solveTridiagonalAmer: solves a tridiagonal system with American exercise conditions
[in]: double strike : strike price
long N : number of time steps
long M : number of space steps
double pu : up probability
double pm : middle probability
double pd : down probability
double *d, *c1, *d1: stores elements of tridiagonal matrix
char type : (C)all or (P)ut
[out]: option price
**********************************************************************************/
void ImplicitDiffMethod::solveTridiagonalAmer(double strike, long N, long M, double
		pu, double pm, double pd, double *d, double *c1, double *d1, char type)
{
	int i,j;
	for (j = -M; j <= M; j++)
        d[j] = C[N][j];

	d1[-M] = d[-M]/pm;
	c1[-M] = pd/pm;
	c1[-M+1] = pd/pm;
	d1[-M+1] = d[-M+1]/pm;

	for (j = -M+1; j <= M-2; j++)
        c1[j+1] = pd/(pm - pu*c1[j]);

	for (j = -M+1; j <= M-1; j++)
        d1[j+1] = (d[j+1] - pu*d1[j])/(pm - pu*c1[j]);

	for (i = N-1; i >= 0; i--)
	{
		for (j = -M+1; j <= M-1; j++)
		{
			if (i != N-1)
                d[j] = C[i+1][-j];
			if (j == -M+1)
			{
				d1[-M+1] = d[-M+1]/pm;
			}
			d1[j+1] = (d[j+1] - pu*d1[j])/(pm - pu*c1[j]);
            C[i][-j] = d1[-j] - c1[-j]*C[i][-j+1];

            // check early exercise condition
            if (type == 'P')
			{
				if (C[i][-j] < strike - S[N][-j])
				C[i][-j] = strike - S[N][-j];
			}
			else
			{
				if (C[i][-j] < S[N][-j] - strike)
				C[i][-j] = S[N][-j] - strike;
			}
		}
	}
}

//****************************************************************************


int main(int, char*[]) {

	try {

		boost::timer timer;
		std::cout << std::endl;


        LARGE_TITLE("London (2005) Derivatives Modeling: Finite Difference Method");

        /*
        ExplicitDiffMethod::explicitDiffAmerican(double price, double strike,
                double vol, double rate, double div, double T, int N, int M, char type,
                char bc)
        ImplicitDiffMethod::implicitDiffAmerican(double price, double strike,
                double vol, double rate, double div, double T, long N, long M,
                char type, char bc)
        ImplicitDiffMethod::solveTridiagonalAmer(double strike, long N, long M, double
                pu, double pm, double pd, double *d, double *c1, double *d1, char type)
        */

        double price = 50.0;
        double strike = 50.0;
        double rate = 0.06;
        double div = 0.03;
        double vol = 0.20;
        double T = 1.0;
        int N = 40; long N_ = 4;
        int M = 50; long M_ = 5;
        char type = 'C';
        char bc = 'D'; // Dirichlet or Neumann boundary conditions

        ExplicitDiffMethod expdiff;
        double expdiffC = expdiff.explicitDiffAmerican(
                    price, strike, vol, rate, div, T, N, M, type, bc);
        std::cout << "Call Option Price: "
                  << std::fixed << std::setprecision(5)
                  << expdiffC << std::endl
                  << " (American, explicit difference method)"
                  << std::endl << std::endl;

        ImplicitDiffMethod impdiff;
        double impdiffC = impdiff.implicitDiffAmerican(
                    price, strike, vol, rate, div, T, N_, M_, type, bc);
        std::cout << "Call Option Price: "
                  << std::fixed << std::setprecision(5)
                  << impdiffC << std::endl
                  << " (American, implicit difference method)"
                  << std::endl << std::endl;




        {
//		LARGE_TITLE("Equity Option with Finite Difference Method");

//		// set up dates
//		Calendar calendar = TARGET();
//		Date todaysDate(3, September, 2012);
//		Date settlementDate(5, September, 2012);
//		Settings::instance().evaluationDate() = todaysDate;

//		// our options
//		Option::Type type(Option::Put);
//		Real underlying = 45;
//		Real strike = 50;
//		Spread dividendYield = 0.015;
//		Rate riskFreeRate = 0.009;
//		Volatility volatility = 0.25;
//		Date maturity(3, September, 2013);
//		DayCounter dayCounter = Actual365Fixed();

//		std::cout << std::setw(30) << "Valuation date = " << todaysDate
//				<< std::endl;
//		std::cout << std::setw(30) << "Maturity = " << maturity << std::endl;
//		std::cout << std::setw(30) << "Option type = " << type << std::endl;
//		std::cout << std::setw(30) << "Underlying price = " << underlying
//				<< std::endl;
//		std::cout << std::setw(30) << "Strike = " << strike << std::endl;
//		std::cout << std::setw(30) << "Risk-free interest rate = "
//				<< io::rate(riskFreeRate) << std::endl;
//		std::cout << std::setw(30) << "Dividend yield = "
//				<< io::rate(dividendYield) << std::endl;
//		std::cout << std::setw(30) << "Volatility = "
//				<< io::volatility(volatility) << std::endl;
//		std::cout << std::endl;
//		std::string method;
//		std::cout << std::endl;

//		// write column headings
//		Size widths[] = { 35, 14, 14, 14 };
//		std::cout << std::setw(widths[0]) << std::left << "Method"
//				<< std::setw(widths[1]) << std::left << "European"
//				<< std::setw(widths[2]) << std::left << "Bermudan"
//				<< std::setw(widths[3]) << std::left << "American" << std::endl;

//		std::vector < Date > exerciseDates;
//		for (Integer i = 1; i <= 4; i++)
//			exerciseDates.push_back(settlementDate + 3 * i * Months);

//		boost::shared_ptr < Exercise
//				> europeanExercise(new EuropeanExercise(maturity));

//		boost::shared_ptr < Exercise
//				> bermudanExercise(new BermudanExercise(exerciseDates));

//		boost::shared_ptr < Exercise
//				> americanExercise(
//						new AmericanExercise(settlementDate, maturity));

//		Handle < Quote
//				> underlyingH(
//						boost::shared_ptr < Quote
//								> (new SimpleQuote(underlying)));
////        Handle<SimpleQuote> underlyingH(
////            boost::shared_ptr<SimpleQuote>(new SimpleQuote(underlying)));

//		// bootstrap the yield/dividend/vol curves
//		Handle < YieldTermStructure
//				> flatTermStructure(
//						boost::shared_ptr < YieldTermStructure
//								> (new FlatForward(settlementDate, riskFreeRate,
//										dayCounter)));
//		Handle < YieldTermStructure
//				> flatDividendTS(
//						boost::shared_ptr < YieldTermStructure
//								> (new FlatForward(settlementDate,
//										dividendYield, dayCounter)));
//		Handle < BlackVolTermStructure
//				> flatVolTS(
//						boost::shared_ptr < BlackVolTermStructure
//								> (new BlackConstantVol(settlementDate,
//										calendar, volatility, dayCounter)));
//		boost::shared_ptr < StrikedTypePayoff
//				> payoff(new PlainVanillaPayoff(type, strike));
//		boost::shared_ptr < BlackScholesMertonProcess
//				> bsmProcess(
//						new BlackScholesMertonProcess(underlyingH,
//								flatDividendTS, flatTermStructure, flatVolTS));

//		// options
//		VanillaOption europeanOption(payoff, europeanExercise);
//		VanillaOption bermudanOption(payoff, bermudanExercise);
//		VanillaOption americanOption(payoff, americanExercise);

//		// Analytic formulas:

//		// Black-Scholes for European
//		method = "Black-Scholes";
//		europeanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new AnalyticEuropeanEngine(bsmProcess)));
//		std::cout << std::setw(widths[0]) << std::left << method << std::fixed
//				<< std::setw(widths[1]) << std::left << europeanOption.NPV()
//				<< std::setw(widths[2]) << std::left << "N/A"
//				<< std::setw(widths[3]) << std::left << "N/A" << std::endl;

//		// Finite differences
//		Size timeSteps = 801;
//		method = "FD (Crank Nicolson)";
//		europeanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDEuropeanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		bermudanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDBermudanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		americanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDAmericanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		std::cout << std::setw(widths[0]) << std::left << method << std::fixed
//				<< std::setw(widths[1]) << std::left << europeanOption.NPV()
//				<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
//				<< std::setw(widths[3]) << std::left << americanOption.NPV()
//				<< std::endl;

//		method = "FD (Explcit Euler)";
//		europeanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDEuropeanEngine<ExplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		bermudanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDBermudanEngine<ExplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		americanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDAmericanEngine<ExplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		std::cout << std::setw(widths[0]) << std::left << method << std::fixed
//				<< std::setw(widths[1]) << std::left << europeanOption.NPV()
//				<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
//				<< std::setw(widths[3]) << std::left << americanOption.NPV()
//				<< std::endl;

//		method = "FD (Implicit Euler)";
//		europeanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDEuropeanEngine<ImplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		bermudanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDBermudanEngine<ImplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		americanOption.setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDAmericanEngine<ImplicitEuler>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		std::cout << std::setw(widths[0]) << std::left << method << std::fixed
//				<< std::setw(widths[1]) << std::left << europeanOption.NPV()
//				<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
//				<< std::setw(widths[3]) << std::left << americanOption.NPV()
//				<< std::endl;


//		LARGE_TITLE("Finite Difference Greeks (SensitivityAnalysis)");

//		//std::cout << Centered << std::endl;
//		boost::shared_ptr < Instrument
//				> europeanOption_(new VanillaOption(payoff, europeanExercise));
//		boost::shared_ptr < Instrument
//				> bermudanOption_(new VanillaOption(payoff, bermudanExercise));
//		boost::shared_ptr < Instrument
//				> americanOption_(new VanillaOption(payoff, americanExercise));
//		method = "FD (Crank Nicolson)";
//		europeanOption_->setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDEuropeanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		bermudanOption_->setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDBermudanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		americanOption_->setPricingEngine(
//				boost::shared_ptr < PricingEngine
//						> (new FDAmericanEngine<CrankNicolson>(bsmProcess,
//								timeSteps, timeSteps - 1)));
//		std::cout << std::setw(widths[0]) << std::left << method << std::fixed
//				<< std::setw(widths[1]) << std::left << europeanOption_->NPV()
//				<< std::setw(widths[2]) << std::left << bermudanOption_->NPV()
//				<< std::setw(widths[3]) << std::left << americanOption_->NPV()
//				<< std::endl;

//		std::vector < boost::shared_ptr<Instrument> > instruments;
//		instruments.push_back(europeanOption_);
//		instruments.push_back(bermudanOption_);
//		instruments.push_back(americanOption_);
//		std::vector < Real > weights;
//		Real sumNPV(aggregateNPV(instruments, weights));
//		std::cout << "Aggregated NPV: " << sumNPV << std::endl;

//		// TODO: rectify quotes
//		std::vector < Handle<SimpleQuote> > quotes;
//		//Handle<Quote> underlyingH(
//		//    boost::shared_ptr<Quote>(new SimpleQuote(underlying)));
//		Handle < SimpleQuote
//				> quote(
//						boost::shared_ptr < SimpleQuote
//								> (new SimpleQuote(underlying)));
//		//Handle<SimpleQuote> quote(boost::dynamic_pointer_cast<SimpleQuote>(underlyingH));
//		quotes.push_back(quote);
//		quotes.push_back(quote);
//		quotes.push_back(quote);

//		Real shift(10.0);

//		std::pair < Real, Real > sensitivities = parallelAnalysis(quotes,
//				instruments, weights, shift, Centered, Null<Real>());

//		std::cout << "Onesided sensitivity: " << sensitivities.first
//				<< std::endl;
//		std::cout << "Centered sensitivity: " << sensitivities.second
//				<< std::endl;
        }



		// End test
		Real seconds = timer.elapsed();
		Integer hours = int(seconds / 3600);
		seconds -= hours * 3600;
		Integer minutes = int(seconds / 60);
		seconds -= minutes * 60;
		std::cout << " \nRun completed in ";
		if (hours > 0)
			std::cout << hours << " h ";
		if (hours > 0 || minutes > 0)
			std::cout << minutes << " m ";
		std::cout << std::fixed << std::setprecision(0) << seconds << " s\n"
				<< std::endl;
		return 0;

	} catch (std::exception& e) {
		std::cerr << e.what() << std::endl;
		return 1;
	} catch (...) {
		std::cerr << "unknown error" << std::endl;
		return 1;
	}
}
