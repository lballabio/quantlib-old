#ifndef _BASKET_H__
#define _BASKET_H__

#include "datecl.h"
#include <vector>
#include <string>
#include "time.h"
#include <algorithm>
#include <map>
#include <numeric>
#include "CDO.h"
//#include "MatrixUtil.h"
//#endif

#define numSimulations 10000000
#define NOTIONAL 1000000
#define RECOVERY 0.4
#define NUM 4
#define FV 100
#define NUM_SIM 500000
#define step 0.25

static vector<double> hazard;
static vector<int> dayDiff;
static vector<int> matLength;
static vector<double> accrual;
static vector<double> discountRate;
static map<double,double> TR;
static map<double,double> rate;
static int counter = 0;


static void discount(double d) {
	discountRate.push_back(d);
}

static double interpolate(double rate1, double rate2, double t1, double t2, double x) {
        double dy = rate2 - rate1;
        double dt = t2 - t1;
		double slope = dy/dt;
           
		return rate1 + slope*x; 
}


class Exposure
{
public:
	int paySize;
	void setPaySize(double p) { paySize = p; }
	int getPaySize() { return paySize; }
	/*
	Exposure() : hazardRate(0.005), recoveryRate(0.4), tradeDate("today"), spread(0.01),
		effectiveDate("today"), notional(10000000), riskfreeRate(0.02) {
		
			effectiveDate = "today" + 1;
			maturityDate = "6/21/2004";
			accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
	}
	Exposure(Date matDate, double hazRate) : recoveryRate(0.4), tradeDate("today"), spread(0.01),
		notional(10000000), riskfreeRate(0.02) {
		
		effectiveDate = tradeDate + 1;
		if (effectiveDate == Date::SATURDAY)
			effectiveDate = effectiveDate + 2;
		else if (effectiveDate == Date::SUNDAY)
			effectiveDate = effectiveDate + 1;
		
		calcPaymentDates();
		maturityDate = matDate;
		hazardRate = hazRate;
		accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
	}	
	*/
	Exposure(vector<int> matYears, vector<double> spreads): recoveryRate(0.4), tradeDate("today"),
		notional(10000000), riskfreeRate(0.02) {

		effectiveDate = tradeDate + 1;
		if (effectiveDate == Date::SATURDAY)
			effectiveDate = effectiveDate + 2;
		else if (effectiveDate == Date::SUNDAY)
			effectiveDate = effectiveDate + 1;
		
		for (int i = 0; i < matYears.size(); i++)
		{
			Date maturity = matYears[i];
			T.push_back(matYears[i+1] - matYears[i]);
			maturityDate = computeDate(maturity);
			std::cout << "maturityDate = " << maturityDate << endl;
			hazardRate = 0.6;
			spread = spreads[i];
			accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
			accrual.push_back(accrualBasis);
			calcPaymentDates();
			calibrateHazardRates(i+1);
		}
	}
	Exposure(vector<int> matYears, vector<double> spreads, string name_, double recovery): T(matYears), recoveryRate(recovery), tradeDate("today"),
		notional(NOTIONAL), riskfreeRate(0.02), name(name_) {

		//effectiveDate = tradeDate;
		effectiveDate = tradeDate + 1;
		if (effectiveDate == Date::SATURDAY)
			effectiveDate = effectiveDate + 2;
		else if (effectiveDate == Date::SUNDAY)
			effectiveDate = effectiveDate + 1;
	
		//discountFactors();

		for (int i = 0; i < matYears.size(); i++)
		{
			spreads[i] = (double) spreads[i]/10000;
			Date maturity = effectiveDate.AddYears(matYears[i]);
			maturityDate = computeDate(maturity);
			//maturity.writeDate(maturityDate);
			//std::cout << "maturityDate = " << maturityDate << endl;
			hazardRate = 0.6;
			spread = spreads[i];
			effectiveDate = tradeDate +  1;
			accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
			accrual.push_back(accrualBasis);
			accrualBasis = accrual[0];
			calcPaymentDates1();
			calibrateHazardRates(i+1);
		}
	}
	Exposure(int matYears): recoveryRate(0.4), tradeDate("today"),
		notional(10000), riskfreeRate(0.05), spread(0.015) {

		effectiveDate = tradeDate + 1;
		if (effectiveDate == Date::SATURDAY)
			effectiveDate = effectiveDate + 2;
		else if (effectiveDate == Date::SUNDAY)
			effectiveDate = effectiveDate + 1;
	

		Date maturity = effectiveDate.AddYears(matYears);
		maturityDate = computeDate(maturity);
		//maturity.writeDate(maturityDate);
		//std::cout << "maturityDate = " << maturityDate << endl;
		effectiveDate = tradeDate +  1;
		accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
		accrual.push_back(accrualBasis);
		calcPaymentDates1();
	}
	Exposure(Date matDate, double hazRate, double s) : recoveryRate(0.4), tradeDate("today"),
		notional(NOTIONAL), riskfreeRate(0.02) {
		
		effectiveDate = tradeDate + 1;
		if (effectiveDate == Date::SATURDAY)
			effectiveDate = effectiveDate + 2;
		else if (effectiveDate == Date::SUNDAY)
			effectiveDate = effectiveDate + 1;
	
		if (matDate.day != 20)
			matDate.day = 20;
		if (matDate.day_of_week == Date::SATURDAY)
				matDate.day = 21;
		if (matDate.day_of_week == Date::SUNDAY)
				matDate.day = 22;
	
		maturityDate = matDate;
		hazardRate = hazRate;
		spread = s;
		accrualBasis = computeAccrualBasis(effectiveDate,maturityDate);
		accrual.push_back(accrualBasis);
		calcPaymentDates();
	}
	~Exposure() { };
	void computePaymentDates(Date start, Date end);
	double calcFixedLeg(double h) {

		hazardRate = h;
		int time = computeNumDays(effectiveDate,maturityDate);
		int time1 = time + 1;
		double survivalProb = (double) exp(-hazardRate*((double)time/365));
		double discountFactor = (double) exp(-riskfreeRate*((double)time1/365));

		return notional*spread*accrualBasis*discountFactor*survivalProb;
	}
	double calcFixedLegDeriv(double h) {
	
		hazardRate = h;
		int time = computeNumDays(effectiveDate,maturityDate);
		int time1 = time + 1;
		double survivalProb = exp(-hazardRate*((double)time/365));
		double discountFactor = exp(-riskfreeRate*((double)time1/365));
		
		return (-(double)time/365)*notional*spread*accrualBasis*discountFactor*survivalProb;	
	}
	double calcNPVFixedTwo(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazardRate*((double)(dayDiff[1])/365));
		double discountFactor = exp(-riskfreeRate*((double)(matLength[0] + 1)/365));
		double discountFactor1 = exp(-riskfreeRate*((double)(matLength[1] + 1)/365));

		return notional*spread*accrualBasis*discountFactor*survivalProb + 
			        notional*spread*((double)dayDiff[1]/360)*discountFactor1*survivalProb*survivalProb1;
	}
	double calcNPVFixedTwoDeriv(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazardRate*((double)(dayDiff[1])/365));
		double discountFactor = exp(-riskfreeRate*((double)(matLength[1] + 1)/365));
		
		return -((double)dayDiff[1]/365)*notional*spread*((double)dayDiff[1]/360)*discountFactor*survivalProb*survivalProb1;
	}
	double calcNPVFixedThree(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazardRate*((double)(dayDiff[2])/365));
		double discountFactor = exp(-riskfreeRate*((double)(matLength[0] + 1)/365));
		double discountFactor1 = exp(-riskfreeRate*((double)(matLength[1] + 1)/365));
		double discountFactor2 = exp(-riskfreeRate*((double)(matLength[2] + 1)/365));

		return notional*spread*((double)dayDiff[0]/360)*discountFactor*survivalProb 
		+      notional*spread*((double)dayDiff[1]/360)*discountFactor1*survivalProb*survivalProb1
		+      notional*spread*((double)dayDiff[2]/360)*discountFactor2*survivalProb*survivalProb1*survivalProb2;
	
	}

	double calcNPVFixedFour(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazard[2]*((double)(dayDiff[2])/365));
		double survivalProb3 = exp(-hazardRate*((double)(dayDiff[3])/365));
		double discountFactor = exp(-riskfreeRate*((double)(matLength[0] + 1)/365));
		double discountFactor1 = exp(-riskfreeRate*((double)(matLength[1] + 1)/365));
		double discountFactor2 = exp(-riskfreeRate*((double)(matLength[2] + 1)/365));
		double discountFactor3 = exp(-riskfreeRate*((double)(matLength[3] + 1)/365));

		return notional*spread*((double)dayDiff[0]/360)*discountFactor*survivalProb 
		+      notional*spread*((double)dayDiff[1]/360)*discountFactor1*survivalProb*survivalProb1
		+      notional*spread*((double)dayDiff[2]/360)*discountFactor2*survivalProb*survivalProb1*survivalProb2
		+      notional*spread*((double)dayDiff[3]/360)*discountFactor3*survivalProb*survivalProb1*survivalProb2*survivalProb3;
	}

	double calcNPVFixedFive(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazard[2]*((double)(dayDiff[2])/365));
		double survivalProb3 = exp(-hazard[3]*((double)(dayDiff[3])/365));
		double survivalProb4 = exp(-hazardRate*((double)(dayDiff[4])/365));
		double discountFactor = exp(-riskfreeRate*((double)(matLength[0] + 1)/365));
		double discountFactor1 = exp(-riskfreeRate*((double)(matLength[1] + 1)/365));
		double discountFactor2 = exp(-riskfreeRate*((double)(matLength[2] + 1)/365));
		double discountFactor3 = exp(-riskfreeRate*((double)(matLength[3] + 1)/365));
		double discountFactor4 = exp(-riskfreeRate*((double)(matLength[4] + 1)/365));

		return notional*spread*((double)dayDiff[0]/360)*discountFactor*survivalProb 
		+      notional*spread*((double)dayDiff[1]/360)*discountFactor1*survivalProb*survivalProb1
		+      notional*spread*((double)dayDiff[2]/360)*discountFactor2*survivalProb*survivalProb1*survivalProb2
		+      notional*spread*((double)dayDiff[3]/360)*discountFactor3*survivalProb*survivalProb1*survivalProb2*survivalProb3
		+	   notional*spread*((double)dayDiff[4]/360)*discountFactor4*survivalProb*survivalProb1*survivalProb2*survivalProb3*survivalProb4;
	}
	double calcNPVFixed(double h, int i)
	{
		hazardRate = h;
		double survivalProb = 1.0;
		double discountFactor = 0;
		double sum = 0.0;

		for (int j = 0; j < i; j++)	
		{
			discountFactor = exp(-riskfreeRate*((double)(matLength[j] + 1)/365));
			if (j != i-1)
				survivalProb *= exp(-hazard[j]*((double)(dayDiff[j])/365));
			else
				survivalProb *= exp(-hazardRate*((double)(dayDiff[j])/365));

			sum += notional*spread*((double)dayDiff[i-1]/360)*discountFactor*survivalProb;
		}

		return sum;
	}
	double calcNPVFixedThreeDeriv(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazardRate*((double)(dayDiff[2])/365));
		double discountFactor2 = exp(-riskfreeRate*((double)(matLength[2] + 1)/365));

		return -((double)dayDiff[2]/365)*notional*spread*((double)dayDiff[2]/360)*discountFactor2*survivalProb*survivalProb1*survivalProb2;
	}
	double calcNPVFixedFourDeriv(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazard[2]*((double)(dayDiff[2])/365));
		double survivalProb3 = exp(-hazardRate*((double)(dayDiff[3])/365));
		double discountFactor3 = exp(-riskfreeRate*((double)(matLength[3] + 1)/365));

		return -((double)dayDiff[3]/365)*notional*spread*((double)dayDiff[3]/360)*discountFactor3*survivalProb*survivalProb1*survivalProb2*survivalProb3;
	}

	double calcNPVFixedFiveDeriv(double h) {

		hazardRate = h;
		double survivalProb = exp(-hazard[0]*((double)(matLength[0])/365));
		double survivalProb1 = exp(-hazard[1]*((double)(dayDiff[1])/365));
		double survivalProb2 = exp(-hazard[2]*((double)(dayDiff[2])/365));
		double survivalProb3 = exp(-hazard[3]*((double)(dayDiff[3])/365));
		double survivalProb4 = exp(-hazard[4]*((double)(dayDiff[4])/365));
		double discountFactor3 = exp(-riskfreeRate*((double)(matLength[4] + 1)/365));

		return -((double)dayDiff[4]/365)*notional*spread*((double)dayDiff[4]/360)*discountFactor3*survivalProb*survivalProb1*survivalProb2*survivalProb3*survivalProb4;
	}
	double calcNPVFixedDeriv(double h, int i) {

		hazardRate = h;
		double survivalProb = 1.0;
		double discountFactor =  exp(-riskfreeRate*((double)(matLength[i-1] + 1)/365));

		for (int j = 0; j < i; j++)	
		{
			if (j != i-1)
				survivalProb *= exp(-hazard[j]*((double)(dayDiff[j])/365));
			else
				survivalProb *= exp(-hazardRate*((double)(dayDiff[j])/365));
		}
		return  -((double)dayDiff[i-1]/365)*notional*spread*discountFactor*survivalProb;

	}
	double calcNPVFloatingTwo(double h) {

		hazardRate = h;
		//printf("dayDiff[0] = %d matLength[0] = %d\n",dayDiff[0],matLength[0]);
		double defaultProb = 1 - exp(-hazard[0]*((double)matLength[0])/365);
	
		double defaultProb1 = 1 - exp(-hazardRate*((double)dayDiff[1])/365);
		double survivalProb = exp(-hazard[0]*((double)dayDiff[0])/365);
		double discountFactor = exp(-riskfreeRate*(double)(matLength[0]+1)/365);
		double discountFactor1 = exp(-riskfreeRate*(double)(matLength[1] + 1)/365);
		double accruedInterest1 = 0.5*notional*spread*((double)dayDiff[1]/360);
		double accruedInterest = 0.5*notional*spread*((double)dayDiff[0]/360);
		payments.push_back(accruedInterest);
		payments.push_back(accruedInterest1);
	
		return (notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb
			 + (notional*(1 - recoveryRate) - accruedInterest1)*discountFactor1*survivalProb*defaultProb1;

	}
	double calcNPVFloatingThree(double h) {
	
		hazardRate = h;
		//printf("dayDiff[1] = %d matLength[1] = %d\n",dayDiff[1],matLength[1]);
		double defaultProb = 1 - exp(-hazard[0]*((double)matLength[0])/365);
		double defaultProb1 = 1 - exp(-hazard[1]*((double)dayDiff[1])/365);
		double defaultProb2 = 1 - exp(-hazardRate*((double)dayDiff[2])/365);
		
		double survivalProb = exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		
		double discountFactor = exp(-riskfreeRate*(double)(matLength[0]+1)/365);
		double discountFactor1 = exp(-riskfreeRate*(double)(matLength[1]+ 1)/365);
		double discountFactor2 = exp(-riskfreeRate*(double)(matLength[2]+ 1)/365);
		
		double accruedInterest = 0.5*notional*spread*((double)dayDiff[0]/360);
		double accruedInterest1 = 0.5*notional*spread*((double)dayDiff[1]/360);
		double accruedInterest2 = 0.5*notional*spread*((double)dayDiff[2]/360);

		payments.push_back(accruedInterest);
		payments.push_back(accruedInterest1);
		payments.push_back(accruedInterest2);

		return (notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb
			 + (notional*(1 - recoveryRate) - accruedInterest1)*discountFactor1*survivalProb*defaultProb1
		     + (notional*(1 - recoveryRate) - accruedInterest2)*discountFactor2*survivalProb*survivalProb1*defaultProb2;
	}
	double calcNPVFloatingFour(double h)
	{

		hazardRate = h;
		//printf("dayDiff[1] = %d matLength[1] = %d\n",dayDiff[1],matLength[1]);
		double defaultProb = 1 - exp(-hazard[0]*((double)matLength[0])/365);
		double defaultProb1 = 1 - exp(-hazard[1]*((double)dayDiff[1])/365);
		double defaultProb2 = 1 - exp(-hazard[2]*((double)dayDiff[2])/365);
		double defaultProb3 = 1 - exp(-hazardRate*((double)dayDiff[3])/365);
		
		double survivalProb = exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		double survivalProb2 = exp(-hazard[2]*((double)dayDiff[2])/365);

		double discountFactor = exp(-riskfreeRate*(double)(matLength[0]+1)/365);
		double discountFactor1 = exp(-riskfreeRate*(double)(matLength[1]+ 1)/365);
		double discountFactor2 = exp(-riskfreeRate*(double)(matLength[2]+ 1)/365);
		double discountFactor3 = exp(-riskfreeRate*(double)(matLength[3]+ 1)/365);
		

		double accruedInterest = 0.5*notional*spread*((double)dayDiff[0]/360);
		double accruedInterest1 = 0.5*notional*spread*((double)dayDiff[1]/360);
		double accruedInterest2 = 0.5*notional*spread*((double)dayDiff[2]/360);
		double accruedInterest3 = 0.5*notional*spread*((double)dayDiff[3]/360);

		payments.push_back(accruedInterest);
		payments.push_back(accruedInterest1);
		payments.push_back(accruedInterest2);
		payments.push_back(accruedInterest3);

		return (notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb
			 + (notional*(1 - recoveryRate) - accruedInterest1)*discountFactor1*survivalProb*defaultProb1
		     + (notional*(1 - recoveryRate) - accruedInterest2)*discountFactor2*survivalProb*survivalProb1*defaultProb2
			 + (notional*(1 - recoveryRate) - accruedInterest3)*discountFactor3*survivalProb*survivalProb1*survivalProb2*defaultProb3;

	}
	double calcNPVFloatingFive(double h)
	{

		hazardRate = h;
		//printf("dayDiff[1] = %d matLength[1] = %d\n",dayDiff[1],matLength[1]);
		double defaultProb = 1 - exp(-hazard[0]*((double)matLength[0])/365);
		double defaultProb1 = 1 - exp(-hazard[1]*((double)dayDiff[1])/365);
		double defaultProb2 = 1 - exp(-hazard[2]*((double)dayDiff[2])/365);
		double defaultProb3 = 1 - exp(-hazard[3]*((double)dayDiff[3])/365);
		double defaultProb4 = 1 - exp(-hazardRate*((double)dayDiff[4])/365);
		
		double survivalProb = exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		double survivalProb2 = exp(-hazard[2]*((double)dayDiff[2])/365);
		double survivalProb3 = exp(-hazard[3]*((double)dayDiff[3])/365);

		double discountFactor = exp(-riskfreeRate*(double)(matLength[0]+1)/365);
		double discountFactor1 = exp(-riskfreeRate*(double)(matLength[1]+ 1)/365);
		double discountFactor2 = exp(-riskfreeRate*(double)(matLength[2]+ 1)/365);
		double discountFactor3 = exp(-riskfreeRate*(double)(matLength[3]+ 1)/365);
		double discountFactor4 = exp(-riskfreeRate*(double)(matLength[4]+ 1)/365);

		double accruedInterest = 0.5*notional*spread*((double)dayDiff[0]/360);
		double accruedInterest1 = 0.5*notional*spread*((double)dayDiff[1]/360);
		double accruedInterest2 = 0.5*notional*spread*((double)dayDiff[2]/360);
		double accruedInterest3 = 0.5*notional*spread*((double)dayDiff[3]/360);
		double accruedInterest4 = 0.5*notional*spread*((double)dayDiff[4]/360);

		payments.push_back(accruedInterest);
		payments.push_back(accruedInterest1);
		payments.push_back(accruedInterest2);
		payments.push_back(accruedInterest3);
		payments.push_back(accruedInterest4);

		return (notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb
			 + (notional*(1 - recoveryRate) - accruedInterest1)*discountFactor1*survivalProb*defaultProb1
		     + (notional*(1 - recoveryRate) - accruedInterest2)*discountFactor2*survivalProb*survivalProb1*defaultProb2
			 + (notional*(1 - recoveryRate) - accruedInterest3)*discountFactor3*survivalProb*survivalProb1*survivalProb2*defaultProb3
			 + (notional*(1 - recoveryRate) - accruedInterest4)*discountFactor4*survivalProb*survivalProb1*survivalProb2*survivalProb3*defaultProb4;
	}
	double calcNPVFloating(double h, int i) {

		hazardRate = h;
		double defaultProb = 1.0;  
		double discountFactor = 1.0;
		double survivalProb = 1.0;
		double accruedInterest = 0.0;
		double sum = 0.0;

		for (int j = 0; j < i; j++)
		{
			discountFactor *= exp(-riskfreeRate*((double)dayDiff[j]+1)/365);
			survivalProb *= exp(-riskfreeRate*((double)dayDiff[j]+1)/365);
			defaultProb *= 1 - exp(-hazardRate*((double)dayDiff[j])/365);
			accruedInterest = 0.5*notional*spread*(((double)dayDiff[j]+1)/360);
			payments.push_back(accruedInterest);
			if (j == 0)
				sum += (notional*(1-recoveryRate) - accruedInterest)*discountFactor*defaultProb;
			else
				sum += (notional*(1-recoveryRate) - accruedInterest)*discountFactor*survivalProb*defaultProb;
		}
		
		return sum;

	}
	double calcNPVFloatingTwoDeriv(double h) {

		hazardRate = h;
		double defaultProb = 1 - exp(-hazard[0]*((double)matLength[0])/365);
		double defaultProb1 = 1 - exp(-hazardRate*((double)dayDiff[1])/365);
		
		double survivalProb = exp(-hazard[0]*((double)dayDiff[0])/365);
		double discountFactor1 = exp(-riskfreeRate*(double)(matLength[1] + 1)/365);
		double accruedInterest = 0.5*notional*spread*((double)dayDiff[1]/360);

		double z = ((double)dayDiff[1]/365)*(notional*(1 - recoveryRate) - accruedInterest)*discountFactor1*survivalProb*defaultProb1;

		return z;
	}
 	double calcNPVFloatingThreeDeriv(double h) {

		hazardRate = h;

		double defaultProb2 = 1 - exp(-hazardRate*((double)dayDiff[2])/365);

		double survivalProb =  exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		double discountFactor2 = exp(-riskfreeRate*(double)(matLength[2] + 1)/365);
		double accruedInterest2 = 0.5*notional*spread*((double)dayDiff[2]/360);

		double z = ((double)dayDiff[2]/365)*(notional*(1 - recoveryRate) - accruedInterest2)*discountFactor2*survivalProb*survivalProb1*defaultProb2;

		return z;
	}

 	double calcNPVFloatingFourDeriv(double h) {

		hazardRate = h;
		double defaultProb3 = 1 - exp(-hazardRate*((double)dayDiff[3])/365);

		double survivalProb =  exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		double survivalProb2 = exp(-hazard[2]*((double)dayDiff[2])/365);
		

		double discountFactor3 = exp(-riskfreeRate*(double)(matLength[3] + 1)/365);		
		double accruedInterest3 = 0.5*notional*spread*((double)dayDiff[3]/360);

		return ((double)dayDiff[3]/365)*(notional*(1 - recoveryRate) - accruedInterest3)*discountFactor3*survivalProb*survivalProb1*survivalProb2*defaultProb3;
	}
	double calcNPVFloatingFiveDeriv(double h) {

		hazardRate = h;
		double defaultProb4 = 1 - exp(-hazardRate*((double)dayDiff[4])/365);

		double survivalProb =  exp(-hazard[0]*((double)dayDiff[0])/365);
		double survivalProb1 = exp(-hazard[1]*((double)dayDiff[1])/365);
		double survivalProb2 = exp(-hazard[2]*((double)dayDiff[2])/365);
		double survivalProb3 = exp(-hazard[3]*((double)dayDiff[3])/365);

		double discountFactor4 = exp(-riskfreeRate*(double)(matLength[4] + 1)/365);		
		double accruedInterest4 = 0.5*notional*spread*((double)dayDiff[4]/360);

		return ((double)dayDiff[4]/365)*(notional*(1 - recoveryRate) - accruedInterest4)*discountFactor4*survivalProb*survivalProb1*survivalProb2*survivalProb3*defaultProb4;
		
	}
	double calcFloatingLegDeriv(double h) {

		hazardRate = h;
		double accruedInterest = calcRebate(effectiveDate);
		int time = computeNumDays(effectiveDate,maturityDate);
		int time1 = time + 1;
		double defaultProb = 1 - exp(-hazardRate*((double)time/365));
		double discountFactor = exp(-riskfreeRate*((double)time1/365));
		
		return ((double)time/365)*(notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb;

	}
	double calcNPVFloatingDeriv(double h, int i) 
	{

		hazardRate = h;
		double defaultProb = 1 - exp(-hazardRate*((double)dayDiff[i-1])/365); 
		double discountFactor = exp(-riskfreeRate*((double)matLength[i-1] + 1)/365);
		double accruedInterest = 0.5*notional*spread*((double)dayDiff[i-1]/360);
		double survivalProb = 1;

		if (i != 1)
		{
			for (int j = 0; j < i; j++)
			{
				survivalProb *= exp(-hazard[j]*((double)dayDiff[j])/365);
			}
		}

		return ((double)dayDiff[i-1]/365)*(notional*(1 - recoveryRate) - accruedInterest)*discountFactor*survivalProb*defaultProb;
	}

	double calcFloatingLeg(double h) {
		
		hazardRate = h;
		double accruedInterest = calcRebate(effectiveDate);
		payments.push_back(accruedInterest);
		int time = computeNumDays(effectiveDate,maturityDate);
		int time1 = time + 1;
		double defaultProb = (double) 1 - exp(-hazardRate*((double)time/365));
		double discountFactor = exp(-riskfreeRate*((double) time1/365));
		
		return (notional*(1 - recoveryRate) - accruedInterest)*discountFactor*defaultProb;
	}

	int computeNumDays(Date start, Date end1) {

		return end1 - start + 1;
	}
	void setNumDays(int numDay) {

		numDays = numDay;
	}
	double calcRebate(Date defaultDate) {

		//int time = defaultDate - effectiveDate;
		int time1 = maturityDate - effectiveDate; 
		return 0.5*notional*spread*((double)time1/360);
		//return notional*spread*(time/360);
	}
	double calcRebate1() {

		int time1 = dayDiff[1];
		return 0.5*notional*spread*((double)time1/360);
	}
	void calcPaymentDates()
	{
		Date d, paymentDay;
		int cnt = 1;
		payments.empty();
		payments.clear();

		std::cout << "payment size = " << payments.size() << "cnt = " << cnt <<  endl;
		Date firstDate = computeDate(effectiveDate);
		std::cout << "first Payment Date " << firstDate << endl;
		paymentDates.push_back(firstDate);
		int diff = firstDate - effectiveDate + 1;
	 
		periodLength.push_back(diff);
		payments.push_back(notional*spread*((double)diff/365));
		survival.push_back(exp(-hazardRate*((double)diff/365)));

		do
		{
			if (cnt != 1)
				d = firstDate.AddMonths(3);
			else
				d = firstDate;
			paymentDay = computeDate(d);
			paymentDates.push_back(paymentDay);
			int length = paymentDates[cnt] - paymentDates[cnt-1];
			int duration = paymentDates[cnt] - tradeDate + 1;
			survival.push_back(exp(-hazardRate*((double)duration/365)));
			double val = (notional*spread*((double)length/365))*survival[cnt-1]*discountRate[cnt-1];
			std::cout << "val = " << val << endl;
			payments.push_back(val);
			//if (cnt != 1)
			//	cout << length << " ";
			//else
			//	cout << diff << " ";
			//if (cnt != 1)
			//	cout << payments[cnt] << endl;
			//else
			//	cout << payments[cnt-1] << endl;
			periodLength.push_back(length);
			cnt++;
		}
		while (paymentDay != maturityDate);
		
		totalPay = accumulate(payments.begin(),payments.end(),(float)0.0);
		std::cout << "payment size = " << payments.size() << "cnt = " << cnt <<  endl;
		setPaySize(payments.size());
		payments.empty();
		payments.clear();
		
	}
	void calcPaymentDates1()
	{
		Date d, paymentDay;
		int cnt = 1;
		payments.empty();
		payments.clear();

		//cout << "payment size = " << payments.size() << "cnt = " << cnt <<  endl;
		Date firstDate = computeDate(effectiveDate);
		//firstDate.writeDate(firstDate);
		//cout << firstDate << endl;
		paymentDates.push_back(firstDate);
		int diff = firstDate - effectiveDate + 1;
		//cout << diff << endl;
		periodLength.push_back(diff);
		payments.push_back(notional*spread*((double)diff/365));
		survival.push_back(exp(-hazardRate*((double)diff/365)));

		do
		{
			if (cnt != 1)
				d = firstDate.AddMonths(3);
			else
				d = firstDate;
			paymentDay = computeDate(d);
			//cout << paymentDay << endl;
			paymentDates.push_back(paymentDay);
			int length = paymentDates[cnt] - paymentDates[cnt-1];
			int duration = paymentDates[cnt] - tradeDate + 1;
			survival.push_back(exp(-hazardRate*((double)duration/365)));
			double val = (notional*spread*((double)length/365))*survival[cnt-1]*discountRate[cnt-1];
			//cout << "val = " << val << endl;
			payments.push_back(val);
			//if (cnt != 1)
			//	cout << length << " ";
			//else
			//	cout << diff << " ";
			//if (cnt != 1)
			//	cout << payments[cnt] << endl;
			//else
			//	cout << payments[cnt-1] << endl;
			periodLength.push_back(length);
			cnt++;
		}
		while (paymentDay <= maturityDate);
		
		totalPay = accumulate(payments.begin(),payments.end(),(float)0.0);
		//std::cout << "payment size = " << payments.size() << "cnt = " << cnt <<  endl;
		setPaySize(payments.size());
		payments.empty();
		payments.clear();
		
	}
	Date computeDate(Date maturity)
	{
		if ((maturity.month == Date::JANUARY) || (maturity.month == Date::FEBRUARY))
		{
			maturity.month = Date::MARCH;
			maturity.day = 20;
			if (maturity.day_of_week == Date::SATURDAY)
				maturity.day = 21;
			if (maturity.day_of_week == Date::SUNDAY)
				maturity.day = 22;
		}
		else if (maturity.month == Date::MARCH)
		{
			if (maturity.day < 20)
			{
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
			else if (maturity.day != 20)
			{
				maturity.month = Date::JUNE;
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
		}
		else if ((maturity.month == Date::APRIL) || (maturity.month == Date::MAY))
		{
		
			maturity.month = Date::JUNE;
			maturity.day = 20;
			if (maturity.day_of_week == Date::SATURDAY)
				maturity.day = 21;
			if (maturity.day_of_week == Date::SUNDAY)
				maturity.day = 22;
		}
		else if (maturity.month == Date::JUNE)
		{
			if (maturity.day < 20)
			{
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
			else if (maturity.day != 20)
			{
				maturity.month = Date::SEPTEMBER;
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
		}
		else if ((maturity.month == Date::JULY) || (maturity.month == Date::AUGUST))
		{
			maturity.month = Date::SEPTEMBER;
			maturity.day = 20;
			if (maturity.day_of_week == Date::SATURDAY)
				maturity.day = 21;
			if (maturity.day_of_week == Date::SUNDAY)
				maturity.day = 22;
		}
		else if (maturity.month == Date::SEPTEMBER)
		{
			if (maturity.day < 20)
			{
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
			else if (maturity.day != 20)
			{
				maturity.month = Date::DECEMBER;
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
		}
		else if ((maturity == Date::OCTOBER) || (maturity == Date::NOVEMBER))
		{

			maturity.month = Date::DECEMBER;
			maturity.day = 20;
			if (maturity.day_of_week == Date::SATURDAY)
				maturity.day = 21;
			if (maturity.day_of_week == Date::SUNDAY)
				maturity.day = 22;
		}
		else if (maturity.month == Date::DECEMBER)
		{
			if (maturity.day < 20)
			{
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
			}
			else if (maturity.day != 20)
			{
				maturity.month = Date::MARCH;
				maturity.day = 20;
				if (maturity.day_of_week == Date::SATURDAY)
					maturity.day = 21;
				if (maturity.day_of_week == Date::SUNDAY)
					maturity.day = 22;
				maturity.year = maturity.year + 1;
			}
		}
		
		return maturity;
	}
	double computeAccrualBasis(Date start, Date end) {
		
		// compute accrual basis
		numDays = computeNumDays(effectiveDate,maturityDate);
		//printf("numDays = %d\n",numDays);
		setNumDays(numDays);
		matLength.push_back(numDays);
		counter++;
		if (dayDiff.size() == 0)
			dayDiff.push_back(numDays);
		else
		{
			int diff1 = matLength[counter-1] - matLength[counter-2] + 1;
			dayDiff.push_back((int)diff1);
		}
		accrualBasis = (double) numDays/360;
		return accrualBasis; 
	}
	void setMaturityDate(Date d1)
	{
		if ((d1 != Date::JUNE) && (d1 != Date::SEPTEMBER)
			&& (d1 != Date::DECEMBER) && (d1 != Date::MARCH))
		{
			//cout << "Maturity month must be March, June, Sept., or Dec." << endl;
			maturityDate = "6/20/2009";  // default maturity
		}
		else
			maturityDate = d1;

	}
	void calibrateHazardRates(int num)
	{
	int j = 0;
	double h = hazardRate;
	double f, fd;
	double error = 0.0;
	double dfixedleg = 0.0;
	double dfloatingleg = 0.0;

		/*	
		do  
		{
			f = calcNPVFloating(h,num) - calcNPVFixed(h,num);
			//printf("f = %0.5f\n",f);
			dfixedleg = calcNPVFixedDeriv(h,num);
			//printf("dfixedleg = %0.5f\n",dfixedleg);
			dfloatingleg = calcNPVFloatingDeriv(h,num);
			//printf("dfloatingleg = %0.5f\n",dfloatingleg);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
		}
		while (fabs(error) > 0.00001);
		*/
 
	switch (num)
	{
	case 1:
		do  
		{
			f = calcFloatingLeg(h) - calcFixedLeg(h);
			dfixedleg = calcFixedLegDeriv(h);
			dfloatingleg = calcFloatingLegDeriv(h);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
		}
		while (fabs(error) > 0.00001);
		break;
	case 2:
		do  
		{
			f = calcNPVFloatingTwo(h) - calcNPVFixedTwo(h);
			//printf("f = %0.3f\n",f);
			dfixedleg = calcNPVFixedTwoDeriv(h);
			//printf("dfixedleg = %0.3f\n",dfixedleg);
			dfloatingleg = calcNPVFloatingTwoDeriv(h);
			//printf("dfloatingleg = %0.3f\n",dfloatingleg);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
		}
		while (fabs(error) > 0.00001);
		break;
	case 3:
	   do
	   {
			f = calcNPVFloatingThree(h) - calcNPVFixedThree(h);
			
			dfixedleg = calcNPVFixedThreeDeriv(h);
			//printf("d3fixedleg = %0.3f\n",dfixedleg);
			dfloatingleg = calcNPVFloatingThreeDeriv(h);
			//printf("d3floatingleg = %0.3f\n",dfloatingleg);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
	   }
	   while (fabs(error) > 0.00001);
	   break;
	case 4:
		do
		{
			f = calcNPVFloatingFour(h) - calcNPVFixedFour(h);
			
			dfixedleg = calcNPVFixedFourDeriv(h);
			//printf("d3fixedleg = %0.3f\n",dfixedleg);
			dfloatingleg = calcNPVFloatingFourDeriv(h);
			//printf("d3floatingleg = %0.3f\n",dfloatingleg);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
		}
		while (fabs(error) > 0.00001);
		break;
	case 5:
		do
		{
			f = calcNPVFloatingFive(h) - calcNPVFixedFive(h);
			dfixedleg = calcNPVFixedFiveDeriv(h);
			dfloatingleg = calcNPVFloatingFiveDeriv(h);
			fd = dfloatingleg - dfixedleg;
			h = h - 0.01*(f/fd); 
			error = -0.01*f/fd;
		}
		while (fabs(error) > 0.00001);
		break;
	}
 
			
	hazardRate = h;
	hazard.push_back(hazardRate);
	hazRate.push_back(hazardRate);

	std::cout << getName() << " " << T[num-1] << "year  hazardRate = " << hazardRate << endl;
}
	void setName(string name_) { name = name_; }
	string getName() { return name; }
private:
	friend class Basket;
	double hazardRate;
	vector<double> hazRate;
	map<Date,double> matHaz;
	double accrualBasis;
	double totalPay;
	Date defaultTime;
	vector<int> periodLength;
	vector<Date> paymentDates;
	vector<double> payments;
	vector<double> survival;
	double survivalRate;
	double recoveryRate;
	double riskfreeRate;
	string name;
	Date maturityDate;
	double notional;
	int numDays;
	Date tradeDate;  // start date
	Date effectiveDate;
	double spread;
	double rebate;
	vector<int> T;
};

struct BondPrice
{
	double price;
	double coupon;
	int numPeriods;
};



class Basket
{
public:
	Basket() { kthToDefault = 1; }
	~Basket() {};
	void bootstrap(vector<double> price, vector<double> coupon, vector<double> period);	
	void bootstrap(map<double,double> TR);
	double genCorrelatedDeviates();
	double priceBasket();
	double priceCDOBasket(int numReference, int expiry,vector<Tranche> tranche);
	void computePaymentDates(Date start, Date end);
	void setEffectiveDate(Date d1);
	void setTradeDate(Date d1);
	void setMaturityDate(Date d1);
	void setkthtoDefault(int kthDefault);
	void addExposure(Exposure& ex) {
		exposure.push_back(ex);
	}
	double calcSumDiscPayments(Date d, int j);
	double calcDefaultTime(double d, int j);
	int getNumExposures();
private:
	vector<Exposure> exposure;
	int frequency;   // frequency of payments
	Date tradeDate;  // start date
	Date effectiveDate;
	Date maturityDate;         // maturity of basket
	int numExposures;          // number of exposures in the basket
	vector<Date> paymentDates;
	vector<double> spotRates;
	vector<Date> numDays;
	vector<double> cdsSpreads;
	vector<double> termStructure;	 
	vector<double> coupons;
	vector<double> bondPrice;
	vector<double> period;
	int kthToDefault;
	vector<double> discountRate;
	vector<double> discountSum;
	//SymmetricMatrix generateCorrelationMatrix(SymmetricMatrix& C);
	double faceValue;
};

#endif



