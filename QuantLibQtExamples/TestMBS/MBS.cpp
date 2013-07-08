// MBS.cpp: implementation of the MBS class.
//
//////////////////////////////////////////////////////////////////////

#include "MBS.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

void MBS::buildTree(double initRate, double financeRate, int N)
{

	Utility util;
	double u = 1.1;
	double d = 1/u;
	double p = (exp(initRate*T) - d)/(u - d);
	double deviate = 0.0;
	long seed = 0;
	double refRate = financeRate;
	long* idum = 0;
	double pay = faceValue;
	double faceAmount = 0.0;
	double interest = 0.0;
	double schedulePrincipal = 0.0;
	double prepaidPrincipal = 0.0;
	double CPR = 0.0;
	double balance = faceValue;
	double sum = 0.0;
	double totalsum = 0.0;
	double SMM = 0.0;
	TNT::Array1D<double> CF(SIZE_X);   // cash_flow 
	vector<double> disc(0.0);

	srand(unsigned(time(0)));
	seed = (long) rand() % 100;
	idum = &seed;
	// build binomial tree for rates
	for (int i = 0; i <= N; i++)
	{
		for (int j = 0; j <= i; j++)
		{
			spotRate[i][j] = initRate*pow(u,j)*pow(d,i-j);
			discountRate[i][j] = spotRate[i][j] + OAS;
		}
	}


	faceAmount = faceValue;
	int k = 0;
	long int M = 10000;
    //int cnt = 0;
	double r = 0.0;
	int j = 0;
    int i = 0;

    for (k = 0; k < M; k++) // M: number of simulation paths
	{ 
		sum = 0.0;
		balance = faceValue;
		refRate = financeRate;
		j = 0;
		disc.clear();
		disc.empty();
		disc.push_back(discountRate[0][0]);

        for (i = 0; i < N; i++) // N: number of tree paths per simulation
		{	
				balance = balance - (schedulePrincipal + prepaidPrincipal);
				deviate = util.gasdev(idum);
			
				if (deviate > 0)
				{
					j++;
					refRate = refRate*u;
				}
				else
				{
					j--;
					if (j < 0)
						j = 0;
					refRate = refRate*d;
				}
				disc.push_back(discountRate[i+1][j]);
				interest = coupon*balance;
				pay = calcPayment(balance,WAM-i);
				schedulePrincipal = pay - interest;
				
				if (balance >= schedulePrincipal)  
				{
					CPR = calcCPR(refRate);
					SMM = calcSMM(CPR);
					prepaidPrincipal = SMM*(balance - schedulePrincipal);
					
					if (i != N-1)
						CF[i] = interest + schedulePrincipal + prepaidPrincipal;	
					else
						CF[i] = interest + balance;
		
					r = computeZeroRates(i,disc);
					sum = sum + CF[i]/(pow(1+r,i+1));
				
				}
				else 
					goto x;
				
		}
		x:
		totalsum = totalsum + sum;  
	}
	double ave = (totalsum/M);
	std::cout << "MBS price = " << ave << endl;
}

double MBS::calcCPR(double rate)
{
	double CPR = 0.0;
	double value = WAC - rate;

	/*
	if (value <= 0)
		CPR = 0.05;
	else if ((value <= 0.005) && (value > 0))
		CPR = 0.10;
	else if ((value <= 0.01) && (value > 0.005))
		CPR = 0.20;
	else if ((value <= 0.0125) && (value > 0.01))
		CPR = 0.30;
	else if ((value <= 0.02) && (value > 0.0125))
		CPR = 0.40;
	else if ((value <= 0.025) && (value > 0.02))
		CPR = 0.50;
	else if ((value <= 0.03) && (value > 0.025))
		CPR = 0.60;
	else 
		CPR = 0.70;
	*/

    CPR = 100.0*(1.0-pow((1.0-(value/100.0)),12.0));

	return CPR;

}

double MBS::calcPayment(double fv, double T) {
    return (fv*coupon)/(1.0-pow(1.0/(1.0+coupon),T));
}

void MBS::calcPrice(double initRate, double financeRate, int N, long int M){

	Utility util;			         // utility class for generating random deviates
	double u = 1.1;			         // up move in binomial tree
	double d = 1/u;			         // down move in binomial tree
	double p = (exp(initRate*T) - d)/(u - d);  // up probablity
	double deviate = 0.0;			 // random deviate
	long seed = 0;					 // seed			
	double refRate = financeRate;	 // refinance rate
	long* idum = NULL;		         // pointer to seed value for RNG	
	double pay = faceValue;	         // face value of MBS
	double faceAmount = 0.0;	     // face amount 
	double interest = 0.0;		     // interest payment
	double schedulePrincipal = 0.0;	 // scheduled principal payments
	double prepaidPrincipal = 0.0;	 // prepaid principal payments
	double CPR = 0.0;		         // conditional prepayments
	double SMM = 0.0;				 // monthly mortality
	double balance = faceValue;      // balance remaining
	double sum = 0.0;		         // sum of discounted cash flows along a path
	double totalsum = 0.0;	         // total sum of all discounted cash flows
	double totalsum2 = 0.0;
	TNT::Array1D<double> CF(SIZE_X);  // cash_flow 
	vector<double> disc(0.0);	        // stores discount rates


	// build binomial tree for rates
	for (int i = 0; i <= N; i++)
	{
		for (int j = 0; j <= i; j++)
		{
			spotRate[i][j] = initRate*pow(u,j)*pow(d,i-j);
			discountRate[i][j] = spotRate[i][j] + OAS;
		}
	}

	srand(unsigned(time(0)));
	seed = (long) rand() % 100;
	idum = &seed;
	faceAmount = faceValue;
	int k = 0;
	int cnt = 0;
	double r = 0.0;
	int j = 0;
    int i = 0;

	for (k = 0; k < M; k++)
	{ 
		sum = 0.0;
		balance = faceValue;
		refRate = financeRate;
		j = 0;
		disc.clear();
		disc.push_back(discountRate[0][0]);

		for (i = 0; i < N; i++)
		{	
			balance = balance - (schedulePrincipal + prepaidPrincipal);
			deviate = util.gasdev(idum);
			
			if (deviate > 0)
			{
			       j++;
			       refRate = refRate*u;
			}
			else
			{
			       j--;
			       if (j < 0)
				j = 0;
			       refRate = refRate*d;
			}
			disc.push_back(discountRate[i+1][j]);
				
			interest = coupon*balance;
			pay = calcPayment(balance,WAM-i);
			schedulePrincipal = pay - interest;
				
			if (balance >= schedulePrincipal)  
			{
			   CPR = calcCPR(refRate);
			   SMM = calcSMM(CPR);
			   prepaidPrincipal = SMM*(balance - schedulePrincipal);
					
			   if (i != N-1)
			      CF[i] = interest + schedulePrincipal + prepaidPrincipal;	
			   else
			      CF[i] = interest + balance;
		
			    r = computeZeroRates(i,disc);
			    sum = sum + CF[i]/(pow(1+r,i+1));
				
			}
			else   // break out of loop
			   goto x;
				
		}
		x:
		totalsum = totalsum + sum; 
		totalsum2 = totalsum2 + sum*sum;
	}
	double ave = (totalsum/M);

	mbsPrice = ave;
	stdDev = sqrt(totalsum2 - (double)(totalsum*totalsum)/M)*(exp(-
	2*initRate*T)/(M-1));
	stdErr = (double) stdDev/sqrt(M);
}


double MBS::calcSMM(double CPR) {

     return (1.0 - pow((1.0 - CPR),(double)1.0/12.0));
}

double MBS::computeZeroRates(int cnt, vector<double> rate)
{
	
	double value = WAC+1;
	for (int j = 1; j <= cnt; j++)
	    value = value*(1 + rate[j]);

	if (cnt == 0)
	    value = WAC;
	else 
	   value = pow(value,(double)1/(cnt+1)) - 1;
	
	return value;
}

double MBS::getPrice() {
     return mbsPrice;
}

double MBS::getStdDev() {
     return stdDev;
}

double MBS::getStdErr() {
     return stdErr;
}

double MBS::getMaturity() {
	return T;
}

double MBS::getWAM() {
	return WAM;
}

double MBS::getWAC() {
	return WAC;
}

double MBS::getOAS() {
	return OAS;
}
