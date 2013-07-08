// MBS.h: interface for the MBS class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MBS_H__76187F6C_FE6C_425F_97B6_7639548A3878__INCLUDED_)
#define AFX_MBS_H__76187F6C_FE6C_425F_97B6_7639548A3878__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <vector>
#include "math.h"
#include "time.h"
#include "Utility.h"
#include "TNT/tnt.h"
#define SIZE_X 100
#define SIZE_Y 100

using namespace std;
static TNT::Array2D<double> spotRate(SIZE_X,SIZE_Y);
static TNT::Array2D<double> discountRate(SIZE_X,SIZE_Y);

class MBS  
{

public:
	MBS();
	MBS(double principal, double coupon, double WAC, double WAM, double OAS) :
		faceValue(principal), coupon(coupon), WAC(WAC), WAM(WAM), OAS(OAS), 
		T(WAM) { }
	virtual ~MBS()  { }
	double calcPayment(double principal, double T);	// compute payment amount
	void calcPrice(double initRate, double financeRate, int N, long int M);
	double calcCPR(double rate);
	void buildTree(double initRate, double financeRate, int N);
	double computeZeroRates(int cnt, vector<double> rate);
	double calcSMM(double x);
	double getPrice();
	double getStdDev();
	double getStdErr();
	double getMaturity();
	double getWAM();
	double getWAC();
	double getOAS();
  private:
	double OAS;               // option adjusted spread
	double faceValue;	      // principal amount
	double coupon;            // coupon rate
	double WAM;				  // weighted average maturity
	double WAC;		          // weighted average coupon
	vector<double> zeroRates; // store discount zero coupon rates
	double T;		          // maturity of MBS	
	double mbsPrice;	      // price	
	double stdDev;		      // standard deviation
	double stdErr;		      // standard error	
};

#endif _MBS_H__

/*
class MBS  
{
public:
	MBS();
	MBS(double principal, double coupon, double WAC, double WAM, double OAS) :
	faceValue(principal), coupon(coupon), WAC(WAC), WAM(WAM), OAS(OAS), T(WAM)
	{ }
	virtual ~MBS();
class MBS  
{
     public:
	MBS() { }
	MBS(double principal, double coupon, double WAC, double WAM, double OAS) :
		faceValue(principal), coupon(coupon), WAC(WAC), WAM(WAM), OAS(OAS), 
T(WAM) { }
	virtual ~MBS()  { }
	double calcPayment(double principal, double T);	// compute payment amount
	void calcPrice(double initRate, double financeRate, int N, long int M);
	double calcCPR(double rate);
	double computeZeroRates(int cnt, vector<double> rate);
	double getPrice();
	double getStdDev();
	double getStdErr();
  private:
	double OAS;                          // option adjusted spread
	double faceValue;	      // principal amount
	double coupon;                     // coupon rate
double WAM;		      // weighted average maturity
	double WAC;		      // weighted average coupon
	vector<double> zeroRates; // store discount zero coupon rates
	double T;		     // maturity of MBS	
	double mbsPrice;	     // price	
	double stdDev;		    // standard deviation
	double stdErr;		    // standard error	
};

#endif _MBS_H__

	double calcCPR(double rate);
private:
	double OAS;         // option adjusted spread
	double faceValue;	// principal amount
	double coupon;   // coupon rate
	double WAM;		// weighted average maturity
	double WAC;		// weighted average coupon
	vector<double> zeroRates;
	double valuationMBS();

	double computeZeroRates(int N, vector<double> rate);
	double T;
};
*/

//#endif // !defined(AFX_MBS_H__76187F6C_FE6C_425F_97B6_7639548A3878__INCLUDED_)
