#include <fstream>
#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <math.h>
#include <map>

//#define SIZE_X 100
#include "MBS.h"
//#include "CMO.h"

int main()
{	
  std::cout.precision(7);
  double principal = 1000000;    // underlying principal (notional) of MBS
  double coupon = 0.08;             // coupon rate
  double WAC = 0.08;                // weighted average coupon rate
  double WAM = 10;                  // weighted average maturity
  double OAS = 0.02;                // option adjusted spread
  double initSpotRate = 0.06;       // spot rate
  double initRefinanceRate = 0.08;  // refinance rate
  int N = 10;                       // number of time steps in tree
  long int M = 100000;              // number of simulation paths
  MBS mbs(principal,coupon,WAC,WAM,OAS);

  std::cout << "Running Monte Carlo to price MBS..." << endl << endl;
  mbs.calcPrice(initSpotRate,initRefinanceRate,N,M);
  std::cout << "MBS Price = " << mbs.getPrice() << endl;
  std::cout << "Std Deviation = " << mbs.getStdDev() << endl;
  std::cout << "Std Error = " << mbs.getStdErr() << endl << endl;
	
  std::cout << "Pricing MBS with Simulations of Binomial Tree Paths..." << std::endl;
  MBS mbs1(principal,coupon,WAC,N,OAS);
  mbs1.buildTree(initSpotRate,coupon,N);
  //std::cout << "MBS Price = " << mbs1.getPrice() << endl;
  std::cout << "Std Deviation = " << mbs1.getStdDev() << endl;
  std::cout << "Std Error = " << mbs1.getStdErr() << endl << endl;

  /*
  vector<Tranche> tranche;
  Tranche trA('A',500000,0.06);
  tranche.push_back(trA);
  Tranche trB('B',300000,0.065);
  tranche.push_back(trB);
  Tranche trC('C',200000,0.07);
  tranche.push_back(trC);
  Tranche trZ('Z',100000, 0.075);
  tranche.push_back(trZ);

  std::cout << std::endl;
  std::cout << "Pricing CMO Tranches..." << std::endl << std::endl;
  CMO cmo(mbs,tranche);
  cmo.calcCashFlows(initSpotRate,initRefinanceRate,N,M);	
  */
}
