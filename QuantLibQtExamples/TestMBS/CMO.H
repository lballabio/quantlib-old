#ifndef _CMO__
#define _CMO__

#include <vector>
#include <algorithm>
#include <numeric>
#include "Constants.h"
#include "MBS.h"

using namespace std;

class Tranche 
{
  public:
        Tranche() {}
        Tranche(char clas, double balance, double coupon) 
          : initBalance_(balance), balance_(balance), coupon_(coupon),
              clas_(clas) {}
        virtual ~Tranche() {}
        double initBalance_;
        double balance_;
        double coupon_;
        vector<double> cashFlows_;
        vector<double> sumCF_;
        vector<double> inter_;
        vector<double> principal_;
        vector<double> discount_;
        vector<double> T_;
        double price_;
        double interest_;
        double princip_;
        double averageLife_;
        char clas_;
};
	
class CMO
{
  public:
        CMO(MBS m, vector<Tranche> tr) : mbs(m), tranche(tr)
        {
          for (int i = 0; i < tranche.size(); i++)
                collateral_.push_back(tranche[i].balance_);
        }
        virtual ~CMO() { }
        void calcTrancheCF();
        inline double calcCPR(double SMM) { return 100*(1-pow((1-
           (SMM/100)),12)); }
        inline double calcSMM(double scheduleBal, double actualBal) { 
          return 100*((double)(scheduleBal - actualBal)/scheduleBal); 
        }
        inline double calcPSA(double age, double CPR) { 

          return 100*((double)(CPR/(0.2*min(age,30)))); 
    }
    inline double calcRefinance(double r) {

          double WAC = mbs.getWAC();
          double a = (double) 0.5/2;
          double b = (double) 100*((0.5 - a)/(PI/2));
          double d = (double) 0.06/b;
          double c = (double) -d*0.02;
          return (double) (a + b*(atan(c + d*(WAC - r))));
        }
        inline double calcBurnout(int t, Tranche tr, double balance) { 

          return (double) (0.3 + 0.7*((double)balance/1000000)); 
        }
        inline double calcMP(int t, Tranche tr, double balance)  {  

          double WAC = mbs.getWAC();
          double WAM = mbs.getWAM();

          return balance*(((double)WAC/12)/(1-pow((1+(double)WAC/12),
             -WAM+t))); 
        }
        inline double calcIP(int t, Tranche tr, double r, 
           double balance) {    

	  double WAC = mbs.getWAC();

	  return (balance)*((double)(tr.coupon_/12));
        }
        inline double calcPP(int t, Tranche tr, double r, 
           double balance) {

          double SMM = calcSMM1(t,tr,r,balance);
          double SP = calcSP(t,tr,r,balance);

          return SMM*(balance - SP);
		
        }
        inline double calcMM(int t) {

          double MM[12] = { 0.94, 0.76, 0.74, 0.95, 0.98, 0.92, 0.98,
                             1.10, 1.18, 1.22, 1.23, 0.98};
          int rem =  t % 12;
			
          if (t == 1)
                rem = 1;
			
          return MM[rem-1];

        }
        inline double calcCPR1(int t, Tranche tr, double r, 
           double balance) {

	  double RI = calcRefinance(r);
	  double age = calcAge(t);
	  double MM = calcMM(t);
	  double BM = calcBurnout(t,tr,balance);

	  return RI*age*MM*BM;

        }
        inline double calcAge(int t) {
          return min((double)t/30,1);
        }
        inline double calcSMM1(int t,Tranche tr, double r, 
           double balance) {

          double CPR = calcCPR1(t,tr,r,balance);
			
          return (1 - pow((1 - CPR),(double)1/12));
        }
        inline double calcSP(int t, Tranche tr, double r, 
           double balance) {

          double MP = calcMP(t,tr,balance);
          double IP = calcIP(t,tr,r,balance);

          return MP - IP;
        }
        void calcCashFlows(double initRate, double financeRate, int N,
           int M);
  private:
        MBS mbs;
        vector<Tranche> tranche;
        vector<double> collateral_;
};

#endif 
