#include "CMO.h"
#include "Utility.h"

void CMO::calcCashFlows(double initRate, double financeRate, int N, int M)
{
   Utility util;
   int i, t = 0;
   double r = 0.0715;
   const double kappa = 0.29368;
   const double vol = 0.11;
   const double theta = 0.08;
   double deviate = 0;
   long seed = 0;
   long* idum = 0;
   double balance = 0;
   double sum = 0;
   double S[4] = {0};
   double sum1 = 0;
   double sum2 = 0;
   double sum3 = 0;
   double sumA = 0;
   double sumB = 0;
   double sumC = 0;
   double sumD = 0;
   double CPR = 0;
   double interest = 0.0;
   double mbsPrice = 0;
   double stdErr = 0;
   double stdDev = 0;
   double totalsum = 0;
   double totalsumA = 0;
   double totalsumB = 0;
   double totalsumC = 0;
   double totalsumD = 0;
   double totalsum2 = 0;
   double schedulePrincipal = 0;
   double prepaidPrincipal = 0;
   double discount = 0;
   double principal = 0;
   double pay = 0.0;
   double r1 = 0.0;
   double rr = 0.0;
   int cnt = 0;
   double trancheBal = 0.0;
   double T = mbs.getMaturity();
   double WAM = mbs.getWAM();
   double OAS = mbs.getOAS();
   double dt = (double) T/N;
   double interest1 = 0;
   vector<double> disc(0.0);
   vector<double> time1;
   TNT::Array1D<double> CF(SIZE_X);   // cash_flow 
   vector<double> p;

   srand(unsigned(time(0)));
   seed = (long) rand() % 100;
   idum = &seed;
	
   for (t = 1; t <= N; t++)
     time1.push_back((double)(t-1)/12);

   for (i = 0; i < M; i++)
   {
     r = initRate;	
     sum = 0;
     sumA = 0;
     sumB = 0;
     sumC = 0;
     sumD = 0;
     schedulePrincipal = 0;
     prepaidPrincipal = 0;
     balance = 1000000;
     cnt = 1;
     disc.clear();
     disc.empty();
     disc.push_back(r);
     p.clear();
     p.push_back(0);

     for (int j = 0; j < tranche.size(); j++)
     {
       tranche[j].balance_ = collateral_[j];
       tranche[j].inter_.clear();
       tranche[j].principal_.clear();
       trancheBal = calcPP(0,tranche[j],r,tranche[j].balance_) + 
          calcMP(0,tranche[j],tranche[j].balance_); 
       tranche[j].principal_.push_back(trancheBal);
       tranche[j].interest_ = calcIP(0,tranche[j],r,tranche[j].balance_);
       tranche[j].inter_.push_back(tranche[j].interest_);
       S[j] = 0;
     }
		
     for (t = 1; t <= N; t++)
     {
       balance = balance - (schedulePrincipal + prepaidPrincipal);			
       deviate = util.gasdev(idum);
       r = r + kappa*(theta - r)*dt + vol*r*sqrt(dt)*deviate;
       disc.push_back(r);
       interest = calcIP(t,tranche[cnt-1],r,balance);  
       schedulePrincipal = calcMP(t,tranche[cnt-1],balance); 
       prepaidPrincipal = calcPP(t,tranche[cnt-1],r,balance); 
       tranche[cnt-1].balance_ = tranche[cnt-1].balance_ - 
          schedulePrincipal - prepaidPrincipal;
       principal = schedulePrincipal + prepaidPrincipal;
       tranche[cnt-1].principal_.push_back(principal);
       tranche[cnt-1].princip_ = principal;
       p.push_back(principal);
			
       if (tranche[cnt-1].balance_ > 0)
         interest1 = calcIP(t,tranche[cnt-1],r,tranche[cnt-1].balance_);
       else
         interest1 = 0;
		
       tranche[cnt-1].inter_.push_back(interest1);	
       tranche[cnt-1].interest_ = interest1;

       for (int k = 1; k <= tranche.size(); k++) 	
       {
         if (k != cnt)
         {
            interest1 = calcIP(t,tranche[k-1],r,tranche[k-1].balance_);
						
            if (tranche[k-1].balance_ != 0 )
            {
              tranche[k-1].inter_.push_back(interest1);
              tranche[k-1].interest_ = interest1;
            }
            else
            {
              tranche[k-1].inter_.push_back(0.0);
              tranche[k-1].interest_ = 0.0;
            }
						
            tranche[k-1].principal_.push_back(0.0);
            tranche[k-1].princip_ = 0.0;
         }

         rr = mbs.computeZeroRates(t-1,disc);
         S[k-1] = (tranche[k-1].interest_ + tranche[k-1].princip_)/
            (pow(1+rr+OAS,(double)(t-1)/12));
				
         if (k == 1)
           sumA = sumA + S[k-1];
         else if (k == 2)
           sumB = sumB + S[k-1];
         else if (k == 3)
           sumC = sumC + S[k-1];
         else
           sumD = sumD + S[k-1];
       }				
			
       if (tranche[cnt-1].balance_ > 0) 
       {
         if (balance >= schedulePrincipal)  
         {
           if (t != N)
              CF[t-1] = schedulePrincipal + interest + prepaidPrincipal;	
           else
              CF[t-1] = interest + balance;

           rr = mbs.computeZeroRates(t-1,disc);
           sum = sum + CF[t-1]/(pow(1+rr+OAS,(double)(t-1)/12));			
         }
         else 
           goto x;
       }
       else
       {
         tranche[cnt-1].balance_ = 0;
         cnt++;
       }
     }
     x:
       totalsum = totalsum + sum;  
       totalsumA = totalsumA + sumA;
       totalsumB = totalsumB + sumB;
       totalsumC = totalsumC + sumC;
       totalsumD = totalsumD + sumD;
       totalsum2 = totalsum2 + sum*sum;
  }
   
  calcTrancheCF();
  for (int j = 0; j < tranche.size(); j++)
  {	
       sum1 = 0;
       sum2 = 0;
       for (i = 0; i < tranche[j].principal_.size(); i++) 
       {
         sum1 = sum1 + (time1[i])*(tranche[j].principal_[i]);
         sum2 = sum2 + tranche[j].principal_[i];
       }
       tranche[j].averageLife_ = sum1/sum2;
  }
  sum1 = 0;
  sum = accumulate(p.begin(),p.end(),0);
  for (j = 0; j < p.size(); j++)
       sum1 = sum1 + time1[j]*p[j];

  std::cout << endl;
  std::cout << "collateral price = " << totalsum/M <<  " " << "Ave.Life = "
     << sum1/sum << endl;	
  std::cout << "Tranche A price =  " << totalsumA/M << " " << "Ave.Life = "
     << tranche[0].averageLife_ << endl;
  std::cout << "Tranche B price =  " << totalsumB/M << " " << "Ave.Life = "
     << tranche[1].averageLife_ << endl; 				 
  std::cout << "Tranche C price =  " << totalsumC/M << " " << "Ave.Life = "
     << tranche[2].averageLife_ << endl;
  std::cout << "Tranche Z price =  " << totalsumD/M << " " << "Ave.Life = "
     << tranche[3].averageLife_ << endl;

  T = mbs.getMaturity();
  stdDev = sqrt(totalsum2 - (double)(totalsum*totalsum)/M)*
     (exp(-2*initRate*T)/(M-1));
  stdErr = (double) stdDev/sqrt(M);
}

void CMO::calcTrancheCF()
{
  vector<Tranche>::iterator iter;
  vector<double>::iterator iter1;
  vector<double>::iterator iter2;
  int cnt = 1;

  for (iter = tranche.begin(); iter != tranche.end(); iter++)
  {
     iter2 = iter->inter_.begin();
        cnt = 1;
        for (iter1 = iter->principal_.begin(); iter1 != 
           iter->principal_.end(); iter1++)
        {
          std::cout << "Mo." << cnt << "  Class: " << iter->clas_ 
            << " " << "Principal= " << *iter1 
            << " " << "Coupon= " << *iter2 << endl;
          iter2++;
          cnt++;
        }
  }
}
