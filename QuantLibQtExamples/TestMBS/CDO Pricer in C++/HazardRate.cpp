#include "HazardRate.h"

void HazardRate::calcDiscountBonds()
{
	for (int i = 0; i <= EIGHTY_FOUR; i++)
		P.push_back(exp(-r[i]*T[i]));
}

double HazardRate::calcHazard(int num)
{
	double h = 0.01;
	double temp = h;
	double diff = 0.0;
	double f, f1 = 0.0;
	const double error = 0.000001;
	int cnt = 0;

	do
	{
		temp = h;
		f = calcFunc(h,num);
		f1 = calcDeriv(h,num);
		h = h - f/f1;
		diff = h - temp;
		cnt++;

	}
	while (absolute(diff) > error);

	haz.push_back(h);


	return h;
}

double HazardRate::calcFunc(double h,int num)
{

	double sum = 0;
	double sum1 = 0;
	double v = 0.0;
		
	if ((num > 0) && (num <= 1))
	{
		for (int i = 1; i <= 5; i++)
			sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

		for (int i = 1; i <= 12; i++)
			sum1 = sum1 + P[i-1]*(Q(num,h,T[i-1]) - Q(num,h,T[i]));
	
		v = 0.6*sum1 - 0.5*spread[0]*sum;
	}
	else if ((num > 1) && (num <= 3))
	{
		for (int i = 1; i <= 13; i++)
			sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

		for (int i = 1; i <= 36; i++)
			sum1 = sum1 + P[i-1]*(Q(num,h,T[i-1]) - Q(num,h,T[i]));

		v = R*sum1 - 0.5*spread[1]*sum;
		
	}
	else if ((num > 3) && (num <= 5))
	{
		for (int i = 1; i <= 21; i++)
			sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

		for (int i = 1; i <= 60; i++)
			sum1 = sum1 + P[i-1]*(Q(num,h,T[i-1]) - Q(num,h,T[i]));

		v = 0.6*sum1 - 0.5*spread[2]*sum;
		
	}
	else if ((num > 5) && (num <= 7))
	{
		for (int i = 1; i <= 29; i++)
			sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

		for (int i = 1; i <= 84; i++)
			sum1 = sum1 + P[i-1]*(Q(num,h,T[i-1]) - Q(num,h,T[i]));

		v = R*sum1 - 0.5*spread[3]*sum;
	}
	else if (num > 7)
	{
		for (int i = 1; i <= 41; i++)
			sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

		for (int i = 1; i <= 120; i++)
			sum1 = sum1 + P[i-1]*(Q(num,h,T[i-1]) - Q(num,h,T[i]));

		v = R*sum1 - 0.5*spread[4]*sum;
	}

	return v;
}

double HazardRate::calcDeriv(double h, int num)
{
	double sum = 0;
	double sum1 = 0;
	double v = 0.0;

	if ((num > 0) && (num <= 1))
	{
	  for (int i = 1; i <= 5; i++)
		sum = sum + delta[i-1]*P[i-1]*(T[i-1]*Q(num,h,T[i-1]) + T[i]*Q(num,h,T[i]));

	   for (int i = 1; i <= 12; i++)
		sum1 = sum1 + P[i-1]*(-T[i-1]*Q(num,h,T[i-1]) + T[i]*Q(num,h,T[i]));
	
	    v = R*sum1 - 0.5*spread[0]*sum;
	
             }
	else if ((num > 1) && (num <= 3))
	{
	  for (int i = 1; i <= 13; i++)
	      sum = sum + delta[i-1]*P[i-1]*((T[i-1]-1)*Q(num,h,T[i-1]) + (T[i]-
                       1)*Q(num,h,T[i]));

	  for (int i = 1; i <= 36; i++)
	       sum1 = sum1 + P[i-1]*(-(T[i-1]-1)*Q(num,h,T[i-1]) + (T[i]-1)*Q(num,h,T[i]));
		
	   v = R*sum1 - 0.5*spread[1]*sum;
	}
	else if ((num > 3) && (num <= 5))
	{
	    for (int i = 1; i <= 21; i++)
	    sum = sum + delta[i-1]*P[i-1]*((T[i-1]-1)*Q(num,h,T[i-1]) + (T[i]-
        1)*Q(num,h,T[i]));

	    for (int i = 1; i <= 60; i++)
	        sum1 = sum1 + P[i-1]*(-(T[i-1]-3)*Q(num,h,T[i-1]) + (T[i]-1)*Q(num,h,T[i]));
		
	     v = R*sum1 - 0.5*spread[2]*sum;
		
	}
	else if ((num > 5) && (num <= 7))
	{
	  for (int i = 1; i <= 29; i++)
	    sum = sum + delta[i-1]*P[i-1]*((T[i-1]-1)*Q(num,h,T[i-1]) + (T[i]-
       1)*Q(num,h,T[i]));

	   for (int i = 1; i <= 84; i++)
	     sum1 = sum1 + P[i-1]*(-(T[i-1]-5)*Q(num,h,T[i-1]) + (T[i]-1)*Q(num,h,T[i]));
		
	    v = 0.6*sum1 - 0.5*spread[3]*sum;
	}
	else if (num > 7)
	{
	   for (int i = 1; i <= 41; i++)
		sum = sum + delta[i-1]*P[i-1]*(Q(num,h,T[i-1]) + Q(num,h,T[i]));

	   for (int i = 1; i <= 120; i++)
	      sum1 = sum1 + P[i-1]*(-(T[i-1]-7)*Q(num,h,T[i-1]) + (T[i]-1)*Q(num,h,T[i]));

		v = R*sum1 - 0.5*spread[4]*sum;
	}

	return v;
 
}

double HazardRate::Q(int num, double h, double T)
{

	double q = 0.0;

	if ((num > 0) && (num <= 1))
		q = exp(-h*T);
	else if ((num > 1) && (num <= 3))
		q = exp(-haz[0] - h*(T-1));
	else if ((num > 3) && (num <= 5))
		q = exp(-haz[0] - 2*haz[1] - h*(T-3));
	else if ((num > 5) && (num <= 7))
		q = exp(-haz[0] - 2*haz[1] - 2*haz[2] - h*(T-5));
	else // assume hazard rate remains constant beyond 10Y maturity
		q = exp(-haz[0] - 2*haz[1] - 2*haz[2] - 2*haz[3] - h*(T-7));

	return q;
}

	