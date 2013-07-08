#if !defined(AFX_UTILITY_H__7917ECC2_5AD5_4C05_B68C_AAE33CB8C3D4__INCLUDED_)
#define AFX_UTILITY_H__7917ECC2_5AD5_4C05_B68C_AAE33CB8C3D4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>

// /usr/include/c++/4.6/bits/sstream.tcc:112: error: expected unqualified-id before '(' token
// http://bytes.com/topic/c/answers/507592-any-tips-finding-problematic-line-code
#define max(a, b)  (((a) > (b)) ? (a) : (b))
#define min(a, b)  (((a) < (b)) ? (a) : (b))
#ifdef max
    #undef max
#endif
#ifdef min
    #undef min
#endif


#pragma warning ( disable : 4005)
#include <stdio.h>
#include <assert.h>
#include "Constants.h"
//#include "Deviates.h"
#include <math.h>


using namespace std;

class Utility 
{
public:
    Utility() {}
    virtual ~Utility() {}
	inline double normalCalc(double d)
	{
	const double a1 =  0.319381530;
	const double a2 = -0.356563782;
	const double a3 =  1.781477937;
	const double a4 = -1.821255978;
	const double a5 =  1.330274429;
	const double gamma = 0.2316419;
	const double k1 = 1/(1 + gamma*d);
	const double k2 = 1/(1 - gamma*d);
	const double normalprime = (1/(sqrt(2*PI)))*exp(-d*d/2);
	double value = 0.0;
	double h = 0.0;

	if (d >= 0)
		value = 1 - normalprime*(a1*k1 + a2*pow(k1,2) + a3*pow(k1,3) + a4*pow(k1,4) + a5*pow(k1,5));
	else
		value = normalprime*(a1*k2 + a2*pow(k2,2) + a3*pow(k2,3) + a4*pow(k2,4) + a5*pow(k2,5));

	return value;

	}

	inline double ran1(long *idum)
	{

	int j;
	long k;
	static long iy = 0;
	static long iv[NTAB];
	double temp;

	if (*idum <= 0 || !iy)
	{
		if (-(*idum) < 1) 
			*idum = 1;
		else
			*idum = -(*idum);
		for (j=NTAB+7; j >= 0; j--)
		{
			k = (*idum)/IQ;
			*idum = IA*(*idum-k*IQ)-IR*k;
			if (*idum < 0)
				*idum += IM;
			if (j < NTAB)
				iv[j] = *idum;
		}
		iy = iv[0];
	}

	k = (*idum)/IQ;
	*idum = IA*(*idum-k*IQ) - IR*k;
	if (*idum < 0)
		*idum += IM;
	j = iy/NDIV;
	iy = iv[j];
	iv[j] = *idum;
	if ((temp=AM*iy) > RNMX)
		return RNMX;
	else
		return temp;
	}

	inline double gasdev(long *idum)
	{
	static int iset = 0;
	static double gset;
	double fac, rsq, v1, v2;

	if (iset == 0)
	{
		do 
		{
			v1 = 2.0*ran1(idum)-1.0;
			v2 = 2.0*ran1(idum)-1.0;
			rsq = v1*v1 + v2*v2;
		}
		while (rsq >= 1.0 || rsq == 0.0);
		fac = sqrt(-2.0*log(rsq)/rsq);
		
		gset = v1*fac;
		iset = 1;
		return v2*fac;
	}
	else
	{
		iset = 0;
		return gset;
	}

	}

	inline double normalCalcPrime(double d)  
	{
	return (1/(sqrt(2*PI)))*exp(-d*d/2);
	}

	/*  The following uniform number generator was adapted from "Numerical Recipies in C",
        chapter 7.  The program was called ran1.  */
	inline double UniformRandom(double min, double max)
	{

	//AFX_MANAGE_STATE(AfxGetStaticModuleState());

	/*#define IA 16807
	#define IM 2147483647
	#define AM (1.0/IM)
	#define IQ 127773
	#define IR 2836
	#define NTAB 32
	#define NDIV (1+(IM-1)/NTAB)
	#define EPS 1.2e-7
	#define RNMX (1.0-EPS)
	*/
        
        int j;
        long k;
        static long iy = 0;
        static long iv[NTAB];
        static long idum;
        double temp;

        if(max <= min) {   /* Initialize the randon mumber generator  */
                idum = rand();
                if (idum < 1) idum=1;  /* do not allow idum to equal 0 */
                for (j=NTAB+7;j>=0;j--) {
                        k=(idum)/IQ;
                        idum=IA*(idum-k*IQ)-IR*k;
                        if (idum < 0) idum += IM;
                        if (j < NTAB) iv[j] = idum;
                }
                iy = iv[0];
        }
        k=(idum)/IQ;
        idum = IA*(idum-k*IQ)-IR*k;
        if (idum < 0) idum += IM;
        j = iy/(1+(IM-1)/NTAB);
        iy=iv[j];
        iv[j] = idum;
        if ((temp=AM*iy) > RNMX) return RNMX*(max-min)+min;
        else return temp*(max-min)+min;
	}

	/************************************************************************/
	inline double NormalRandom(double mu, double sigma)
	{

	//AFX_MANAGE_STATE(AfxGetStaticModuleState());

        static int iset=0;
        static double gset;
        double fac,rsq,v1,v2;

        if(iset == 0) {
                do {
                        v1=2.0*UniformRandom(0.0, 1.0)-1.0;
                        v2=2.0*UniformRandom(0.0, 1.0)-1.0;
                        rsq=v1*v1+v2*v2;
                } while (rsq >= 1.0 || rsq == 0.0);
                fac=sqrt(-2.0*log(rsq)/rsq);
                gset=v1*fac;
                iset=1;
                return mu + sigma*v2*fac;
        } else {
                iset=0;
                return mu + sigma*gset;
        }
	}


	/*  This function calculates F(x), that is the probability that X is less than x
	given that X is normally distributed with mean and var as provided, ie P(X<x).
	It uses a polynomial approximation that is described in Hull, p 243. */
	inline double NormalCDF(double x)
	{
    
    double k, N1;
    double gamma = 0.2316419;
    double a1 = 0.319381530;
    double a2 = -0.356563782;
    double a3 = 1.781477937;
    double a4 = -1.821255978;
    double a5 = 1.330274429;
    double pi = 3.1415927;
    
    if(x>=0.0) 
	{
        k = 1./(1.+gamma*x);
        N1 = exp(-(x*x)/2)/sqrt(2*PI);
        return(1.-N1*(a1*k + a2*k*k + a3*k*k*k + a4*pow(k,4.0) + a5*pow(k,5.0)));
	}
    else 
	{
        k = 1./(1.+gamma*-x);    /* x replaced with -x */
        N1 = exp(-(x*x)/2)/sqrt(2*PI);
        return(N1*(a1*k + a2*k*k + a3*k*k*k + a4*pow(k,4.0) + a5*pow(k,5.0)));
    }
	}

	/* This function calculates an approximation to the cumulative bivariate normal distribution 
   as described in Hulls book */
	inline double sgn(double x) 
	{
	if (x >= 0.0) 
		return 1.0;
	return -1.0;
	}

	inline double f(double x, double y, double aprime, double bprime, double rho) 
	{
	double r = aprime*(2*x - aprime) + bprime*(2*y-bprime) + 2*rho*(x - aprime)*(y - bprime);
	return exp(r);
	}

	inline double BivariateCDF(double a, double b, double rho) 
	{
	
	double A[4] = {0.3243030, 0.4211071, 0.1334425, 0.006374323 };
	double B[4] = {0.1337764, 0.6243247, 1.3425378,  2.2626645 };
	double sum = 0.0;
	int i,j;

	if ((a <= 0.0) && (b <= 0.0) && (rho <= 0.0)) 
	{
		double aprime = a/(sqrt(2.0*(1.0-rho*rho)));
		double bprime = b/(sqrt(2.0*(1.0-rho*rho)));

		for (i = 0; i < 4; i++)  
		{
			for (j = 0; j < 4; j++) 
			{
				sum += A[i]*A[j]*f(B[i],B[j],aprime,bprime,rho);
			}
		}
		sum = sum*(sqrt(1.0 - rho*rho)/PI);
		return sum;
	}
	else if (a*b*rho <= 0.0) 
	{
		if ((a <= 0.0) && (b >= 0.0) && (rho >= 0.0)) 
			return NormalCDF(a) - BivariateCDF(a,-b,-rho);
	}
	else if ((a >= 0.0) && (b <= 0.0) && (rho >= 0.0)) 
	{
		return NormalCDF(b) - BivariateCDF(-a, b, -rho);
	}
	else if ((a >= 0.0) && (b >= 0.0) && (rho <= 0.0))
	{
		return NormalCDF(a) + NormalCDF(b) - 1.0 + BivariateCDF(-a,-b,rho);
	}
	else if (a*b*rho >= 0.0) 
	{
		double denom = sqrt(a*a - 2*rho*a*b + b*b);
		double rho1 = ((rho*a - b)*sgn(a))/denom;
		double rho2 = ((rho*b - a)*sgn(b))/denom;
		double delta = (1.0 - sgn(a)*sgn(b))/4.0;

		return BivariateCDF(a,0.0,rho1) + BivariateCDF(b,0.0,rho2);
	}
	return -1;  // should never get here
	
	}
	
	/**************************************************************************/



	/************************************************************************/
	inline double CEV(double S, double K, double r, 
											double q, double T,
											double beta, int M, int N)
	{

//AFX_MANAGE_STATE(AfxGetStaticModuleState());

        int i=0, j=0, k=0, b=0;
        double Value = 0.0;     /* Value of option */
        double dt;              /* time increment, (yrs) */
        double St1;       /* Stock price at time t */
		double St2;       /* Antithetic stock price at time t */
		double delta;
        double e;
		double flag1;
		double flag2;

/*      The srand function sets the starting point for generating a series of 
        pseudorandom integers. To reinitialize the generator, use 1 as the seed 
        argument. Any other value for seed sets the generator to a random starting 
        point. rand retrieves the pseudorandom numbers that are generated. Calling 
        rand before any call to srand generates the same sequence as calling srand 
        with seed passed as 1.  */

        dt = T/(float)N;
        srand( 1 ); 
        UniformRandom(1.0, 0.0);  /* initialize random number generator  */     
        Value = 0.;
        delta = 0.2*pow(100.,-beta);
                for(i=0; i<M; i++)  // i is the path number
                {    
                        St1 = S;
						St2 = S;
						flag1 = 1.;
						flag2 = 1.;
                        for(k=0; k<N; k++) 
                        {  // k is the step along the path
                            e = NormalRandom(0., 1.);

	St1 = St1*exp((r-q-delta*delta*pow(St1,2.*beta)/2.)*dt+delta*pow(St1,beta)*sqrt(dt)*e);                                
	//St1 = St1+(r-q)*St1*dt+delta*pow(St1,1.+beta)*sqrt(dt)*e;
	//St1 = St1+((r-q)*St1-delta*delta*(1.+beta)*pow(St1,1.+2*beta)/2.)*dt+
	//      delta*pow(St1,1+beta)*sqrt(dt)*e+
	//	  delta*delta*(1.+beta)*pow(St1,1.+2*beta)*e*e*dt/2.;
	if(beta<-0.5 && St1 <0.1) flag1 = 0.;

	St2 = St2*exp((r-q-delta*delta*pow(St2,2.*beta)/2.)*dt+delta*pow(St2,beta)*sqrt(dt)*(-e));                                
	//St2 = St2+(r-q)*St2*dt+delta*pow(St2,1.+beta)*sqrt(dt)*-e;
	//St2 = St2+((r-q)*St2-delta*delta*(1.+beta)*pow(St2,1.+2*beta)/2.)*dt+
	//      delta*pow(St2,1+beta)*sqrt(dt)*-e+
	//	  delta*delta*(1.+beta)*pow(St2,1.+2*beta)*e*e*dt/2.;
	if(beta<-0.5 && St2 <0.1) flag2 = 0.;
						}
                        
                        Value = Value + flag1*exp(-r*T)*max(St1-K,0.0);
                        Value = Value + flag2*exp(-r*T)*max(St2-K,0.0);

                }
                
        return(Value/2./float(M));
	}

	/************************************************************************/
	inline double SVM(double S, double K, double r,
											double q, double T,
											double rho, double sigma, double alpha,
											double gama, int M, int N)
	{

	//AFX_MANAGE_STATE(AfxGetStaticModuleState());

	int i=0, j=0, k=0, b=0;
	double Value = 0.0;     /* Value of option */
	double dt;                      /* time increment, (yrs) */
	double St1, St2, St3, St4;       /* Stock price at time t */
	double Vt1, Vt2, Vt3, Vt4;        /* vol at time t */    
	double e1; 
	double e2;     

	/* The srand function sets the starting point for generating a series of 
   pseudorandom integers. To reinitialize the generator, use 1 as the seed 
   argument. Any other value for seed sets the generator to a random starting 
   point. rand retrieves the pseudorandom numbers that are generated. Calling 
   rand before any call to srand generates the same sequence as calling srand 
   with seed passed as 1.  */

	dt = T/(float)N;
	srand( 1 ); 
	UniformRandom(1.0, 0.0);  /* initialize random number generator  */     
	Value = 0.;
	for(i=0; i<M; i++)  // i is the path number
    {    
		St1 = S; St2 = S; St3 = S; St4 = S;
        Vt1 = sigma*sigma; Vt2 = Vt1; Vt3 = Vt1; Vt4 = Vt1;
        for(k=0; k<N; k++) 
        {  // k is the step along the path
			e1 = NormalRandom(0., 1.);
			e2 = NormalRandom(0., 1.);

            St1 = St1*(1+(r-q)*dt+sqrt(Vt1*dt)*e1);                            
            Vt1 = Vt1*(1+alpha*dt+gama*sqrt(dt)*(rho*e1+sqrt(1-rho*rho)*e2));

            St2 = St2*(1+(r-q)*dt+sqrt(Vt2*dt)*(-e1));                            
            Vt2 = Vt2*(1+alpha*dt+gama*sqrt(dt)*(rho*(-e1)+sqrt(1-rho*rho)*e2));

            St3 = St3*(1+(r-q)*dt+sqrt(Vt3*dt)*e1);                            
            Vt3 = Vt3*(1+alpha*dt+gama*sqrt(dt)*(rho*e1+sqrt(1-rho*rho)*(-e2)));

            St4 = St4*(1+(r-q)*dt+sqrt(Vt4*dt)*(-e1));                            
            Vt4 = Vt4*(1+alpha*dt+gama*sqrt(dt)*(rho*(-e1)+sqrt(1-rho*rho)*(-e2)));

        }
        Value = Value + exp(-r*T)*max(St1-K,0.0);
        Value = Value + exp(-r*T)*max(St2-K,0.0);
        Value = Value + exp(-r*T)*max(St3-K,0.0);
        Value = Value + exp(-r*T)*max(St4-K,0.0);
    }

    return(Value/4./float(M));

	}


	/************************************************************************/
	inline double StockPrice_SVM(double S, double r,
											double q, double T,
											double rho, double sigma, double alpha,
											double gama, int N)
	{

	//AFX_MANAGE_STATE(AfxGetStaticModuleState());

	int k=0;
	double dt;                      /* time increment, (yrs) */
	double V;        /* vol at time t */    
	double e1; 
	double e2;     

	/* The srand function sets the starting point for generating a series of 
   pseudorandom integers. To reinitialize the generator, use 1 as the seed 
   argument. Any other value for seed sets the generator to a random starting 
   point. rand retrieves the pseudorandom numbers that are generated. Calling 
   rand before any call to srand generates the same sequence as calling srand 
   with seed passed as 1.  */

	dt = T/(float)N;
//	srand( 1 ); 
	UniformRandom(1.0, 0.0);  /* initialize random number generator  */     
        V = sigma*sigma;
        for(k=0; k<N; k++) 
        {  // k is the step along the path
			e1 = NormalRandom(0., 1.);
			e2 = NormalRandom(0., 1.);

            S = S*(1+(r-q)*dt+sqrt(V*dt)*e1);                            
            V = V*(1+alpha*dt+gama*sqrt(dt)*(rho*e1+sqrt(1-rho*rho)*e2));

        }

    return(S);

	}

	/*****************************************************************************/
	inline void BlackScholes( double *kappa, double *call,
                  const double K, const double T, 
                  const double So, 
                  const double sigma, const double r, 
                  const double div)
	/*  this routine calculates an European call price and its derivative to volatility 
	using the Black-Scholes method given the following data:

K		The option strike price ($)
T		The time to maturity of the option (yrs)
sigma	A measure of stock volitility based on historical data
r		The risk free interest rate
div		The dividend expressed as an simple annual rate
call    The European call price
kappa   The derivative of European call with respect to the volatility
*/
                                                     
    {
    //AFX_MANAGE_STATE(AfxGetStaticModuleState());
    
    /* DECLARE LOCAL VARIABLES */
    double d1, d2;
    const double e = 2.7182818;
    const double pi = 3.1415927;
    
    
    d1 = (log(So/K)/log(e)+(r-div+sigma*sigma/2.)*T)/(sigma*sqrt(T));
    d2 = d1 - sigma*sqrt(T);
    
    
    // compute the theoretical European call price using B-S formula
    *call =(So*exp(-div*T)*NormalCDF(d1) - K*exp(-r*T)*NormalCDF(d2));
    
    // compute the derivative of the European call with respect to volatility
    *kappa = exp(-div*T)*So*sqrt(T)*exp(-d2*d2/2)/sqrt(2*pi);
    }

/*****************************************************************************/
double ImpVolatility(double K, double T, double So, 
					 double Cmarket, double r, double div)
/* This rountine uses Newton-Raphson Method to find the implied volatility for the 
European call option given the following data:

K		The option strike price ($)
T		The time to maturity of the option (yrs)
So      The current underlying asset price
Cmarket The current option price quoted at the market
r		The risk free interest rate
div		The dividend expressed as an simple annual rate
*/
{
    
    //AFX_MANAGE_STATE(AfxGetStaticModuleState());
    
    /* DECLARE LOCAL VARIABLES */
    double sigma = 0.10; // the implied volatility, the initial guess is 10%.
    const double acc = 0.00000001; // the desired accuracy 
    double call;        //The theoretical European call price from B-S formula
    double kappa;       //The derivative of European call with respect to the volatility
    int i = 0;          // the counter for the number of iteratations
	int MAXIT = 100;
    
    do{
        BlackScholes( &kappa, &call,K, T, So, sigma, r, div);
        
        sigma = sigma - (call - Cmarket)/kappa; // when sigma is very small, kappa may be zero
        // an improved algorithm needed.         
        i++;
        
        }while ( (fabs(call - Cmarket) > acc) && (i < MAXIT));
    if (i == MAXIT)  return 0;
    
    return sigma;
    
}

double _u( void )                                // uniform rng
   { 
	  long _M    = 0x7fffffff; // 2147483647 (Mersenne prime 2^31-1)
	  long _A    = 0x10ff5;    // 69621
      long _Q    = 0x787d;     // 30845
      long _R    = 0x5d5e;     // 23902
	  long _seed = -7;
      long _F    = 1. / _M;
	  int _NTAB = 32;         // arbitrary length of shuffle table
      long _DIV = 1+(_M-1)/NTAB;
      long k = _seed / _Q;                          // seed = ( A*seed ) % M
      long         _table[ NTAB ];                 // shuffle table of seeds
	  long         _next = 0;                      // seed to be used as index into table
	  
	  _seed = _A * ( _seed - k * _Q ) - k * _R;     // without overflow by
      if ( _seed < 0 ) _seed += _M;                 // Schrage's method

      int index = _next / _DIV;                     // Bays-Durham shuffle
      _next = _table[ index ];                      // seed used for next time
      _table[ index ] = _seed;                      // replace with new seed
   
      return _next * _F;                            // scale value within [0,1)
}

inline double exponential( double a = 0., double c = 1. )   // Exponential
{                                                    // location a, shape c
   assert( c > 0.0 );
   return a - c * log( _u() );
}
   
inline double gammaDeviate( double a, double b, double c )  // Gamma
{                                                           // location a, scale b, shape c
      assert( b > 0. && c > 0. );
   
      const double A = 1. / sqrt( 2. * c - 1. );
      const double B = c - log( 4. );
      const double Q = c + 1. / A;
      const double T = 4.5;
      const double D = 1. + log( T );
      const double C = 1. + c / E;
   
      if ( c < 1. ) {  
         while ( true ) {
            double p = C * _u();      
            if ( p > 1. ) {
               double y = -log( ( C - p ) / c );
               if ( _u() <= pow( y, c - 1. ) ) return a + b * y;
            }
            else {
               double y = pow( p, 1. / c );
               if ( _u() <= exp( -y ) ) return a + b * y;
            }
         }
      }
      else if ( c == 1.0 ) return exponential( a, b );
      else {
         while ( true ) {
            double p1 = _u();
            double p2 = _u();
            double v = A * log( p1 / ( 1. - p1 ) );
            double y = c * exp( v );
            double z = p1 * p1 * p2;
            double w = B + Q * v - y;
            if ( w + D - T * z >= 0. || w >= log( z ) ) return a + b * y;
         }
      }
}

/*
inline double gammaPDF(unsigned int n, int x)
{

	double logsum = 0.0;  // initialize sum of logs

	for (int i = 1; i <= n-1; i++)
		logsum += log(i);

	printf("exp = %0.3f\n",exp(-x + (n-1)*log(x) - logsum));
	return exp(-x + (n-1)*log(x) - logsum);
}


inline double gammaDensity1(double price, double strike, double vol, double rate, double t, double skew)
{
	double lambda = 1/(2*(1 - skew));
	double volhat = vol*pow(price,1-skew);
	printf("lambda = %0.3f\n",lambda);
	printf("volhat = %0.3f\n",volhat);
	double x = ((2*lambda*log(rate))/((volhat*volhat)*(pow(rate, t/lambda) - 1)))*pow(price,1/lambda)*pow(rate,t/lambda);
	double y = ((2*lambda*log(rate))/((volhat*volhat)*(pow(rate, t/lambda) - 1)))*pow(strike,1/lambda);
	double sum = 0.0;
	printf("x = %0.3f   y = %0.3f\n",x,y);


	for (int i = 1; i <= 50; i++)
	{
		//printf("gd = %0.3f   cdf = %0.3f\n",gammaPDF(i,x),gsl_ran_gamma_pdf(y,i+lambda,1));
		//printf("gammapdf = %0.3f\n",gammaPDF(2,x));
		sum += gammaPDF(i,x)*gsl_ran_gamma_pdf(y,i+lambda,1);
		printf("computing gammaDensity1. %d..\n",i);
	}

	return sum;
}

inline double gammaDensity2(double price, double strike, double vol, double rate, double t, double skew)
{

	double lambda = 1/(2*(1 - skew));
	double volhat = vol*pow(price,1-skew);
	double x = ((2*lambda*log(rate))/((volhat*volhat)*(pow(rate, t/lambda) - 1)))*pow(price,1/lambda)*pow(rate,t/lambda);
	double y = ((2*lambda*log(rate))/((volhat*volhat)*(pow(rate, t/lambda) - 1)))*pow(strike,1/lambda);
	double sum = 0.0;
	
	printf("Inside gammaDensity2...\n");
	for (int i = 1; i <= 50; i++)
	{
		printf("gd = %0.3f   cdf = %0.3f\n",gammaPDF(i+lambda,x),gsl_ran_gamma_pdf(y,i,1));
		sum += gammaPDF(i+lambda,x)*gsl_ran_gamma_pdf(y,i,1);
		printf("computing gammaDensity2 %d...\n",i);
	}

	return sum;



}
*/

inline int poisson( double mu )   // Poisson
{
      assert ( mu > 0. );
   
      double a = exp( -mu );
      double b = 1.;
   
      int i;
      
	  for ( i = 0; b >= a; i++ ) 
		  b *= _u();   
		
	  return i - 1;
}

};

#endif


/*
#include <math.h>
#include "Constants.h"

class Distribution
{
public:
	Distribution() {};
	~Distribution() {};
	double n(double z);                             // normal distribution function
	double n(double r,double mu, double sigmasqr);  // normal distribution function    
	double N(double z);                             // cumulative probability of normal
	double N(double a, double b, double rho);       // cum prob of bivariate normal
	double random_normal(void);  // normal number generation mean 0, variance 1
	double random_normal(const double mu, const double sigma);
	double random_log_normal(const double mu, const double sigma);
	double random_uniform(void);                     // uniform [0,1)
	double random_uniform(double low, double hi);    // uniform [low,high)
	double normalCalcPrime(double d);
	double ran1(long *idum);
	double gasdev(long *idum);
	double normalCalc(double d);
};
*/
