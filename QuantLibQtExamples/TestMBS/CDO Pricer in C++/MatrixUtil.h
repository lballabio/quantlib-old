#ifndef _DEVIATES
#define _DEVIATES

#include "tnt.h"
#include "jama_cholesky.h"
#include "jama_eig.h"
#include "Constants.h"
#include "Random.h"
#include <vector>
#include <time.h>
#include <math.h>
#include <stdlib.h>

using namespace TNT;
using namespace JAMA;
using namespace std;

class MatrixUtil
{
private:
	unsigned long seed;
public:
	MRNG mrng;
	MatrixUtil() { 
		srand(unsigned(time(0))); 
		do
		{
  		  seed = unsigned(time(0));
		}
		while (seed == 0);

		mrng.sgenrand(seed);
	}
	~MatrixUtil() {};

inline double ran(long *idum)
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
		for (int j = NTAB+7; j >= 0; j--)
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

inline double NormalDeviate(long *idum)
{
	static int iset = 0;
	static double gset;
	double fac, rsq, v1, v2;

	if (iset == 0)
	{
		do 
		{
			v1 = 2.0*ran(idum)-1.0;
			v2 = 2.0*ran(idum)-1.0;
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

inline TNT::Array1D<double> genEigenValue(TNT::Array2D<double>& R)
{
	int m = R.dim1();

	TNT::Array2D<double> lb(m,m);  // lower-banded (lb) matrix
	double deviate = 0.0;
	double normdeviate = 0.0;
	unsigned long seed = 0.0;
	double sum = 0.0;
	double dt = 0.25;
	TNT::Array1D<double> dw(m);
	TNT::Array1D<double> z(m);
	TNT::Array1D<double> D(m);
	TNT::Array2D<double> V(m,m);
	vector<double> dz;
	vector<double> eigenValue;
	vector<double> eigenVector[MAX_SIZE];
	vector<double>::iterator eigenVecIter;

	int i, j;
	JAMA::Eigenvalue<double> eig(R);
	// get eignvalues
	D = eig.getRealEigenvalues();
	// get eigenvectors
	V = eig.getV();
	// store eigenvalues
	for (int i = 0; i < m; i++)
		eigenValue.push_back(D[i]);

	// stores rows of eigenvectors so that we an compute
	// dz[i] = v[i][1]*sqrt(eigenvalue[1])*dw1+ v[i][2]*sqrt(eigenvalue[2])*dw2 + ...
	for (int i = 0; i < m; i++)
		for (int j = 0; j < m; j++)
			eigenVector[i].push_back(V[i][j]);


	// generate uncorrelated deviates
	for (int i = 0; i < m; i++)
	{
		normdeviate = mrng.genrand();
 	    deviate = normsinv(normdeviate);
		dw[i] = deviate; //sqrt(dt);
	}
	// generate correlated deviates
	for (int i = 0; i < m; i++)
	{
		sum = 0;
		eigenVecIter = eigenVector[i].begin();
		for (int j = 0; j < m; j++)
		{
			sum += (*eigenVecIter)*sqrt(eigenValue[j])*dw[j];
			   eigenVecIter++;
		}
		z[i] = sum;
	}
	return z;
}

inline TNT::Array1D<double> genCholesky(TNT::Array2D<double> R)
{
	int m = R.dim1();
	std::vector<double> normaldev;
	TNT::Array2D<double> lb(m,m);  // lower-banded (lb) matrix
	TNT::Array1D<double> dw(m);
	TNT::Array1D<double> z(m);
	double deviate = 0.0;
	double sum = 0.0;
	double dt = 0.25;
	double normdeviate = 0.0;
	int i, j;

	JAMA::Cholesky<double> ch(R);
	lb = ch.getL();

	// generate uncorrelated deviates
	for (int i = 0; i < m; i++)
	{
		normdeviate = mrng.genrand();
 	    deviate = normsinv(normdeviate);
		dw[i] = deviate;
	}

	// generate correlated deviates
	for (int i = 0; i < m; i++)
	{
		sum = 0;
		for (int j = 0; j < m; j++)
		{
			sum = sum + lb[i][j]*dw[j];
		}
		z[i] = sum;
		//normaldev.push_back(normalCalc(z[i]));
	}
	return z;
}
 

inline TNT::Array1D<double> genCholesky1(TNT::Array2D<double> R)
{
	int m = R.dim1();
	TNT::Array2D<double> lb(m,m);  // lower-banded (lb) matrix
	double deviate = 0.0;
	JAMA::Cholesky<double> ch(R);
	TNT::Array1D<double> dw(m);
	TNT::Array1D<double> z(m);
	double sum = 0.0;
	double dt = 0.25;
	double normdeviate = 0.0;
	int i, j;
 
	lb = ch.solve(R);
	// generate uncorrelated deviates
	for (int i = 0; i < m; i++)
	{
		normdeviate = mrng.genrand();
 	    deviate = normsinv(normdeviate);
		dw[i] = deviate;   //sqrt(dt);
	}

	// generate correlated deviates
	for (int i = 0; i < m; i++)
	{
		sum = 0;
		for (int j = 0; j < m; j++)
		{
			sum = sum + lb[i][j]*dw[i];
		}
		z[i] = sum;
	}

	return z;
}

/*
inline vector<double> genCholesky4(TNT::Array2D<double> R)
{
	int m = R.dim1();
	TNT::Array2D<double> lb(m,m);  // lower-banded (lb) matrix
	double deviate = 0.0;
	double dt = 0.25;
	TNT::Array1D<double> dw(m);
	vector<double> z;

	//double* z = new double[m];
	double sum = 0.0;
	double normdeviate = 0.0;
	int i, j;
	JAMA::Cholesky<double> ch(R);
	lb = ch.getL();

	// generate uncorrelated deviates
	for (int i = 0; i < m; i++)
	{
		normdeviate = mrng.genrand();
 	    deviate = normsinv(normdeviate);
		dw[i] = deviate*sqrt(dt);
	}

	// generate correlated deviates
	for (int i = 0; i < m; i++)
	{
		sum = 0;
		for (int j = 0; j < m; j++)
		{
			sum = sum + lb[i][j]*dw[j];
		}
		z.push_back(sum);
	}
	
	return z;
}
*/ 

inline vector<double> genCholesky4(TNT::Array2D<double> R)
{
	int m = R.dim1();
	std::vector<double> normaldev;
	std::vector<double> defaultTime;
	TNT::Array2D<double> lb(m,m);  // lower-banded (lb) matrix
	TNT::Array1D<double> dw(m);
	TNT::Array1D<double> z(m);
	double deviate = 0.0;
	double sum = 0.0;
	double dt = 0.25;
	double normdeviate = 0.0;
	int i, j;

	JAMA::Cholesky<double> ch(R);
	lb = ch.getL();

	// generate uncorrelated deviates
	for (int i = 0; i < m; i++)
	{
		normdeviate = mrng.genrand();
 	    deviate = normsinv(normdeviate);
		dw[i] = deviate;
	}

	// generate correlated deviates
	for (int i = 0; i < m; i++)
	{
		sum = 0;
		for (int j = 0; j < m; j++)
		{
			sum = sum + lb[i][j]*dw[j];
		}
		z[i] = sum;
		normaldev.push_back(normalCalc(z[i]));
	}

	return normaldev;
}

inline double normsinv(double p)
{
double x;
double q, r;
if ((0 < p )  && (p < P_LOW)){
   q = sqrt(-2*log(p));
   x = (((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+1);
}
else{
        if ((P_LOW <= p) && (p <= P_HIGH)){
           q = p - 0.5;
           r = q*q;
           x = (((((A1*r+A2)*r+A3)*r+A4)*r+A5)*r+A6)*q /(((((B1*r+B2)*r+B3)*r+B4)*r+B5)*r+1);
        }
        else{
                if ((P_HIGH < p)&&(p < 1)){
                   q = sqrt(-2*log(1-p));
                   x = -(((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+1);
                }
        }
}

/* If you are compiling this under UNIX OR LINUX, you may uncomment this block for better accuracy.
if(( 0 < p)&&(p < 1)){
   e = 0.5 * erfc(-x/sqrt(2)) - p;
   u = e * sqrt(2*M_PI) * exp(x*x/2);
   x = x - u/(1 + x*u/2);
}
*/

return x;
}

/*
 * A normally distributed random number generator.  We avoid
 * the uniform rv's being 0.0 since this will result in infinte
 * values, and double count the 0 == 2pi.
 */
double random_normal() {
 static int i = 1;
 static double u[2] = {0.0, 0.0};
 register double r[2];

 if (i == 1) {
  r[0] = sqrt(-2*log((double)(rand()+1)/(double)(RAND_MAX+1)));
  r[1] = 2*M_PI*(double)(rand()+1)/(double)(RAND_MAX+1);
  u[0] = r[0]*sin(r[1]);
  u[1] = r[0]*cos(r[1]);
  i = 0;
 } else {
  i = 1;
 }

 return u[i];
}

/*
 * The standard normal PDF, for one random variable.
 */
inline double stdnormal_pdf(double u)
{
    return exp(-u*u/2)/M_SQRT2PI;
}


/*  This function calculates F(x), that is the probability that X is less than x
given that X is normally distributed with mean and var as provided, ie P(X<x).
It uses a polynomial approximation that is described in Hull, p 243. */
double normalCDF(double x)
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


double normalCalc(double d)
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

};

#endif