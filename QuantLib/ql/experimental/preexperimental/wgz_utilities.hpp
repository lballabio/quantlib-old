/*! \file wgz_utilities.hpp
\brief Some stand alone utility functinos
Peter Caspers
*/

#ifndef quantlib_wgzutilities_hpp
#define quantlib_wgzutilities_hpp

#include <ql/quantlib.hpp>
#include <gammafunction.hpp>
#include <gamma2distribution.hpp>
#include <tDistributionnd.hpp>
#include <kernelDensity.hpp>
#include <stdio.h>
#include <iostream>
#include <sstream>

namespace QuantLib {

/*! gives version info */
std::string versionInfo();

/*! converts a vector to an ql array */
Array& vector2Array(const std::vector<Real>& v);

/*! converts an ql array to a vector */
std::vector<double>& array2Vector(const Array& a);

/*! compute elliptic correlation estimation sin(pi/2 * Kendall Tau) */
/* if pearson = true, the pearson estimator is used */
double ellipticCorrelation(const std::vector<Real>& x, const std::vector<Real>& y,const bool pearson=false);

/*! likelihood of t-copula for two data vectors */
/* correlation is estimated using ellipticCorrealtion (pearson = false) 
/* for nu=0.0 gauss logLikelihood is computed 
/* iterations refer to the number of iterations in kernel density estimation (bruce hansen), 4 is recommended */
std::vector<double> tLogLikelihood(const std::vector<Real>& x, const std::vector<Real>& y, const std::vector<Real>& nu, const int iterations=4);

/*! simulate density of (elliptic) correlation based on finite (sample size) t copula sample using a given number of paths */
/* if pearson = true, the pearson estimator is used */
boost::shared_ptr<KernelDensity> correlationDensity(const double correlation, const double nu, const int sampleSize, const int paths, const long seed, const bool pearson);

/*! check smile data for arbitrage */
bool smileArbitrageFree(const double forward, const double maturity, const vector<double>& strikes, const vector<double>& impliedVols);
bool smileArbitrageFree(const double forward, const vector<double>& s, const vector<double>& c, const int firstIndex, const int lastIndex, const int i);
vector<long> smileArbitrageFree(const double forward, const double maturity, const vector<double>& strikes, const vector<double>& impliedVols, const int centralPoint);

/*! convert YYYYMMDD string to QuantLib Date */
Date dateFromYYYYMMDD(const string& dateStr);

/*! convert QuantLib Date to YYYYMMDD string */
string YYYYMMDDFromDate(const Date& date);

/*! compute Annuity */
double annuity(boost::shared_ptr<Schedule> sched, const DayCounter& dc, boost::shared_ptr<YieldTermStructure> yts);

/*! test function */
std::vector<double> testMc(const long paths, const bool useLog, const long seed);

/*! test function 3 */
double testGamma2(const double a, const double b, const double x, const bool cumulative);

/*! test function 2 */
int testMultiIndex2(const int depth, const double bound, Handle<PricingEngine> h);

}

#endif
