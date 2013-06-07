#include <wgz_utilities.hpp>

namespace QuantLib {

std::string versionInfo() {
	string res("PC Extensions Version 1.06 (build ");
	res+=__DATE__;
	res+=" ";
	res+=__TIME__;
	res+=") based on QuantLib 1.0.1";
	return res;
};

// converts a vector to an quantlib array
Array& vector2Array(const std::vector<Real>& v) {
	Array *a = new Array(v.size());
	for(int i=0;i<v.size();i++) (*a)[i]=v[i];
	return *a;
}

// converts an array to a vector
std::vector<double>& array2Vector(const Array& a) {
	std::vector<double> *v = new std::vector<double>(a.size());
	for(int i=0;i<a.size();i++) (*v)[i]=a[i];
	return *v;
}

// compute sin pi/2 kendalls tau as estimator for elliptic correlation
double ellipticCorrelation(const std::vector<Real>& d1, const std::vector<Real>& d2, const bool pearson) {

	int n=d1.size();
	QL_REQUIRE(d2.size()==n,"vectors are of different dimensions (" << d1.size() << "," << d2.size() << ")");
		
	

	if(pearson) {
		double mu1=0.0,mu2=0.0,var1=0.0,var2=0.0,cov=0.0;
		for(int i=0;i<n;i++) {
			mu1+=d1[i]; mu2+=d2[i];
		}
		mu1/=n; mu2/=n;
		for(int i=0;i<n;i++) {
			cov+=(d1[i]-mu1)*(d2[i]-mu2);
			var1+=(d1[i]-mu1)*(d1[i]-mu1);
			var2+=(d2[i]-mu2)*(d2[i]-mu2);
		}
		return cov/sqrt(var1*var2);
	}
	else {
		double sum=0.0;
		double t,res;
		for(int i=0;i<n;i++) {
			for(int j=0;j<i;j++) {
				t=(d1[j]-d1[i])*(d2[j]-d2[i]);
				if(t!=0.0) {
					sum+= t > 0.0 ? 1.0 : -1.0;
				}
			}
		}
		res=2.0* sum / (((double)n)*((double)(n-1)));
		return sin(M_PI/2.0*res);	
	}
	

}

// compute density of elliptic correlation estimation for a finite sample size using bootstrap with given number of paths  
boost::shared_ptr<KernelDensity> correlationDensity(const double correlation, const double nu, const int sampleSize, const int paths, const long seed, const bool pearson) {
	
	MersenneTwisterUniformRng mt(seed);

	vector<double> u(2),e(2);
	vector<double> d1(sampleSize),d2(sampleSize),sample(paths);
	double v;

	Matrix corr(2,2,1.0);
	corr[1][0]=correlation;
	corr[0][1]=correlation;

	TDistributionnd tnd(corr);

	for(int i=0;i<paths;i++) {
		for(int j=0;j<sampleSize;j++) {
			u[0] = mt.next().value;
			u[1] = mt.next().value;
			v = mt.next().value;
			e = tnd.copulaSample(nu,u,v);
			d1[j] = e[0];
			d2[j] = e[1];
		}
		sample[i] = ellipticCorrelation(d1,d2,pearson);
	}

	boost::shared_ptr<KernelDensity> res(new KernelDensity(sample,0.05,0.01)); // standard bandwidth for speed reasons, can be overwritten later by user
	return res;
}

std::vector<double> tLogLikelihood(const std::vector<Real>& x, const std::vector<Real>& y, const std::vector<Real>& nu, const int iterations) {
	
	double correlation = ellipticCorrelation(x,y,false);
	Matrix corr(2,2,1.0);
	corr[1][0]=correlation;
	corr[0][1]=correlation;

	KernelDensity k1(x,0.0,0.1,iterations); // distribution bandwidth is automatically computed, density bandwidth is not important
	KernelDensity k2(y,0.0,0.1,iterations); // distribution bandwidth is automatically computed, density bandwidth is not important
	
	// apply empirical cumulative function to normalize data
	vector<double> n1,n2;
	for(int i=0;i<x.size();i++) {
		n1.push_back(k1.cumulative(x[i]));
		n2.push_back(k2.cumulative(y[i]));
	}
	
	// compute likelihood
	vector<double> xl(2,0.0);
	vector<double> res;

	TDistribution1d t1d;
	TDistributionnd tnd(corr);

	for(int k=0;k<nu.size();k++) {

		double sum=0.0;

		for(int i=0;i<x.size();i++) {
			xl[0] = t1d.cumulativeInv(nu[k],n1[i]);
			xl[1] = t1d.cumulativeInv(nu[k],n2[i]);
			sum-=log(t1d.density(nu[k],xl[0]));
			sum-=log(t1d.density(nu[k],xl[1]));
			sum+=log(tnd.density(nu[k],xl));
		}

		res.push_back(sum);

	}
	return res;


}


bool smileArbitrageFree(const double forward, const double maturity, const vector<double>& strikes, const vector<double>& impliedVols) {
	
	QL_REQUIRE(strikes.size()==impliedVols.size(),"Strikes size (" << strikes.size() << ") must be equal to impliedVols size (" << impliedVols.size() << ")");
	
	//FILE *out=fopen("smile.log","a");
	const double tol = 1.0E-10;

	// negative strikes are ignored
	vector<double> c,s;
	int m;
	m=0;
	for(int i=0;i<strikes.size();i++) {
		if(i>0) QL_REQUIRE(strikes[i]>strikes[i-1],"Strikes must be strictly ascending, violated at i=" << i << " s(i)=" << strikes[i] << " s(i-1)=" << strikes[i-1]);
		if(strikes[i]>0.0) {
			c.push_back(blackFormula(Option::Call,strikes[i],forward,impliedVols[i]*sqrt(maturity)));
			s.push_back(strikes[i]);
			//fprintf(out,"s=%1.12f, c=%1.12f fwd=%f vol=%f mat=%f\n",s[m],c[m],forward,impliedVols[m],maturity);
			m++;
		}
	}

	

	for(int i=0;i<m;i++) {
		double t1 = i>0 ? (c[i]-c[i-1])/(s[i]-s[i-1]) : (c[i] - forward) / s[i];
		double t2 = i<m-1 ? (c[i+1]-c[i])/(s[i+1]-s[i]) : 0.0;
        //fprintf(out,"i=%d, t1=%1.12f, t2=%1.12f\n",i,t1,t2);
		if(t1<=-1.0-tol || t2>=tol || t1>=t2+tol) return false;
	}
	//fclose(out);

	if(c[m-1]<0.0) return false;

	return true;

}

bool smileArbitrageFree(const double forward, const vector<double>& s, const vector<double>& c, const int firstIndex, const int lastIndex, const int i) {

	const double tol = 1.0E-10;
	
	QL_REQUIRE(i>=firstIndex && i<=lastIndex,"Index to be checked (" << i << ") must be geq first index of smile (" << firstIndex << ") and leq last index of smile (" << lastIndex << ")");
	QL_REQUIRE(firstIndex>=0 && firstIndex<=lastIndex && lastIndex< s.size(),"First index (" << firstIndex << ") must be leq last index (" << lastIndex << ") lt strike vector length (" << s.size() << ")");
	
	double t1 = i>firstIndex ? (c[i]-c[i-1])/(s[i]-s[i-1]) : (c[i] - forward) / s[i];
	double t2 = i<lastIndex ? (c[i+1]-c[i])/(s[i+1]-s[i]) : 0.0;
	if(t1<=-1.0-tol || t2>=tol || (i>firstIndex ? t1>=t2+tol : false)) return false; // CHECK i>firstIndex ? ... : false 
	if(c[lastIndex] < 0.0) return false;
	return true;

}

vector<long> smileArbitrageFree(const double forward, const double maturity, const vector<double>& strikes, const vector<double>& impliedVols, const int aCentralPoint) {

	QL_REQUIRE(strikes.size()==impliedVols.size(),"Strikes size (" << strikes.size() << ") must be equal to impliedVols size (" << impliedVols.size() << ")");
	QL_REQUIRE(aCentralPoint>=0 && aCentralPoint < strikes.size(),"Central point index (" << aCentralPoint << ") must be geq 0 and lt strikes vector length (" << strikes.size() << ")");

	int centralPoint=aCentralPoint;
	
	// negative strikes are ignored
	vector<double> c,s;
	int m,correction;
	m=0; correction=0;
	for(int i=0;i<strikes.size();i++) {
		if(i>0) QL_REQUIRE(strikes[i]>strikes[i-1],"Strikes must be strictly ascending, violated at i=" << i << " s(i)=" << strikes[i] << " s(i-1)=" << strikes[i-1]);
		if(strikes[i]>0.0) {
			c.push_back(blackFormula(Option::Call,strikes[i],forward,impliedVols[i]*sqrt(maturity)));
			s.push_back(strikes[i]);
			m++;
		}
		else {
			centralPoint--;
			correction++;
		}
	}

	QL_REQUIRE(c.size()>0,"After removing negative strikes at least one point must remain");

	int minInd = centralPoint;
	int maxInd = centralPoint;

	QL_REQUIRE(smileArbitrageFree(forward,s,c,minInd,maxInd,centralPoint),"Smile consisting of central point only is not arbitrage free");

	// determine min index for which smile is af
	bool isAf=true;
	while(minInd>0 && isAf) {
		minInd--;
		isAf=isAf&smileArbitrageFree(forward,s,c,minInd,maxInd,minInd+1);
		isAf=isAf&smileArbitrageFree(forward,s,c,minInd,maxInd,minInd);
	}
	if(!isAf) minInd++;

	// determine max index for which smile is af
	if(!smileArbitrageFree(forward,s,c,minInd,maxInd,maxInd)) {
		isAf=false;
		while(maxInd>0 && !isAf) {
			maxInd--;
			isAf=smileArbitrageFree(forward,s,c,minInd,maxInd,maxInd);
		}
	}
	else {
		isAf=true;
		while(maxInd<m-1 && isAf) {
			maxInd++;
			isAf=isAf&smileArbitrageFree(forward,s,c,minInd,maxInd,maxInd-1);
			isAf=isAf&smileArbitrageFree(forward,s,c,minInd,maxInd,maxInd);
		}
		if(!isAf) maxInd--;
	}

	vector<long> result;
	result.push_back(minInd+correction);
	result.push_back(maxInd+correction);

	return result;
}

Date dateFromYYYYMMDD(const string& dateStr) {
	istringstream ds1(dateStr.substr(0,4));
	istringstream ds2(dateStr.substr(4,2));
	istringstream ds3(dateStr.substr(6,2));
	int year,month,day;
	ds1 >> year;
	ds2 >> month;
	ds3 >> day;
	Date result(day,Month(month),year);
	return result;
}

string YYYYMMDDFromDate(const Date& date) {
	ostringstream os;
	os << date.year();
	os << (((int)date.month() < 10) ? "0" : "");
	os << (int)date.month();
	os << (((int)date.dayOfMonth() < 10) ? "0" : "");
	os << (int)date.dayOfMonth();
	return os.str();
}

double annuity(boost::shared_ptr<Schedule> sched, const DayCounter& dc, boost::shared_ptr<YieldTermStructure> yts) {
	
	vector<Date> dates = sched->dates();
	
	double res=0.0;
	for(int i=1;i<dates.size();i++) {
		res+=yts->discount(dates[i])*dc.yearFraction(dates[i-1],dates[i]);			
	}

	return res;

}



/*! test function */
double testGamma2(const double a, const double b, const double x, const bool cumulative) {
	Gamma2Distribution g(a,b);
	if(cumulative)
		return g.cumulativeInv(x);
	else
		return g.density(x);
}


/*! test function */
std::vector<double> testMc(const long paths, const bool useLog, const long seed) {
	
	const int bn=50; // branchenmatrixgröße
	MersenneTwisterUniformRng mt(seed);
	vector<double> u(50),v,p(50);
	Matrix corr(50,50,0.0);
	
	TDistribution1d t1d;

	for(int j=0;j<50;j++) {
		u[j]=0.5;
		corr[j][j]=1.0;
	}

	TDistributionnd tnd(corr);

	for(int i=0;i<paths;i++) {
		v=tnd.copulaSample(0.0,u,mt.next().value);
		for(int j=0;j<50;j++) {
				p[j]=t1d.cumulativeInv(0.0,v[j]);
		}
		int d=0;
		for(int j=1;j<25000;j++) {
			if(mt.next().value <= p[5])
				d++;
		}
	}

	return v;

	/*InverseCumulativeNormal icnd;
	MersenneTwisterUniformRng mt(seed);
	IncrementalStatistics stat;

	for(int n=0;n<paths;n++) {
		double x=icnd(mt.next().value);
		stat.add(useLog? exp(x) : x);
	}

	std::vector<double> result(0);
	result.push_back(stat.mean());
	result.push_back(stat.variance());
	result.push_back(stat.skewness());
	result.push_back(stat.kurtosis());

	return result;*/

	/*GaussHermiteIntegration gh(paths);
	vector<double> a1 = array2Vector(gh.roots());
	vector<double> a2 = array2Vector(gh.weights());

	vector<double> a;
	for(int i=0;i<a1.size();i++) a.push_back(a1[i]);
	for(int i=0;i<a2.size();i++) a.push_back(a2[i]);
	return a;*/

}

/*! test function */
double sumMultiIndex(const std::vector<int> x) {
	double sum=0;
	for(int i=0;i<x.size();i++) {
		sum+=x[i];
		//sum+=gammaLn(complex<double>(((double)x[i])+1.0,0.0)).real();
	}
	return sum;
}

/*! test function */
int testMultiIndex2(const int depth, const double bound, Handle<PricingEngine> h) {

	//FILE *out=fopen("z:multiindex.csv","w");
	
	std::vector<int> x(depth,0);
	int k=0;
	long n=0;

	do {
		if(k==depth-1) {
			/*for(int i=0;i<depth;i++) {
				fprintf(out,"%d;",x[i]);
			}
			fprintf(out,"%1.12f\n",sumMultiIndex(x));*/
			n++;
			x[k]=x[k]+1;
		}
		if(sumMultiIndex(x)>bound) {
			for(int i=k;i<depth;i++) {
				x[i]=0;
			}
			k=k-1;
			if(k>=0) {
				x[k]=x[k]+1;
			}
		}
		else {
			if(k<depth-1) {
				k=k+1;
			}
		}
	} while(k>=0);

	//fclose(out);

	return n;

}

}

