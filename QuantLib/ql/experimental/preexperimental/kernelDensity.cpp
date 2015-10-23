#include <kernelDensity.hpp>

using namespace std;

namespace QuantLib {

	KernelDensity::KernelDensity(const vector<double>& data, const double hDist, const double hDens, const int iterations,const int steps, const double factor) {
		data_ = vector<double>(data);
		setup(hDist,hDens,iterations,steps,factor);
	}

	void KernelDensity::setup(const double hDist, const double hDens, const int iterations,const int steps, const double factor) {

		n_ = data_.size();
		cnd_ = new CumulativeNormalDistribution();
		icn_ = new InverseCumulativeNormal();

		minCalculated_=false;
		maxCalculated_=false;

		fastCalculated_=false;

		momentsCalculated_=false;
		moments_=vector<double>(6);

		weightedMomentsCalculated_=false;
		weightedMoments_=vector<double>(2);

		pointMassDensityCalculated_=false;

		if(hDist==0.0) {
			h_ = optimalBandwidth(iterations);
		}
		else {
			h_=hDist;
		}
		if(hDens==0.0) {
			h2_ = optimalBandwidth2(steps,factor);
		}
		else {
			h2_=hDens;
		}

	}

	vector<double> KernelDensity::data() {
		return data_;
	}


	double KernelDensity::minData() {
		if(!minCalculated_) {
			minData_=data_[0];
			for(int i=1;i<data_.size();i++) {
				if(data_[i]<minData_)
					minData_=data_[i];
			}
			minCalculated_=true;
		}
		return minData_;
	}

	double KernelDensity::maxData() {
		if(!maxCalculated_) {
			maxData_=data_[0];
			for(int i=1;i<data_.size();i++) {
				if(data_[i]>maxData_)
					maxData_=data_[i];
			}
			maxCalculated_=true;
		}
		return maxData_;
	}

	double KernelDensity::setDensityBandwidth(const double hDens, const int steps, const double factor) {
		if(hDens>0.0) h2_ = hDens;
		else h2_ = optimalBandwidth2(steps,factor);
		return h2_;
	}

	double KernelDensity::setDistributionBandwidth(const double hDist, const int iterations) {
		if(hDist>0.0) h_ = hDist;
		else h_= optimalBandwidth(iterations);
		return h_;
	}

	double KernelDensity::densityBandwidth() {
		return h2_;
	}

	double KernelDensity::distributionBandwidth() {
		return h_;
	}

	double KernelDensity::density(const double x, const double h, const int leaveOut) {

		double hs = h2_;
		if(h>0.0) hs=h;

		double sum=0.0;

		if(leaveOut>=0) {
			for(int i=0;i<n_;i++) {
				if(i!=leaveOut)
					sum+=kernel((x-data_[i])/hs);
			}
			return sum / ((double)(n_-1)*hs);
		}
		else {
			for(int i=0;i<n_;i++) {
				sum+=kernel((x-data_[i])/hs);
			}
			return sum / ((double)n_*hs);
		}

	}

	double KernelDensity::cumulative(const double x, const double h) {

		double hs = h_;
		if(h>0.0) hs=h;

		double sum=0.0;
		
		for(int i=0;i<n_;i++) {
			sum+=kernelF((x-data_[i])/hs);
		}

		return sum / ((double)n_);

	}

	double KernelDensity::fastCumulativeInv(double p) {
		unsigned int a=0,b=fastx_.size()-1,c;
		if(p<=fastx_[0]) return fasty_[0]+(fasty_[0]-fasty_[1])/(fastx_[0]-fastx_[1])*(p-fastx_[0]);
		if(p>=fastx_[b]) return fasty_[b]+(fasty_[b-1]-fasty_[b])/(fastx_[b-1]-fastx_[b])*(p-fastx_[b]);
		do {
			if(b-a < 10) {
				while(p>fastx_[a]) a++;
				return fasty_[a-1]+(fasty_[a]-fasty_[a-1])/(fastx_[a]-fastx_[a-1])*(p-fastx_[a-1]);
			}
			else {
				c = (a+b)/2;
				if(p>=fastx_[c]) a=c; else b=c;
			}
		} while(true);
	}

	void KernelDensity::preCalculateCumulativeInv() {
		fastx_.clear();
		fasty_.clear();
		double p=0.0,pt;
		double step=1.0/((double)fastBins_);
		for(int i=0;i<fastBins_-1;i++) {
			p+=step;
			//pt=p; // uniform
			pt=0.5*(sin(M_PI/2.0*(2.0*p-1))+1);
			fastx_.push_back(pt);
			fasty_.push_back(cumulativeInverse(pt,fastH_,false));
		}
		fastCalculated_=true;
	}

	double KernelDensity::cumulativeInverse(const double p, const double h, bool fast, const long bins, const double accuracy) {
		if(fast) {
			if(!fastCalculated_ || h!=fastH_ || bins!=fastBins_) {
				fastH_=h;
				fastBins_=bins;
				preCalculateCumulativeInv();
			}
			return fastCumulativeInv(p);
		}
		InvHelper hlp(this,p,h);
		Brent b;
		return b.solve(hlp,accuracy,0.0,(maxData()-minData())/100.0);
	}

	double KernelDensity::normalCumulativeInv(const double p, const int start, const double lambda) {
		double x = icn_->operator()(p);
		vector<double> m;
		if(start==0) {
			m = moments();
		}
		else {
			m = weightedMoments(start,lambda);
		}
		return sqrt(m[1])*x+m[0];
	}

	vector<double> KernelDensity::moments() {

		if(momentsCalculated_ && mH_==h_) return moments_;
		
		// 1st (kernel must have 1st moment 0)
		double m1=0.0;
		for(int i=0;i<n_;i++) {
			m1+=data_[i];
		}
		m1/=(double)n_;
		
		// 2nd (kernel must have 2nd moment 1)
		double m2=0.0,m2r=0.0;
		for(int i=0;i<n_;i++) {
			m2+=(data_[i]-m1)*(data_[i]-m1);
		}
		m2/=(double)n_;
		m2r=m2;
		m2+=h_*h_;

		// 3rd (kernel must have 3rd moment 0)
		double m3=0.0;
		for(int i=0;i<n_;i++) {
			m3+=(data_[i]-m1)*(data_[i]-m1)*(data_[i]-m1);
		}
		m3/=(double)n_;

		// 4th (kernel must have 4rd moment 3)
		double m4=0.0,m4r=0.0;
		for(int i=0;i<n_;i++) {
			m4+=(data_[i]-m1)*(data_[i]-m1)*(data_[i]-m1)*(data_[i]-m1);
		}
		m4/=(double)n_;
		m4r=m4;
		m4+=6.0*h_*h_*(m2*m2-m1*m1);

		moments_[0]=m1;
		moments_[1]=m2;
		moments_[2]=m3;
		moments_[3]=m4;
		moments_[4]=m2r;
		moments_[5]=m4r;

		momentsCalculated_=true;
		mH_=h_;

		return moments_;
	}

	vector<double> KernelDensity::weightedMoments(int start, double lambda) {
		
		if(weightedMomentsCalculated_ && wmStart_==start && wmLambda_==lambda) {
			return weightedMoments_;
		}

		if(start==0) start=data_.size();
		
		// 1st (all data points)
		double m1=0.0;
		for(int i=0;i<n_;i++) {
			m1+=data_[i];
		}
		m1/=(double)n_;
		
		// 2nd start value
		double m2=0.0;
		for(int i=0;i<start;i++) {
			m2+=(data_[i]-m1)*(data_[i]-m1);
		}
		m2/=(double)n_;

		// 2nd weighting
		for(int i=start;i<n_;i++) {
			m2 = lambda*m2 + (1-lambda)*(data_[i]-m1)*(data_[i]-m1);
		}

		weightedMoments_[0]=m1;
		weightedMoments_[1]=m2;
		weightedMomentsCalculated_=true;
		wmStart_=start;
		wmLambda_=lambda;

		return weightedMoments_;
		
	}

	boost::shared_ptr<KernelDensity> KernelDensity::quantileDensity(const double p, const long numberOfBatches) {
		
		vector<double> qData_(numberOfBatches,0.0);

		long n2 = n_ / numberOfBatches;
		double h2 = h_ * pow(((double)n_)/((double)n2),1.0/3.0); // distribution bandwidth for single batch
		
		vector<double> batchData_(n2,0.0);

		for(int i=0;i<numberOfBatches;i++) {
			// fill in batch data
			for(int j=0;j<n2;j++) {
				batchData_[j] = data_[i*n2+j];
			}
			KernelDensity kLocal(batchData_,h2,h2); // density bandwidth does not matter for quantile computation
			qData_[i]=kLocal.cumulativeInverse(p);
		}

		return boost::shared_ptr<KernelDensity>(new KernelDensity(qData_)); // automatic bandwidth selection, since only few points will be in here

	}

	
	double KernelDensity::kernel(const double x) {
		//return 1.0/(M_PI*(1.0+x*x)); // cauchy kernel
		return normalDensity(x); // gauss kernel
	}

	double KernelDensity::kernelF(const double x) {
		//return 1.0/M_PI*atan(x)+0.5; // cauchy kernel
		return cnd_->operator()(x); // gauss kernel
	}

	double KernelDensity::kernelC(const double x) {
		return exp(-x*x/4.0)/sqrt(4.0*M_PI);
	}

	double KernelDensity::kernelPsi() {
		//return ????; // cauchy kernel
		return 1.0/sqrt(M_PI); // gauss kernel
	}

	double KernelDensity::hermiteP(const int m, const double x) {
		/*if(m==0) return 1.0;
		if(m==1) return x;
		// m>=2
		return x*hermiteP(m-1,x)-((double)(m-1))*hermiteP(m-2,x);*/
		switch(m) {
			case 0:	return 1.0;
					break;
			case 1:	return x;
					break;
			case 2:	return x*x-1.0;
					break;
			case 3:	return x*x*x-3.0*x;
					break;
			case 4:	return x*x*x*x-6.0*x*x+3.0;
					break;
			case 5:	return x*x*x*x*x-10.0*x*x*x+15.0*x;
					break;
			case 6:	return x*x*x*x*x*x-15.0*x*x*x*x+45.0*x*x-15.0;
					break;
			case 7:	return x*x*x*x*x*x*x-21.0*x*x*x*x*x+105.0*x*x*x-105.0*x;
					break;
			case 8:	return x*x*x*x*x*x*x*x-28.0*x*x*x*x*x*x+210.0*x*x*x*x-420.0*x*x+105.0;
					break;
			default: QL_FAIL("INTERNAL ERROR: Hermite Polynomial of degree " << m << " can not be computed.");
				break;
		}
	}

	double KernelDensity::optimalBandwidth(const int iterations) {

		QL_REQUIRE(iterations>=0 && iterations<=4,"Iterations ("<<iterations<<") must be 0, 1, 2, 4");
		
		// compute empirical variance
		IncrementalStatistics stat;
		for(int i=0;i<n_;i++) {
			stat.add(data_[i]);
		}
		double v = stat.variance();

		// simple method, for gaussian kernel
		// return sqrt(v)*pow(4.0/((double)n_),1.0/3.0);

		// Bruce Hansen
		int j = iterations; // start value J 
		double r = exp(gammaLn(((double)j)+1.5)) / (2.0*M_PI*pow(sqrt(v),2.0*((double)j)+3.0)); // R(J+1)
		while(j>=1) {
			r = estimateRm(j,r);
			j=j-1;
		}
		return pow(kernelPsi()/(r*((double)n_)),1.0/3.0); 

	}

	double KernelDensity::estimateRm(const int m, const double r) {
		double a = pow(
			pow(2.0,((double)m)+0.5)*exp(gammaLn(((double)m)+0.5)) / (M_PI*r*((double)n_)),
			1.0/(2.0*((double)m)+3.0)
			);
		double sum=0.0;
		for(int i=0;i<n_;i++) {
			for(int j=0;j<n_;j++) {
				double y=(data_[j]-data_[i])/a;
				sum+=pow(a,-(1.0+2.0*((double)m)))*hermiteP(2*m,y)*normalDensity(y);
			}
		}
		//sum*= (m%2==0 ? 1.0 : -1.0) / ((double)(n_*n_));
		sum= abs( sum / ((double)n_*(double)n_) );
		return sum;
	}

	double KernelDensity::optimalBandwidth2(int steps, double factor) {

		QL_REQUIRE(steps>0, "Steps " << steps << " must be a positive number.");
		QL_REQUIRE(factor>=1.0,"Factor " << factor << " must be >= 1.0.");

		double ph=plugInBandwidth();

		vector<double> cv = crossValidation(ph/factor,ph*factor,steps);
		
		double min=1E+12;
		double h0=ph/factor,h=h0;
		double dh = (ph*factor-ph/factor)/steps;

		for(int i=0;i<cv.size();i++) {
			if(cv[i]<min) {
				min=cv[i];
				h=h0+i*dh;
				//if(i==cv.size()-1) QL_FAIL("Did not find optimal density bandwidth. Choose higher factor (" << factor <<")");
			}
		}
		
		return h;

	}

	double KernelDensity::plugInBandwidth() {
		return 1.06*pow((double)n_,-0.2)*sqrt(moments()[1]);
	}

	vector<double> KernelDensity::crossValidation(double h1, double h2, int steps) {
		
		double dh = (h2-h1)/steps;
		double h=h1,A,B;
		vector<double> res;

		for(int c=0;c<steps;c++) {
			// simple cross validation
			A=0.0;
			for(int i=0;i<n_;i++) {
				A+=pow(density(data_[i],h,i)-data_[i],2);
			}
			A=A/n_;
			res.push_back(A);
			
			// bruce hansen --- this does not work ---- ?!?!?!?
			/*A=0.0;
			B=0.0;
			for(int i=0;i<n_;i++) {
				for(int j=0;j<n_;j++) {
					A+=kernelC((data_[i]-data_[j])/h);
					if(j!=i) B+=kernel((data_[i]-data_[j])/h);
				}
				//B+=density(data_[i],h,i);
			}
			A=A/(n_*n_*h);
			//B=B/n_;
			B=B/(n_*(n_-1)*h);
			res.push_back(A-2.0*B);*/
			//
			h+=dh;
		}

		return res;

	}

	double KernelDensity::normalDensity(double x) {
		return 1.0/sqrt(2.0*M_PI)*exp(-0.5*x*x);
	}

	vector<double> KernelDensity::pointMassDensity(double unit) {

		if(pointMassDensityCalculated_ && pointMassDensityUnit_==unit) {
			return pointMassDensity_;
		}
		else {
			double min = minData();
			double max = maxData();
			QL_REQUIRE(unit > 0.0, "Unit (" << unit << ") must be positive");
			long minIndex = floor(0.5 + min / unit);
			pointMassDensityMinIndex_=minIndex;
			long maxIndex = floor(0.5 + max / unit);
			QL_REQUIRE(maxIndex < 1000000,"Result Vector too long (" << maxIndex << ")");
			pointMassDensity_ = vector<double>(maxIndex-minIndex+1,0.0);

			double oneNth = 1.0/(double)n_;
			for(int i=0;i<data_.size();i++) {
				long uIndex = floor(0.5 + data_[i] / unit);
				pointMassDensity_[uIndex-minIndex] += oneNth; 
			}

			pointMassDensityUnit_=unit;
			pointMassDensityCalculated_=true;

			return pointMassDensity_;
		}

	}

	long KernelDensity::pointMassMinIndex(double unit) { 
		pointMassDensity(unit); // make sure that density is calculated w.r.t. given unit
		return pointMassDensityMinIndex_; 
	}

	double KernelDensity::pointMassCumulativeInverse(double unit, double p) {

		if(!pointMassDensityCalculated_ || pointMassDensityUnit_!=unit) {
			pointMassDensity(unit);
		}
		
		// look up the two neighbour points
		int i=-1;
		double sum=0.0;
		do {
			i++;
			sum+=pointMassDensity_[i];
		} while(i<pointMassDensity_.size() && sum < p);
		
		double p1= i>0 ? sum-pointMassDensity_[i] : 0.0;
		double p2= sum >= p ? sum : 1.0;
		double l1= i>0 ? (i-1+pointMassDensityMinIndex_)*unit : 0.0;
		double l2=(i+pointMassDensityMinIndex_)*unit;

		return l1+(p-p1)/(p2-p1)*(l2-l1) + unit / 2.0;

	}

			
}


	
