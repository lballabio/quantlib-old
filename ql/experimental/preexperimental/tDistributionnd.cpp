#include <tDistributionnd.hpp>

using namespace std;

namespace QuantLib {

	TDistributionnd::TDistributionnd(const Matrix& corr) : corr_(corr) {
		setup();
	}

	TDistributionnd::TDistributionnd(const vector<vector<double>>& corr) {
		corr_=Matrix(corr.size(),corr.size());
		for(int i=0;i<corr.size();i++) {
			for(int j=0;j<corr.size();j++) {
				corr_[i][j]=corr[i][j];
			}
		}
		setup();
	}
	
	void TDistributionnd::setup() {
		SInv_=inverse(corr_);
		det_=determinant(corr_);
		p_=corr_.rows();
		Chol_=CholeskyDecomposition(corr_);
	}

	Matrix& TDistributionnd::cholesky() {
		return Chol_;
	}

	double TDistributionnd::density(const double nu, const vector<double> x) {
		QL_REQUIRE(x.size()==p_,"Argument dimension (" << x.size() << ") does not match distribution (" << p_ << ")");
		double sum=0.0;
		for(int i=0;i<p_;i++) {
			for(int j=0;j<p_;j++) {
				sum+=x[i]*x[j]*SInv_[i][j];
			}
		}
		if(nu==0.0) {
			return exp(-0.5*sum)/(sqrt(det_)*pow(2.0*M_PI,((double)p_)/2.0));
		}
		else {
			return exp(gammaLn((nu+(double)p_)/2.0)-gammaLn(nu/2.0))/
				(pow(nu*M_PI,(double)p_/2.0)*sqrt(det_)*pow(1.0+sum/nu,(nu+(double)p_)/2.0));
		}
	}

	vector<double> TDistributionnd::copulaSample(const double nu, const vector<double>& u, const double v, const bool normalize) {
		InverseCumulativeNormal icn;
		vector<double> nv;
		QL_REQUIRE(u.size()==p_,"u dimension " << u.size() << " does not match distribtion dim " << p_);
		for(int i=0;i<p_;i++) {
			nv.push_back(icn(u[i]));
		}
		vector<double> x(p_,0.0);
		for(int i=0;i<p_;i++) {
			for(int j=0;j<p_;j++) {
				x[i]+=Chol_[i][j]*nv[j];
			}
		}
		double w=1.0;
		if(nu!=0.0) {
			InverseGamma ig(nu/2.0,nu/2.0);
			w = sqrt(ig.cumulativeInv(v));
		}
		if(normalize) {
			TDistribution1d t1d;
			for(int i=0;i<p_;i++) {
				x[i] = t1d.cumulative(nu,w*x[i]);
			}
		}
		else {
			if(nu!=0.0) {
				for(int i=0;i<p_;i++) {
					x[i]*=w;
				}
			}
		}
		return x;
	}

	vector<vector<double>> TDistributionnd::copulaSample(const double nu, const unsigned long n, const unsigned long seed) {
		
		MersenneTwisterUniformRng mt(seed);
		
		vector<double> u(p_);
		double v;
		vector<vector<double>> result;
		
		for(int i=0;i<n;i++) {
			for(int j=0;j<p_;j++) {
				u[j]=mt.next().value;
			}
			v=mt.next().value;
			result.push_back(copulaSample(nu,u,v));
		}
		return result;
	}


	
		
}


	
