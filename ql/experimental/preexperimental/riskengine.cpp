#include <riskengine.hpp>

using namespace std;

namespace QuantLib {

	RiskEngine::RiskEngine(vector<string> names, vector<long> dates, vector<vector<double>> values) {
		
		QL_REQUIRE(values.size()==dates.size(),"wrong number of values " << values.size() << " != " << dates.size());
		
		for(int i=0;i<names.size();i++) {
			name_.push_back(names[i]);
			value_.push_back(vector<double>(0));
			date_.push_back(vector<long>(0));
			for(int j=0;j<dates.size();j++) {
				QL_REQUIRE(values[j].size()==names.size(),"wrong number of names " << values[j].size() << " != " << names.size());
				value_[i].push_back(values[j][i]);
				date_[i].push_back(dates[j]);
			}
		}
	}

	RiskEngine::RiskEngine(string path) {

			ifstream in;
			in.open(path.c_str());

			string line,name,name2,date,time;
			double value;
			long i=-1;
			size_t pos1,pos2;
			Date d;
			int day,month,year;
			long sn;

			while(!in.eof()) {
				in >> name; in >>  date; in >> time; in >> value;
				
				// convert date to serial number
				pos1=date.find(".",0);
				pos2=date.find(".",pos1+1);
				if(pos1==string::npos || pos2==string::npos)
					QL_FAIL("Invalid date format line = " << i+2 << " string = " << date);
				
				day=atoi(date.substr(0,pos1).c_str());
				month=atoi(date.substr(pos1+1,pos2).c_str());
				year=atoi(date.substr(pos2+1).c_str());
				if(year==0)
					QL_FAIL("Invalid date line = " << i+2 << " string = " << date);
				d = Date(day,(Month)month,year);
				sn=d.serialNumber();
				
				if(name.compare(name2)==0) {
					value_[i].push_back(value);
					date_[i].push_back(sn);
				}
				else {
					name_.push_back(name);
					i++;
					value_.push_back(vector<double>());
					value_[i].push_back(value);
					date_.push_back(vector<long>());
					date_[i].push_back(sn);
				}
				name2=name;
			}

			in.close();
		
	}

	vector<double> RiskEngine::series(string name) {

		return value_[lookup(name)];

	}

	vector<long> RiskEngine::dates(string name) {
		
		return date_[lookup(name)];

	}


	vector<string> RiskEngine::names() {
		
		return name_;

	}

	long RiskEngine::lookup(string name) {
		long i;
		for(i=0;i<name_.size();i++) {
			if(name.compare(name_[i])==0) {
				return i;	
			}
		}
		QL_FAIL("Can not find series " << name);
	}

	boost::shared_ptr<KernelDensity> RiskEngine::kernelDensity(string name, int method) {
		return boost::shared_ptr<KernelDensity>(new KernelDensity(transformData(this->series(name), method)));
	}

	vector<double> RiskEngine::transformData(const vector<double>& data, int mode) {

			vector<double> data2(0);
			double x;
			for(int i=0;i<data.size();i++) {
				switch(mode) {
					case 0: data2.push_back(data[i]);
							break;
					case 1: if(i>=1) {
								data2.push_back(data[i]-data[i-1]);
							}
							break;
					case 2: if(i>=1) {
								if(data[i-1]<SMALL)
									data2.push_back(0.0);
								else
									data2.push_back((data[i]-data[i-1])/data[i-1]);
							}
							break;
					case 3: if(i>=1) {
								if(fabs(data[i-1])<SMALL)
									data2.push_back(0.0);
								else
									data2.push_back(log(data[i]/data[i-1]));
							}
							break;
					default: QL_FAIL("Method " << mode << " not defined.");
							break;
				}
			}
			return data2;

	}

	long RiskEngine::numberOfData(vector<string> names) {
		return consolidateData(names,0)[0].size();
	}

	vector<vector<double>> RiskEngine::consolidateData(vector<string> names, int mode) {
		
		vector<vector<long>> v(0);
		vector<vector<double>> w(0);

		vector<vector<double>> result1(0),result2(0);

		// consolidate

		int n = names.size();
		
		for(int i=0;i<n;i++) {
			v.push_back(dates(names[i]));
			w.push_back(series(names[i]));
			result1.push_back(vector<double>(0));
		}

		vector<long> indices(n,0);
		bool done=false;

		while(!done) {
		
			long maxI=0;
			for(int x=0;x<n;x++) {
				if(v[x][indices[x]]>maxI)
					maxI=v[x][indices[x]];
			}
			for(int x=0;x<n;x++) {
				while(v[x][indices[x]]<maxI && indices[x]<v[x].size()-1) 
					indices[x]++;
			}
			bool equal=true;
			
			done=true;
			for(int x=0;x<n;x++) {
				if(indices[x]<v[x].size()-1)
					done=false;
			}
			for(int x=1;x<n;x++) {
				if(v[x][indices[x]]!=v[0][indices[0]]) {
					equal=false;
					break; 
				}
			}
			if(equal) {
				for(int x=0;x<n;x++) {
					result1[x].push_back(w[x][indices[x]]);
					indices[x]++;
				}
			}

		}

		// transform
		for(int x=0;x<n;x++) {
			result2.push_back(transformData(result1[x],mode));
		}
		
		return result2;

	}

	Matrix RiskEngine::correlation(vector<string> names,  int mode, bool ensurePositivity, bool naive, int start, double lambda) {
		
		vector<vector<double>> data = consolidateData(names, mode);
		return correlation(data,ensurePositivity,naive,start,lambda);

	}

	Matrix RiskEngine::correlation(const vector<vector<double>>& data, bool ensurePositivity, bool naive, int start, double lambda) {
		
		int n = data.size();
		Matrix corr(n,n);

		// pointwise estimation

		for(int i=0;i<n;i++) {
			for(int j=0;j<i;j++) {
				corr[i][j]=corr[j][i] = naive ? naiveCorr(data[i],data[j],start,lambda) : sin( M_PI/2.0* kendallsTau(data[i],data[j],start,lambda) );
			}
			corr[i][i]=1.0;
		}

		// correct negative eigenvalues

		if(ensurePositivity) {
			SymmetricSchurDecomposition dec(corr);
			Array ev = dec.eigenvalues();
			Matrix tr = dec.eigenvectors();
			Array dg(n);

			for(int i=0;i<n;i++) {
				if(ev[i] < SMALLESTEIGENVALUE)
					ev[i]=SMALLESTEIGENVALUE;
			}
			
			for(int i=0;i<n;i++) {
				for(int j=0;j<n;j++) {
					corr[i][j]=0.0;
					for(int k=0;k<n;k++) {
						corr[i][j] += tr[i][k]*ev[k]*tr[j][k];
					}
				}
			}

			for(int i=0;i<n;i++) {
				dg[i]=corr[i][i];
			}

			for(int i=0;i<n;i++) {
				for(int j=0;j<n;j++) {
					corr[i][j]/=sqrt(dg[i]*dg[j]);
				}
			}
		}

		return corr;
	}

	vector<double> RiskEngine::eigenvalues(vector<string> names,  int mode, bool naive, int start, double lambda) {
		vector<vector<double>> data = consolidateData(names, mode);
		int n = data.size();
		Matrix corr(n,n);
		// pointwise estimation
		for(int i=0;i<n;i++) {
			for(int j=0;j<i;j++) {
				corr[i][j]=corr[j][i]=naive ? naiveCorr(data[i],data[j],start,lambda) : sin( M_PI/2.0* kendallsTau(data[i],data[j],start,lambda) );
			}
			corr[i][i]=1.0;
		}
		SymmetricSchurDecomposition dec(corr);
		return array2Vector(dec.eigenvalues());
	}

	double RiskEngine::naiveCorr(const vector<double>& d1, const vector<double>& d2,int start, double lambda) {
		
		int n=d1.size();
		
		QL_REQUIRE(d2.size()==n,"vectors are of different dimensions (" << d1.size() << "," << d2.size() << ")");
		QL_REQUIRE(start<=n,"number of start values " << start << " have to be le number of data points (" << n << ")");
		
		double s1,m1,s2,m2,res;
		IncrementalStatistics stat1,stat2;

		if(start==0) {
			// no weighting
			for(int i=0;i<n;i++) {
				stat1.add(d1[i]);
				stat2.add(d2[i]);
			}
			m1=stat1.mean();
			s1=stat1.standardDeviation();
			m2=stat2.mean();
			s2=stat2.standardDeviation();
			
			res=0.0;
			for(int i=0;i<n;i++) {
				res+=(d1[i]-m1)*(d2[i]-m2);
			}
			res/=s1*s2*((double)(n-1));
		}
		else {
			// weighting
			for(int i=0;i<start;i++) {
				stat1.add(d1[i]);
				stat2.add(d2[i]);
			}
			m1=stat1.mean();
			s1=stat1.standardDeviation();
			m2=stat2.mean();
			s2=stat2.standardDeviation();
			res=0.0;
			for(int i=0;i<start;i++) {
				res+=(d1[i]-m1)*(d2[i]-m2);
			}
			res/=start-1;
			// recursion
			for(int i=start;i<n;i++) {
				m1=(i-1)/i*m1+d1[i]/i; // no weighting of means
				m2=(i-1)/i*m2+d2[i]/i;
				s1=sqrt(lambda*s1*s1+(1.0-lambda)*(d1[i]-m1)*(d1[i]-m1));
				s2=sqrt(lambda*s2*s2+(1.0-lambda)*(d2[i]-m2)*(d2[i]-m2));
				res=lambda*res+(1.0-lambda)*(d1[i]-m1)*(d2[i]-m2);
			}
			res/=s1*s2;
		}
		return res;
	}

	double RiskEngine::kendallsTau(const vector<double>& d1, const vector<double>& d2,int start, double lambda) {
		
		int n=d1.size();
		QL_REQUIRE(d2.size()==n,"vectors are of different dimensions (" << d1.size() << "," << d2.size() << ")");
		
		double sum=0.0;
		double t,res;

		if(start==0) {
			// unweighted
			for(int i=0;i<n;i++) {
				for(int j=0;j<i;j++) {
					t=(d1[j]-d1[i])*(d2[j]-d2[i]);
					if(t!=0.0) {
						sum+= t > 0.0 ? 1.0 : -1.0;
					}
				}
			}
			res=2.0* sum / (((double)n)*((double)(n-1)));
		}
		else {
			// weighted
			double li,lj;
			double denom=0.0;
			for(int i=0;i<n;i++) {
				for(int j=0;j<i;j++) {
					li = i<=start ? 1.0 / sqrt((double)start) * pow(lambda,(n-start)/2.0) : pow(lambda,n-i)*sqrt(1.0-lambda);
					lj = j<=start ? 1.0 / sqrt((double)start) * pow(lambda,(n-start)/2.0) : pow(lambda,n-j)*sqrt(1.0-lambda);
					t=(d1[j]-d1[i])*(d2[j]-d2[i]);
					if(t!=0.0) {
						sum+= li*lj* (t > 0.0 ? 1.0 : -1.0);
					}
					denom+=li*lj;
				}
			}
			res=sum / denom;
		}

		return res;

	}

	vector<double> RiskEngine::tLogLikelihood(vector<string> names, int mode, vector<double> nu) {
		
		vector<vector<double>> data = consolidateData(names, mode);

		int n = names.size();
		int m = data[0].size();

		Matrix corr = correlation(data,true);
		
		// create kernel estimators for marginal distributions
		vector<KernelDensity> dens;
		for(int i=0;i<n;i++) {
			dens.push_back(KernelDensity(data[i]));
		}
		
		// normalize data, i.e. apply cumulative distribution (from kernel density estimation) to marginal data
		vector<vector<double>> dataNorm;
		for(int i=0;i<n;i++) {
			dataNorm.push_back(vector<double>(0));
			for(int j=0;j<m;j++) {
				dataNorm[i].push_back(dens[i].cumulative(data[i][j]));
			}
		}

		
		vector<double> x(n,0.0);

		vector<double> res;

		TDistribution1d t1d;
		TDistributionnd tnd(corr);

		// compute likelihood
		for(int k=0;k<nu.size();k++) {

			double sum=0.0;

			for(int i=0;i<m;i++) {
				for(int j=0;j<n;j++) {
					x[j] = t1d.cumulativeInv(nu[k],dataNorm[j][i]);
					sum-=log(t1d.density(nu[k],x[j]));
				}
				sum+=log(tnd.density(nu[k],x));
			}

			res.push_back(sum);

		}
		return res;

	}

	boost::shared_ptr<KernelDensity> RiskEngine::lossDistribution(vector<string> names, const vector<double> deltas, const vector<double> gammas, const int method, const vector<double> fx, const double nu, const long paths, const long seed,
		int start, double lambda, bool naive, bool normal, int resolution) {
		
		int p = names.size();

		QL_REQUIRE(deltas.size()==p,"Deltas Size " << deltas.size() << " does not match names size " << p);
		QL_REQUIRE(gammas.size()==p,"Gammas Size " << gammas.size() << " does not match names size " << p);
		QL_REQUIRE(fx.size()==p,"FX Size " << gammas.size() << " does not match names size " << p);

		vector<vector<double>> data = consolidateData(names, method);
		int m = data[0].size();
		Matrix corr = correlation(data,true,naive,start,lambda);
		TDistributionnd tnd(corr);

		vector<KernelDensity> dens;
		for(int i=0;i<p;i++) {
			dens.push_back(KernelDensity(data[i]));
		}

		MersenneTwisterUniformRng mt(seed);
		
		vector<double> u(p);
		double v,rf,deltap;
		vector<double> cs(p);

		vector<double> resData;
		
		for(int i=0;i<paths;i++) {
			// generate copula sample
			for(int j=0;j<p;j++) {
				u[j]=mt.next().value;
			}
			v=mt.next().value;
			cs=tnd.copulaSample(nu,u,v);
			// transform to risk factors marginals
			deltap=0.0;
			for(int j=0;j<p;j++) {
				if(normal) rf = dens[j].normalCumulativeInv(cs[j],start,lambda);
				else rf=dens[j].cumulativeInverse(cs[j],0.0,true,resolution);
				deltap+=fx[j]*(deltas[j]*rf+0.5*gammas[j]*rf*rf);
			}
			resData.push_back(deltap);
		}

		boost::shared_ptr<KernelDensity> result(new KernelDensity(resData));
		return result;
	
	
	}

	boost::shared_ptr<KernelDensity> RiskEngine::lossDistribution(vector<string> names, const vector<double> deltas, const vector<double> gammas, const int method, const vector<double> fx,int length) {
		
		int p = names.size();

		QL_REQUIRE(deltas.size()==p,"Deltas Size " << deltas.size() << " does not match names size " << p);
		QL_REQUIRE(gammas.size()==p,"Gammas Size " << gammas.size() << " does not match names size " << p);
		QL_REQUIRE(fx.size()==p,"FX Size " << gammas.size() << " does not match names size " << p);

		vector<vector<double>> data = consolidateData(names, method);
		int m = data[0].size();

		QL_REQUIRE(length<=m,"length " << length << " must be leq " << m);

		if(length==0) length=m;

		vector<double> resData;
		double rf,deltap;

		for(int i=0;i<length;i++) {
			deltap=0.0;
			for(int j=0;j<p;j++) {
				rf=data[j][m-1-i];
				deltap+=fx[j]*(deltas[j]*rf+0.5*gammas[j]*rf*rf);
			}
			resData.push_back(deltap);
		}

		boost::shared_ptr<KernelDensity> result(new KernelDensity(resData));
		return result;


	}
	
			
}


	
