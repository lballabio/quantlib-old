#include <simulatedAnnealing.hpp>

using namespace std;

namespace QuantLib {
	
	double SimulatedAnnealing::amotsa(Problem& P, double fac) {
		double fac1,fac2,yflu,ytry;
		Array ptry(n,0.0);
		int j;
		fac1=(1.0-fac)/(double)n;
		fac2=fac1-fac;
		//cout << "simulated annealing: trying ";
		for(j=0;j<n;j++) {
			ptry[j]=sum_[j]*fac1-vertices_[ihi][j]*fac2;
			//cout << ptry[j] << " ";
		}
		if(!P.constraint().test(ptry))
			ytry = MAX_DOUBLE;
		else
			ytry=P.value(ptry);
		if(!(ytry==ytry)) { // NAN
			ytry = MAX_DOUBLE;
		}
		//cout << "objective function is " << ytry;
		if(ytry<yb) {
			yb = ytry;
			pb = ptry;
			//fprintf(logFile," [new best ever point!] ");
			//for(j=0;j<n;j++) {
			//	fprintf(logFile,"%f ",ptry[j]); 
			//}
			//fprintf(logFile," objective fct is %f\n",ytry);
		}
		yflu=ytry-tt*log((double)(mt.next().value));
		if(yflu<yhi) {
			values_[ihi]=ytry;
			yhi=yflu;
			for(j=0;j<n;j++) {
				sum_[j]+=ptry[j]-vertices_[ihi][j];
				vertices_[ihi][j]=ptry[j];
			}
		}
		return yflu;
	}
		
    EndCriteria::Type SimulatedAnnealing::minimize(Problem& P,
                                        const EndCriteria& endCriteria) {
        EndCriteria::Type ecType = EndCriteria::None;
        P.reset();
        Array x_ = P.currentValue();
        Integer iterationNumber_=0;

        bool end = false;
        n = x_.size();
		
		// build vertices

        vertices_ = std::vector<Array>(n+1, x_);
        for (i=0; i<n; i++) {
            Array direction(n, 0.0);
            direction[i] = 1.0;
            P.constraint().update(vertices_[i+1], direction, lambda_);
        }
        values_ = Array(n+1, 0.0);
		for (i=0; i<=n; i++) {
			//fprintf(logFile,"simulated annealing: start vertex %d is ",i);
			//for(j=0; j<n; j++) {
			//	fprintf(logFile,"%f ",vertices_[i][j]);
			//}
			if(!P.constraint().test(vertices_[i]))
				values_[i] = MAX_DOUBLE;
			else
				values_[i]=P.value(vertices_[i]);
			if(!(ytry==ytry)) { // NAN
				values_[i] = MAX_DOUBLE;
			}
			//fprintf(logFile," with objective function value %f\n",values_[i]);
		}
		
		// start minimizing

		currentTemp = startTemp;
		//srand(1);
		mt=MersenneTwisterUniformRng(seed);
		yb=100000.0;
		pb=Array(n,100000.0);
        do {
			int iterationsTempStart = iterationNumber_;
			do {
				sum_ = Array(n, 0.0);
				for(i=0;i<n;i++) sum_[i]=0.0;
				for (i=0; i<=n; i++)
					sum_ += vertices_[i];
				
				tt=-currentTemp;
				ilo=0;
				ihi=1;
				ynhi=values_[0]+tt*log((double)(mt.next().value));
				ylo=ynhi;
				yhi=values_[1]+tt*log((double)mt.next().value);
				if(ylo>yhi) {
					ihi=0;
					ilo=1;
					ynhi=yhi;
					yhi=ylo;
					ylo=ynhi;
				}
				for(i=2;i<n+1;i++) {
					yt=values_[i]+tt*log((double)mt.next().value);
					if(yt<=ylo) {
						ilo=i;
						ylo=yt;
					}
					if(yt>yhi) {
						ynhi=yhi;
						ihi=i;
						yhi=yt;
					}
					else {
						if(yt>ynhi) {
							ynhi=yt;
						}
					}
				}
				rtol=2.0*fabs(yhi-ylo)/(fabs(yhi)+fabs(ylo));
				//cout << "Iteration number " << iterationNumber_ << " , rtol=" << rtol << endl;
				if (rtol < endCriteria.functionEpsilon() ||
					endCriteria.checkMaxIterations(iterationNumber_, ecType)) {
						endCriteria.checkStationaryFunctionAccuracy(QL_EPSILON, true, ecType);
						endCriteria.checkMaxIterations(iterationNumber_, ecType); // WARNING: A CHE COSA SERVE ???
						//x_ = vertices_[ilo];
						//P.setFunctionValue(swap);
						//P.setCurrentValue(x_);
						P.setCurrentValue(pb);
						P.setFunctionValue(yb);
						//fclose(logFile);
						return ecType;
				}
				iterationNumber_+=2;
				ytry = amotsa(P,-1.0);
				if(ytry<=ylo) {
					ytry = amotsa(P,2.0);
				}
				else {
					if(ytry>=ynhi) {
						ysave=yhi;
						ytry=amotsa(P,0.5);
						if(ytry>=ysave) {
							for(i=0;i<n+1;i++) {
								if(i!=ilo) {
									for(j=0;j<n;j++) {
										sum_[j]=0.5*(vertices_[i][j]+vertices_[ilo][j]);
										vertices_[i][j]=sum_[j];
									}
									values_[i]=P.value(sum_);
								}
							}
							iterationNumber_+=n;
							for(i=0;i<n;i++) sum_[i]=0.0;
							for (i=0; i<=n; i++)
								sum_ += vertices_[i];
						}
					}
					else {
						iterationNumber_+=1;
					}
				}
			} while(iterationNumber_<iterationsTempStart+iterBeforeTempDec);
		currentTemp = (1-tempDecayFactor) * currentTemp;
		if(iterationNumber_>iterBeforeTempZero) {
			currentTemp = 0.0;
		}
		//fprintf(logFile,"simulated annealing: iteration number %d rtol %f decreasing temperature to %f\n",iterationNumber_,rtol,currentTemp);
		} while (end == false);
        QL_FAIL("optimization failed: unexpected behaviour");
    }
}
