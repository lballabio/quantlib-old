/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2012 Minoru Mizutani

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include "customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

using namespace QuantLib;

class RosenBrockFunction: public CostFunction{
public:
    Real value(const Array& x) const{
        QL_REQUIRE(x.size()==2,"Rosenbrock function is 2-dim.");
        Real res=(1-x[0])*(1-x[0]);
        res+=100.0*(x[1]-x[0]*x[0])*(x[1]-x[0]*x[0]);
        return res;
    }

    Disposable<Array> values(const Array& x) const{
        QL_REQUIRE(x.size()==2,"Rosenbrock function is 2-dim.");
        // irrelevant what you write in res for most of the optimizers
        // most of them are using value anyways. try with res[0]=100.0
        Array res(1); res[0]=value(x);
        return res;
    }
};

void testOptimizer1(){

    Size maxIterations=1000;
    Size minStatIterations=100;
    Real rootEpsilon=1e-8;
    Real functionEpsilon=1e-9;
    Real gradientNormEpsilon=1e-5;

    EndCriteria myEndCrit(	maxIterations,
        minStatIterations,
        rootEpsilon,
        functionEpsilon,
        gradientNormEpsilon);

    RosenBrockFunction myFunc;
    NoConstraint constraint;

    Problem myProb1(myFunc, constraint, Array(2,0.1));
    Problem myProb2(myFunc, constraint, Array(2,0.1));

    Simplex solver1(0.1);
    ConjugateGradient solver2;

    EndCriteria::Type solvedCrit1=solver1.minimize(myProb1,myEndCrit);
    EndCriteria::Type solvedCrit2=solver2.minimize(myProb2,myEndCrit);

    std::cout << "Criteria Simplex:"<< solvedCrit1 << std::endl;
    std::cout << "Root Simplex:"<< myProb1.currentValue() << std::endl;
    std::cout << "Min F Value Simplex:"<< myProb1.functionValue() << std::endl;
    std::cout << "Criteria CG:"<< solvedCrit2 << std::endl;
    std::cout << "Root CG:"<< myProb2.currentValue() << std::endl;
    std::cout << "Min F Value CG:"<< myProb2.functionValue() << std::endl;

}


#include <boost/math/distributions.hpp>

Real   blackScholesPrice(const Real& spot,
    const Real& strike,
    const Rate& rd,
    const Rate& rf,
    const Volatility& vol,
    const Time& tau,
    const Integer& phi){

        boost::math::normal_distribution<> d(0.0,1.0);
        Real dp,dm, fwd, stdDev, res, domDf, forDf;

        domDf=std::exp(-rd*tau); forDf=std::exp(-rf*tau);
        fwd=spot*forDf/domDf;
        stdDev=vol*std::sqrt(tau);

        dp=(std::log(fwd/strike)+0.5*stdDev*stdDev)/stdDev;
        dm=(std::log(fwd/strike)-0.5*stdDev*stdDev)/stdDev;

        res=phi*domDf*(fwd*cdf(d,phi*dp)-strike*cdf(d,phi*dm));
        return res;
}

class CallProblemFunction: public CostFunction{
private:

    Real C1_,C2_,C3_,C4_,K1_,K2_,K3_,K4_;
    Rate rd_,rf_;
    Integer phi_;
    Time tau_;
public:
    CallProblemFunction(const Rate& rd, const Rate& rf, const Time& tau, const Integer& phi,
        const Real& K1,const Real& K2,const Real& K3,const Real& K4,
        const Real& C1,const Real& C2,const Real& C3,const Real& C4)
        :rd_(rd), rf_(rf), phi_(phi), tau_(tau),
        C1_(C1),C2_(C2),C3_(C3),C4_(C4),
        K1_(K1),K2_(K2),K3_(K3),K4_(K4){}

    Real value(const Array& x) const{

        Array tmpRes=values(x);
        Real res=tmpRes[0]*tmpRes[0];
        res+=tmpRes[1]*tmpRes[1];
        res+=tmpRes[2]*tmpRes[2];
        res+=tmpRes[3]*tmpRes[3];
        return res;
    }

    Disposable<Array> values(const Array& x) const{

        Array res(4);
        res[0]=blackScholesPrice(x[0],K1_,rd_,rf_,x[1],tau_,phi_)-C1_;
        res[1]=blackScholesPrice(x[0],K2_,rd_,rf_,x[1],tau_,phi_)-C2_;
        res[2]=blackScholesPrice(x[0],K3_,rd_,rf_,x[1],tau_,phi_)-C3_;
        res[3]=blackScholesPrice(x[0],K4_,rd_,rf_,x[1],tau_,phi_)-C4_;

        return res;
    }
};

void testOptimizer2(){
    // setup of market parameters
    Real spot=98.51;
    Volatility vol=0.134;
    Real K1=87.0, K2=96.0, K3=103.0, K4=110.0;
    Rate rd=0.002, rf=0.01;
    Integer phi=1;
    Time tau=0.6;
    // calculate Black Scholes prices
    Real C1=blackScholesPrice(spot,K1,rd,rf,vol,tau,phi);
    Real C2=blackScholesPrice(spot,K2,rd,rf,vol,tau,phi);
    Real C3=blackScholesPrice(spot,K3,rd,rf,vol,tau,phi);
    Real C4=blackScholesPrice(spot,K4,rd,rf,vol,tau,phi);

    CallProblemFunction optFunc(rd, rf, tau, phi,K1, K2, K3, K4, C1, C2, C3, C4);

    Size maxIterations=1000;
    Size minStatIterations=100;
    Real rootEpsilon=1e-5;
    Real functionEpsilon=1e-5;
    Real gradientNormEpsilon=1e-5;

    EndCriteria myEndCrit(maxIterations,minStatIterations, rootEpsilon,
        functionEpsilon, gradientNormEpsilon);

    Array startVal(2); startVal[0]=80.0; startVal[1]=0.20;
    NoConstraint constraint;
    Problem myProb(optFunc, constraint, startVal);
    LevenbergMarquardt solver;
    EndCriteria::Type solvedCrit=solver.minimize(myProb,myEndCrit);

    std::cout << "Criteria :"<< solvedCrit << std::endl;
    std::cout << "Root :" << myProb.currentValue() << std::endl;
    std::cout << "Min Function Value :"	<< myProb.functionValue() << std::endl;

}


class RPCostFunction : public CostFunction {
public:
    RPCostFunction(const Matrix& cov)
            : cov_(cov){}

    Real value(const Array& x) const{ // x: calibration parameters
        Real wVarCov;
        for (Size i=0; i<x.size(); ++i) {
            for (Size j=0; j<x.size(); ++j) {
                wVarCov += x[i] * cov_[i][j] * x[j];
            }
        }
        return wVarCov;
    }

    Disposable<Array> values(const Array& x) const{
        QL_REQUIRE(x.size()>1,"cost function is multi dimensional.");
        Array res(1); res[0]=value(x);
        return res;
    }

private:
    Matrix cov_;
    //std::vector<Real> y_;
    //std::vector<Real> b_;
    //std::vector<std::vector<Real> > cov_;

};

class RPCostFunction2 : public CostFunction {
public:
    RPCostFunction2(const Matrix& cov, const Array& beta)
        : cov_(cov), beta_(beta) {}

    Real value(const Array& x) const{ // x: calibration parameters
        Array tmpRes = values(x);
        Real res = 0;
        for (Size i=0; i<x.size(); ++i) {
            res += tmpRes[i] * tmpRes[i];
        }
        return res;
    }

    Disposable<Array> values(const Array& x) const{
        QL_REQUIRE(x.size()>1,"cost function must be multi dimensional.");
        QL_REQUIRE(x.size()==beta_.size(),"x and beta must have the same number of dimensions.");
        Array res(x.size());
        Real portVar = 0.0;
        Array iPortVar(x.size(), 0.0);
        for (Size i=0; i<x.size(); ++i) {
            iPortVar[i] = x[i] * (cov_ * x)[i];
            portVar += iPortVar[i];
        }
        for (Size i=0; i<x.size(); ++i) {
            res[i] = (iPortVar[i] / portVar) - beta_[i];
        }
        return res;
    }

private:
    Matrix cov_;
    Array beta_;
};

class RPConstSumLEQOne : public Constraint {
private:
    class Impl : public Constraint::Impl {
    public:
        Impl(Real sumcap)
            : sumcap_(sumcap) {}
        bool test(const Array& params) const {
            Real sum = 0.0;
            for (Size i=0; i<params.size(); ++i) {
                sum += params[i];
            }
            if (sum > sumcap_){
                return false;
            } else{
                return true;
            }
        }
    private:
        Real sumcap_;
    };
public:
    RPConstSumLEQOne(Real sumcap)
        : Constraint(boost::shared_ptr<Constraint::Impl>(
        new RPConstSumLEQOne::Impl(sumcap))) {}
};

class RPConstSumGEQOne : public Constraint {
private:
    class Impl : public Constraint::Impl {
    public:
        Impl(Real sumfloor)
            : sumfloor_(sumfloor) {}
        bool test(const Array& params) const {
            Real sum = 0.0;
            for (Size i=0; i<params.size(); ++i) {
                sum += params[i];
            }
            if (sum < sumfloor_){
                return false;
            } else{
                return true;
            }
        }
    private:
        Real sumfloor_;
    };
public:
    RPConstSumGEQOne(Real sumfloor)
        : Constraint(boost::shared_ptr<Constraint::Impl>(
        new RPConstSumGEQOne::Impl(sumfloor))) {}
};

int main(int, char* []) {

    try {

        boost::timer timer;


        LARGE_TITLE("Risk Parity Portfolio Optimization");

        // Reference: Managing Risk Exposures using the Risk Budgeting Approach

        /**
         * (1) Harris Global Equity Fund
         * (2) Japan Equity Index Fund
         * (3) Gold Futures Fund
         * (4) Japan Gov Bond Index Fund
         */

        std::cout << std::setiosflags(std::ios::fixed);

        Size n = 4;

        /*
        Real ravg[] = {0.00417326505, 0.00321332276666667, 0.000602390176666667, -1.93802166666667e-005};
        Real stdev[] = {0.0122162006205562, 0.00790953545784156, 0.012253522208572, 0.000750972199325999};
        std::vector<Real> w(4);
        std::vector<Real> r(4);
        std::vector<Real> s(4);


        std::cout << "\nravg" << std::endl;
        for (Size i=0; i<LENGTH(ravg); ++i) {
            std::cout << std::setw(15) << std::setprecision(6) << io::rate(ravg[i]);
            r.push_back(ravg[i]);
        }
        std::cout << "\n\nstdev" << std::endl;
        for (Size i=0; i<LENGTH(stdev); ++i) {
            std::cout << std::setw(15) << std::setprecision(6) << io::rate(stdev[i]);
            s.push_back(stdev[i]);
        }
        */


        std::cout << "\nreturns" << std::endl;
        Matrix returns(n, 1);
        returns[0][0]=0.00417326505;	returns[1][0]=0.00321332276666667;	returns[2][0]=0.000602390176666667;	returns[3][0]=-1.93802166666667e-005;
        returns *= 12.0; // annualize
        returns *= 100.0;
        std::cout << returns << std::endl;

        std::cout << "stdevs" << std::endl;
        Matrix stdevs(n,1);
        stdevs[0][0]=0.0122162006205562; stdevs[1][0]=0.00790953545784156; stdevs[2][0]=0.012253522208572; stdevs[3][0]=0.000750972199325999;
        stdevs *= 12.0;
        stdevs *= 100.0;
        std::cout << stdevs << std::endl;

        std::cout << "corr" << std::endl;
        Matrix corr(n, n);
        corr[0][0]=1.0;                corr[0][1]=0.554599264991065;  corr[0][2]=0.249373675380011; corr[0][3]=-0.216575037138942;
        corr[1][0]=0.554599264991065;  corr[1][1]=1.0;                corr[1][2]=0.157861700788098; corr[1][3]=-0.356110477665697;
        corr[2][0]=0.249373675380011;  corr[2][1]=0.157861700788098;  corr[2][2]=1.0;               corr[2][3]=0.18705711370009;
        corr[3][0]=-0.216575037138942; corr[3][1]=-0.356110477665697; corr[3][2]=0.18705711370009;  corr[3][3]=1.0;
        std::cout << corr << std::endl;

        std::cout << "cov" << std::endl;
        Matrix cov(n,n);
        for (Size i=0; i<n; ++i) {
            for (Size j=0; j<n; ++j) {
                cov[i][j] = stdevs[i][0] * corr[i][j] * stdevs[j][0];
            }
        }
        std::cout << cov << std::endl;

        std::cout << "weights" << std::endl;
        Matrix weights(n, 1, 1.0/n);
        std::cout << weights << std::endl;

        std::cout << "portvar" << std::endl;
        Matrix portvar_ = transpose(weights) * corr * weights;
        Real portvar = portvar_[0][0];
        std::cout << portvar << std::endl;


        /**
         * Minimization parameter finding algorithms available in QuantLib
         * LevenbergMarquardt
         * Simplex
         * ConjugateGradient (line search based method)
         * SteepestDescent (line search based method)
         * BFGS (line search based method)
         */

        std::cout << "\n\n\nNow solving the risk parity optimization problem\n" << std::endl;

        Size maxIterations=100000;
        Size minStatIterations=1000;
        Real rootEpsilon=1e-7;
        Real functionEpsilon=1e-7;
        Real gradientNormEpsilon=1e-7;
        EndCriteria myEndCrit(maxIterations,
                              minStatIterations,
                              rootEpsilon,
                              functionEpsilon,
                              gradientNormEpsilon);

        Array beta(n, 1.0/n); // equal risk contributions
        Array guessWeights(n, 1.0/n);
//            guessWeights[0]=0.03;
//            guessWeights[1]=0.05;
//            guessWeights[2]=0.03;
//            guessWeights[3]=0.90;
            guessWeights[0]=0.05;
            guessWeights[1]=0.10;
            guessWeights[2]=0.07;
            guessWeights[3]=0.80;
        std::cout << "guessWeights: " << guessWeights << std::endl;

        RPCostFunction2 optFunc(cov, beta);

        // sum of parameters is equal to or less than x
        //RPConstSumLEQOne sumleqconst(1.0);
        //RPConstSumGEQOne sumgeqconst(0.1);
        //PositiveConstraint pconst;
        BoundaryConstraint bconst(0.0, 1.0);
        //CompositeConstraint constraints(pconst, sumgeqconst);

        //Problem myProb(optFunc, constraints, guessWeights);
        Problem myProb(optFunc, bconst, guessWeights);
        LevenbergMarquardt solver;
        EndCriteria::Type solvedCrit = solver.minimize(myProb, myEndCrit);

        std::cout << "Criteria: \t" << solvedCrit << std::endl;
        std::cout << "Roots: \t" << myProb.currentValue() << std::endl;
        std::cout << "Min Function Value: \t" << myProb.functionValue() << std::endl;

        Array y = myProb.currentValue();
        std::cout << "\nunnormalized risk parity weights (y)" << std::endl;
        std::cout << y << std::endl;
        std::cout << "Sum: " << y[0]+y[1]+y[2]+y[3] << std::endl;
        //[ 0.041073; 0.073940; 0.038539; 0.999708 ]

        Array x(n, 0.0);
        Real sumy = 0;
        for (Size i=0; i<y.size(); ++i) {sumy += y[i];}
        for (Size i=0; i<y.size(); ++i) {x[i] = y[i]/sumy;}
        std::cout << "\nrisk parity weights (x)" << std::endl;
        std::cout << x << std::endl;
        //[ 0.035615; 0.064114; 0.033417; 0.866854 ]

        // post optimization check
        Real portVar = 0.0;
        Array iPortVar(x.size(), 0.0);
        Array iPortVarShare(x.size(), 0.0);
        for (Size i=0; i<x.size(); ++i) {
            iPortVar[i] = x[i] * (cov * x)[i];
            portVar += iPortVar[i];
        }
        for (Size i=0; i<x.size(); ++i) {
            iPortVarShare[i] = iPortVar[i] / portVar;
        }
        std::cout << "\npost risk contribution share" << std::endl;
        std::cout << iPortVarShare << std::endl;
        //[ 0.250000; 0.250000; 0.250000; 0.250000 ]


        //LARGE_TITLE("Optimizer 1");
        //testOptimizer1();

        //LARGE_TITLE("Optimizer 2");
        //testOptimizer2();


        Real seconds = timer.elapsed();
        Integer hours = int(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = int(seconds/60);
        seconds -= minutes * 60;
        std::cout << " \nRun completed in ";
        if (hours > 0)
            std::cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            std::cout << minutes << " m ";
        std::cout << std::fixed << std::setprecision(0)
                  << seconds << " s\n" << std::endl;
        return 0;

    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cout << "unknown error" << std::endl;
        return 1;
    }
}
