/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007, 2009 StatPro Italia srl

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

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif

#include <boost/math/distributions.hpp>


Real blackScholesPrice(const Real& spot,
                         const Real& strike,
                         const Rate& rd,
                         const Rate& rf,
                         const Volatility& vol,
                         const Time& tau,
                         const Integer& phi){

    boost::math::normal_distribution<> d(0.0, 1.0);
    Real dp,dm, fwd, stdDev, res, domDf, forDf;

    domDf=std::exp(-rd*tau); forDf=std::exp(-rf*tau);
    fwd=spot*forDf/domDf;
    stdDev=vol*std::sqrt(tau);

    dp=(std::log(fwd/strike)+0.5*stdDev*stdDev)/stdDev;
    dm=(std::log(fwd/strike)-0.5*stdDev*stdDev)/stdDev;

    res=phi*domDf*(fwd*cdf(d,phi*dp)-strike*cdf(d,phi*dm));
    return res;
}

Real impliedVolProblem(const Real & spot,
    const Rate & strike,
    const Rate & rd,
    const Rate & rf,
    const Volatility & vol,
    const Time & tau,
    const Integer & phi,
    const Real & price) {
    return blackScholesPrice(spot ,strike , rd ,rf ,vol ,tau , phi ) - price;
}

class CallProblemFunction : public CostFunction {
private :
    Real C1_ ,C2_ ,C3_ ,C4_ ,K1_ ,K2_ ,K3_ , K4_;
    Rate rd_ , rf_ ;
    Integer phi_ ;
    Time tau_ ;
public :
    CallProblemFunction ( const Rate & rd , const Rate & rf , const Time & tau , const Integer & phi ,
    const Real & K1 , const Real & K2 , const Real & K3 , const Real & K4 ,
    const Real & C1 , const Real & C2 , const Real & C3 , const Real & C4)
        : rd_ (rd), rf_ (rf), phi_ (phi), tau_ ( tau ),
        C1_ (C1), C2_ (C2), C3_ (C3), C4_(C4),
        K1_ (K1), K2_ (K2), K3_ (K3), K4_(K4 ){}
    Real value ( const Array & x) const {
        Array tmpRes = values (x);
        Real res = tmpRes [0]* tmpRes [0];
        res += tmpRes [1]* tmpRes [1];
        res += tmpRes [2]* tmpRes [2];
        res += tmpRes [3]* tmpRes [3];
        return res;
    }
    Disposable <Array > values ( const Array & x) const {
        Array res (4);
        res [0]= blackScholesPrice (x[0] , K1_ ,rd_ ,rf_ ,x[1] , tau_ , phi_ )- C1_ ;
        res [1]= blackScholesPrice (x[0] , K2_ ,rd_ ,rf_ ,x[1] , tau_ , phi_ )- C2_ ;
        res [2]= blackScholesPrice (x[0] , K3_ ,rd_ ,rf_ ,x[1] , tau_ , phi_ )- C3_ ;
        res [3]= blackScholesPrice (x[0] , K4_ ,rd_ ,rf_ ,x[1] , tau_ , phi_ )- C4_ ;
        return res;
    }
};


int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;

        //---------------------------------------------------------------------------

        LARGE_TITLE("Levenberg-Marquart Optimization");


        // setup of market parameters
        Real spot = 98.51;
        Volatility vol = 0.134;
        Real K1 = 87.0 , K2 = 96.0 , K3 = 103.0 , K4 = 110.0;
        Rate rd = 0.002 , rf = 0.01;
        Integer phi = 1;
        Time tau = 0.6;

        // calculate Black Scholes prices
        Real C1 = blackScholesPrice(spot ,K1 ,rd ,rf ,vol ,tau , phi);
        Real C2 = blackScholesPrice(spot ,K2 ,rd ,rf ,vol ,tau , phi);
        Real C3 = blackScholesPrice(spot ,K3 ,rd ,rf ,vol ,tau , phi);
        Real C4 = blackScholesPrice(spot ,K4 ,rd ,rf ,vol ,tau , phi);

        CallProblemFunction optFunc(rd , rf , tau , phi ,K1 , K2 , K3 , K4 , C1 , C2 , C3 , C4 );

        Size maxIterations = 1000;
        Size minStatIterations = 100;
        Real rootEpsilon = 1e-5;
        Real functionEpsilon = 1e-5;
        Real gradientNormEpsilon = 1e-5;

        EndCriteria myEndCrit(maxIterations , minStatIterations, rootEpsilon,
        functionEpsilon, gradientNormEpsilon);
        Array startVal(2); startVal[0]=80.0; startVal[1]=0.20;
        NoConstraint constraint;

        Problem myProb(optFunc, constraint, startVal);

        LevenbergMarquardt solver;

        EndCriteria :: Type solvedCrit = solver.minimize (myProb , myEndCrit);

        std :: cout << " Criteria :" << solvedCrit << std::endl;
        std :: cout << " Root :" << myProb.currentValue() << std::endl;
        std :: cout << " Min Function Value :" << myProb.functionValue() << std::endl ;




        //---------------------------------------------------------------------------

        // End test
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
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}
