#ifndef FDHESTONTEST_HPP
#define FDHESTONTEST_HPP

#include <ql/quantlib.hpp>
using namespace QuantLib;

class FdHestonTest
{
public:
    static void testFdmHestonBarrier();
    static void testFdmHestonBarrierVsBlackScholes();
    static void testFdmHestonAmerican();
    static void testFdmHestonIkonenToivanen();
    static void testFdmHestonEuropeanWithDividends();
    static void testFdmHestonConvergence();
    static void testFdmHestonBlackScholes();
    static void testBlackScholesFokkerPlanckFwdEquation();
    static void testSquareRootZeroFlowBC();
    static void testTransformedZeroFlowBC();
    static void testSquareRootEvolveWithStationaryDensity();
    static void testSquareRootFokkerPlanckFwdEquation();
    static void testHestonFokkerPlanckFwdEquation();
    struct NewBarrierOptionData {
        Barrier::Type barrierType;
        Real barrier;
        Real rebate;
        Option::Type type;
        Real strike;
        Real s;        // spot
        Rate q;        // dividend
        Rate r;        // risk-free rate
        Time t;        // time to maturity
        Volatility v;  // volatility
    };
};

#endif // FDHESTONTEST_HPP
