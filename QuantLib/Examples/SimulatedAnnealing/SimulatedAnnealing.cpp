#include <ql/quantlib.hpp>

#include <iostream>

using namespace QuantLib;


int main(int, char* []) {

    MersenneTwisterUniformRng mt(42);

    //SimulatedAnnealing<> sa(1.0,1000.0,0.05,10);
    //SimulatedAnnealing<> sa(1.0,2000.0,500,2.0);
    //LevenbergMarquardt lm;
    //Simplex sa(1.0);

    //DifferentialEvolution sa;

    class cfct : public CostFunction {
    public:
        Real value(const Array &x) const {
            //return -cos(x[0])*cos(x[1])*exp(-std::pow(x[0]-M_PI,2.0)-std::pow(x[1]-M_PI,2.0));
            Real y = (2.0+std::sin(x[0])*std::sin(x[1]))*(x[0]*x[0]+x[1]*x[1]);
            std::cout << "f(" << x[0] << "," << x[1] << ")=" << y << std::endl;
            return y;
        }
        Disposable<Array> values(const Array &x) const {
            Array res(2);
            res[0] = x[0];
            res[1] = x[1];
            return res;
        }
    } aCfct;

    EndCriteria ec(1000000,50,1E-10,1E-10,1E-10);
    NoConstraint constraint;
    Array initial(2);
    initial[0] = 10.0;//30.0;
    initial[1] = 20.0;//-20.0;
    Problem p(aCfct,constraint,initial);

    EndCriteria::Type ret = sa.minimize(p,ec);
    // EndCriteria::Type ret = lm.minimize(p,ec);

    std::cout << "Optimizer returns " << ret << std::endl;
    Array sol = p.currentValue();
    std::cout << "Solution found " << sol[0] << " , " << sol[1] << " => " << p.functionValue() << std::endl;

    return(0);

}

