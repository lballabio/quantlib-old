#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/experimental/models/qg1dlinearmodel.hpp>

#include <iostream>

using namespace QuantLib;

int main() {

    Date refDate(17,Sep,2015);
    Settings::instance().evaluationDate() = refDate;

    Handle<YieldTermStructure> yts(boost::make_shared<FlatForward>(0,TARGET(),0.03,Actual365Fixed()));

    // no steps

    std::vector<Date> stepDates;

    for(Size i=1;i<10;++i) {
        stepDates.push_back(TARGET().advance(refDate,i*Years));
    }

    std::vector<Real> lambda(10,1.0);
    std::vector<Real> alpha(10,1.0);
    std::vector<Real> beta(10,1.0);
    std::vector<Real> kappa(10,0.02);

    Qg1dLinearModel model(yts,stepDates,lambda, alpha, beta, kappa);

    // test kappa, h, G

    std::cout.precision(16);

    Real t = 0.0;
    while( t < 10.0 ) {
        std::cout << t << " " << model.kappa(t) << " " << model.h(t) << " " << std::exp(-0.02*t) << " " << model.G(0.0,t) << " " << -(exp(-0.02*t)-1.0)/0.02 << std::endl;
        t += 0.01;
    }


    return 0;

}
