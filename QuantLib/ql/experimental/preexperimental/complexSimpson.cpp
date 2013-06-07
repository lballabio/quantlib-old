#include <complexSimpson.hpp>

namespace QuantLib {

	complex<double> ComplexSimpsonIntegral::operator()(FunctionC& f, Real a, Real b) const {

            // start from the coarsest trapezoid...
            Size N = 1;
            complex<double> I = (f(a)+f(b))*(b-a)/2.0, newI;
            complex<double> adjI = I, newAdjI;
            // ...and refine it
            Size i = 1;
            do {
				// integrate method
				complex<double> sum = 0.0;
				double dx = (b-a)/N;
				double x = a + dx/2.0;
				for (Size i=0; i<N; x += dx, ++i)
					sum += f(x);
				newI = (I + dx*sum)/2.0;
				//
                N *= 2;
                newAdjI = (4.0*newI-I)/3.0;
                // good enough? Also, don't run away immediately
                if(abs(adjI-newAdjI) <= accuracy_ && i > 5)
                    // ok, exit
                    return newAdjI;
                // oh well. Another step.
                I = newI;
                adjI = newAdjI;
                i++;
            } while (i < maxIterations_);
            QL_FAIL("max number of iterations reached");
	}
		

}