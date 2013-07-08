// http://www.bnikolic.co.uk/blog/ql-fitting-curves.html
// Bojan Nikolic <bojan@bnikolic.co.uk>
//
// After orignal by Nicolas Di Cesare <Nicolas.Dicesare@free.fr> ?

#include <iostream>
#include <fstream>
#include <cmath>
#include <vector>

#include <boost/assign/std/vector.hpp>

#include <ql/quantlib.hpp>

using namespace std;

/*!
  Brace volatility term structure function
  (similar to Nelson-Siegel function for zero rate)

  sigma(t) = (a+b*t)*exp(-c*t) + d
*/
class BraceVolatilityFunction
{
  //! coefficients of the function
  double a_, b_, c_, d_;
public :
  //! Default constructor to set coeffs
  inline BraceVolatilityFunction(double a,
                                 double b,
                                 double c,
                                 double d):
    a_(a),
    b_(b),
    c_(c),
    d_(d)
  {}

  ~BraceVolatilityFunction()
  {}

  //! operator to evaluate the function at given time to maturity
  double operator() (double t)
  {
    return (a_+b_*t)*exp(-c_*t) + d_;
  }

  //! First derivative with respect to a
  double da(double t)
  {
    return exp(-c_*t);
  }

  //! First derivative with respect to b
  double db(double t)
  {
    return t*exp(-c_*t);
  }

  //! First derivative with respect to c
  double dc(double t)
  {
    return -t*(a_+b_*t)*exp(-c_*t);
  }

  //! First derivative with respect to d
  double dd(double t)
  {
    return 1.;
  }

};


/*!
  Curve Fitting Problem
 */
class CurveFittingProblem:
  public QuantLib::LeastSquareProblem
{
  const QuantLib::Array &ttm_;
  const QuantLib::Array &target_;
public :
  /*!
    Default constructor : set time to maturity vector
    and target value
  */
  CurveFittingProblem(const QuantLib::Array &ttm,
                      const QuantLib::Array &target):
    ttm_(ttm),
    target_(target)
  {}

  //! Destructor
  virtual ~CurveFittingProblem() {}

  //! Size of the least square problem
  virtual size_t size()
  {
    return ttm_.size();
  }

  //! return function and target values
  virtual void targetAndValue(const QuantLib::Array& abcd,
                              QuantLib::Array& target,
                              QuantLib::Array& fct2fit)
  {
    BraceVolatilityFunction bvf(abcd[0],
                                abcd[1],
                                abcd[2],
                                abcd[3]);

    target = target_;// target values
    for (int i=0; i<ttm_.size(); ++i)
        fct2fit[i] = bvf(ttm_[i]);

  }

  //! return function, target and first derivatives values
  virtual void targetValueAndGradient(const QuantLib::Array& abcd,
                                      QuantLib::Matrix& grad_fct2fit,
                                      QuantLib::Array& target,
                                      QuantLib::Array& fct2fit)
  {
    BraceVolatilityFunction bvf(abcd[0],abcd[1],abcd[2],abcd[3]);
    target = target_;// target values
    for (int i=0; i<ttm_.size(); ++i) {
        // function value at current point abcd
        fct2fit[i] = bvf(ttm_[i]);
        /*
            matrix of first derivatives :
            the derivatives with respect to the parameter a,b,c,d
            are stored by row.
        */
        grad_fct2fit[i][0] = bvf.da(ttm_[i]);
        grad_fct2fit[i][1] = bvf.db(ttm_[i]);
        grad_fct2fit[i][2] = bvf.dc(ttm_[i]);
        grad_fct2fit[i][3] = bvf.dd(ttm_[i]);
    }

  }
};

/*
    We define here an inverse problem to show how to fit
    parametric function to data.
*/
int main()
{
  using namespace boost::assign;

  /*
    Parameter values that produce the volatility hump.
    Consider it as optimal values of the curve fitting
    problem.
  */
  std::vector<double> abcd_;
  abcd_  +=
    0.147014,
    0.057302,
    0.249964,
    0.148556;


  QuantLib::Array abcd(abcd_.begin(), abcd_.end());


  cout << "Optimal values  : " << abcd << endl;

  // Define the target volatility function
  BraceVolatilityFunction bvf(abcd[0],
                              abcd[1],
                              abcd[2],
                              abcd[3]);

  try {

    // start date of volatility
    double startDate = 0.0;
    // end date of volatility
    double endDate = 20.;
    // period length between values (in year fraction : quarterly)
    double period = 0.25;

    int i;
    // number of period
    size_t periodNumber = (size_t)(endDate / period);

    QuantLib::Array targetValue(periodNumber);
    QuantLib::Array timeToMaturity(periodNumber);

    // Fill target and time to maturity arrays
    for (i=0; i<periodNumber; ++i)
    {
      const double t = startDate + i*period;
      timeToMaturity[i] = t;
      targetValue[i] = bvf(t);
    }

    // Accuracy of the optimization method
    QuantLib::Real accuracy = 1e-10;// It is the square of the accuracy
    // Maximum number of iterations
    QuantLib::Size maxiter = 10000;

    QuantLib::Array initialValue(4, 0.1);

    // Least square optimizer
    QuantLib::NoConstraint nc;
    QuantLib::NonLinearLeastSquare lsqnonlin(nc,
                                             accuracy,
                                             maxiter);


    // Define the least square problem
    CurveFittingProblem cfp(timeToMaturity,
                            targetValue);

    // Set initial values
    lsqnonlin.setInitialValue(initialValue);

    // perform fitting
    QuantLib::Array solution = lsqnonlin.perform(cfp); /////////////////////

    cout << endl
         << "Optimal values  : " << abcd << "\n"
         << "Solution values : " << solution << "\n"
         << "Solution Error  : " << (solution - abcd ) << endl;

    // save in given format
    BraceVolatilityFunction bvf2(solution[0],
                                 solution[1],
                                 solution[2],
                                 solution[3]);

    std::ofstream file("curve.csv");
    const char separator[] = ", ";
    // CSV format
    for (i=0; i<periodNumber; ++i)
    {
      const double t = startDate + i*period;
      file<<t
          <<separator
          <<bvf(t)
          <<separator
          <<bvf2(t)<< endl;
    }
    file.close();

  }
  catch(QuantLib::Error & e)
  {
    cerr<<e.what()
        << endl;;
  }

  return 1;
}