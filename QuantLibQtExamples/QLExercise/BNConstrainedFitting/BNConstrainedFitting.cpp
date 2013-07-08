// Bojan Nikolic <bojan@bnikolic.co.uk>
//
// Example of using QuantLib Optimisation routines to do a contrained
// fit

#include <iostream>
#include <boost/assign/list_of.hpp>
#include <ql/quantlib.hpp>


/*! This class defines the problem to be optimised -- in this case
   fitting a sine wave to numbr of observed points
 */
class SinFit:
  public QuantLib::LeastSquareProblem
{
  const QuantLib::Array x_;
  const QuantLib::Array yobs_;

public :

  /*!
  */
  SinFit(const std::vector<double> &x,
      const std::vector<double> &yobs):
    x_(QuantLib::Array(x.begin(), x.end())),
    yobs_(QuantLib::Array(yobs.begin(), yobs.end()))
  {}

  //! Size of the least square problem
  virtual size_t size()
  {
    return x_.size();
  }

  //! return function and target values
  virtual void targetAndValue(const QuantLib::Array& p,
                           QuantLib::Array& target,
                           QuantLib::Array& y)
  {
    target = yobs_;
    for (size_t i=0; i<x_.size(); ++i)
    {
      y[i] = p[0]*std::sin(p[1]+p[2]*x_[i]);
    }
  }

  //! return function, target and first derivatives values
  virtual void targetValueAndGradient(const QuantLib::Array& p,
                                   QuantLib::Matrix& dy,
                                   QuantLib::Array& target,
                                   QuantLib::Array& y)
  {
    target = yobs_;
    const double a=p[0];
    const double b=p[1];
    const double c=p[2];

    for (size_t i=0; i<x_.size(); ++i)
    {
      y[i] = a*std::sin(b+c*x_[i]);

      dy[i][0]=std::sin(b+c*x_[i]);
      dy[i][1]=a*std::cos(b+c*x_[i]);
      dy[i][2]=a*x_[i]*std::cos(b+c*x_[i]);
    }
  }
};

int main(void)
{


  std::vector<double> x=boost::assign::list_of(0)(0.1)(0.2)(0.3)(1.0)(2.0);
  std::vector<double> yobs(x.size());
  for(size_t i=0; i<x.size(); ++i)
    yobs[i]=0.5*std::sin(1.1*x[i]);

  SinFit f(x, yobs);

  QuantLib::Real accuracy = 1e-20;
  QuantLib::Size maxiter = 10000;
  /* Starting point for the minimisation */
  QuantLib::Array p0(3, 1.0);

  { // no Constraint optimisation
    QuantLib::NoConstraint nc;
    QuantLib::NonLinearLeastSquare lsqnonlin(nc,
                                          accuracy,
                                          maxiter);
    // Set initial values
    lsqnonlin.setInitialValue(p0);
    QuantLib::Array solution = lsqnonlin.perform(f);

    std::cout<<solution<<std::endl;
  }

  { // All parameters must be positive

    QuantLib::PositiveConstraint pc; ////////////////////////////////////////
    QuantLib::NonLinearLeastSquare lsqnonlin(pc,
                                          accuracy,
                                          maxiter); ////////////////////////////////////////
    // Set initial values
    lsqnonlin.setInitialValue(p0);
    QuantLib::Array solution = lsqnonlin.perform(f); ////////////////////////////////////////

    std::cout<<solution<<std::endl;
  }

  { // Box constraint containing the optimum. Note that the same
    // bounds applied to all of the parameters

    QuantLib::BoundaryConstraint bc(-1,2); ////////////////////////////////////////
    QuantLib::NonLinearLeastSquare lsqnonlin(bc,
                                          accuracy,
                                          maxiter); ////////////////////////////////////////
    // Set initial values
    lsqnonlin.setInitialValue(p0);
    QuantLib::Array solution = lsqnonlin.perform(f); ////////////////////////////////////////

    std::cout<<solution<<std::endl;
  }

  { // Box constraint not containing the optimum

    QuantLib::BoundaryConstraint bc(0.7,2);
    QuantLib::NonLinearLeastSquare lsqnonlin(bc,
                                          accuracy,
                                          maxiter);
    // Set initial values
    lsqnonlin.setInitialValue(p0);
    QuantLib::Array solution = lsqnonlin.perform(f);

    std::cout<<solution<<std::endl;
  }

}