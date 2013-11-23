
%module QuantLibAddin
%{
#include <ql/quotes/simplequote.hpp>
#include <ql/math/comparison.hpp>
%}
typedef double Real;
namespace QuantLib {
    bool close(Real x, Real y);
    class SimpleQuote : public Quote {
      public:
        SimpleQuote(Real value = Null<Real>());
        Real value() const;
    };
}

