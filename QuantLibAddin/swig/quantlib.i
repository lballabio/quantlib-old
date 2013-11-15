
%rename("arse") Real;
%module QuantLibAddin
%{
#include <ql/math/comparison.hpp>
#include <ql/quotes/simplequote.hpp>
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

