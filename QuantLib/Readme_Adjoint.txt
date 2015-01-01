Existing client code should compile unchanged and run without any
differences in result or computation time. Minor adjustments to
existing client code will be unavoidable however. Here we assume that
certain internals of the library, though publicly accesiable, are not
really used in client code, e.g. Linear().interpolate(...) to get a
LinearInterpolation object, see below for more details.

It is not doable to make the whole library code AD enabled. The parts
of the library that are not ADized, should at best compile without any
changes, but here and there amendments will be necessary.

Where classes are templated the implementation part is moved to the
header file and the implementation file is deleted.

In general, type names keep their original meaning. Where we need a
templated version, we mark it by underscore t, e.g. we have Quote_t as
a new template class and keep Quote via a typedef Quote_t<Real> Quote
the same as before. As another example more relevant for client code,
SimpleQuote(0.03) must stay a valid expression. Nevertheless we
generalize SimpleQuote to a template class SimpleQuote_t<T> with a
typedef SimpleQuote_t<Real> SimpleQuote, so that we can set up a
simple quote based on an AD type (possibly abbreviated with another
typedef, say typedef SimpleQuote_t<AD<double>> SimpleQuoteAD, later on).

Interpolation factories change their meaning. Linear now is a
template, so that the actual interpolation class is retrieved by
Linear<Real>().interpolate(...) and is of type
LinearInterpolation_t<Real>. The latter is, again by typedef, the same
as LinearInterpolation, i.e. here the semantics stays the same. The
rationale is that the factory method  is more used internally in the
library, in the client code it should rather appear as a template
parameter (e.g. as the interpolation trait in PiecewiseYieldCurve).

Similarly, bootstrap traits change their meaning becoming now templated
by a type T in addition. PiecewiseYieldTermStructure takes an
additional type T. Similarly, bootstrap helper, bootstrap error, bootstrap.

Functions are generalized to templates, because we believe that in
most cases the inference for the template parameters should work when
not explicitly given. This is not always the case, e.g. close(a,0)
worked for a double, but now does not compile, because 0 is
interpreted as an integer. close(a,0.) does work however.

Not all variables are template'ized, typically e.g. Time inputs stay
to be of type Time = double. However, since all the work is done, it
would be easy to extend the template type to them as well. 

Notes: The constants in the error function do only compile with
c++11 at the moment, we should generalize this to handle both c++03
and 11.

FloatingLeg, DigitalFloatingLeg template classes change their
meaning. However, these are internal helper classes only, so this
should not be harmful.

couponpricer.hpp was subdivided into couponpricerbase.hpp and
couponpricer.hpp. Since the latter includes the former, no external
change is introducd.

Likewise iborcoupon.hpp was subdivided into iborcouponbase.hpp and
iborcoupon.hpp.

setCouponPricers must often be changed to setCouponPricers<Real>,
because infering the template types does not work => investigate why.

Moved subperiodcouponpricer to couponpricer.

IndexManager is a singleton for each type. Fixings are not copied from the double instance to the AD<double> instance !


Done:

PiecewiseYieldCurve etc.
PiecewiseYoYInflationCurve, PiecewiseZeroInflaitonCurve etc.

LinearInterpolation


Todo:

CreditCurves
Voltermstructures

Other 1d interpolations

LocalBootstrap



