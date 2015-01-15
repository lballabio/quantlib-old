/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2003 Ferdinando Ametrano
 Copyright (C) 2015 Peter Caspers

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

/*! \file errorfunction.hpp
    \brief Error function
*/

#ifndef quantlib_error_function_h
#define quantlib_error_function_h

#include <ql/types.hpp>
#include <functional>
#include <float.h>

namespace QuantLib {

    //! %Error function
    /*! formula here ...
        Used to calculate the cumulative normal distribution function
    */

    using std::fabs;
    using std::exp;

    template<class T = Real> class ErrorFunction_t : public std::unary_function<T,T> {
      public:
        ErrorFunction_t() {}
        // function
        T operator()(T x) const;
      private:
		static const Real tiny;
		static const Real one;
		static const Real erx;
        //
        // Coefficients for approximation to  erf on [0,0.84375]
        //
		static const Real efx;
		static const Real efx8;
		static const Real pp0;
		static const Real pp1;
		static const Real pp2;
		static const Real pp3;
		static const Real pp4;
		static const Real qq1;
		static const Real qq2;
		static const Real qq3;
		static const Real qq4;
		static const Real qq5;
        //
        // Coefficients for approximation to  erf  in [0.84375,1.25]
        //
		static const Real pa0;
		static const Real pa1;
		static const Real pa2;
		static const Real pa3;
		static const Real pa4;
		static const Real pa5;
		static const Real pa6;
		static const Real qa1;
		static const Real qa2;
		static const Real qa3;
		static const Real qa4;
		static const Real qa5;
		static const Real qa6;
        //
        // Coefficients for approximation to  erfc in [1.25,1/0.35]
        //
		static const Real ra0;
		static const Real ra1;
		static const Real ra2;
		static const Real ra3;
		static const Real ra4;
		static const Real ra5;
		static const Real ra6;
		static const Real ra7;
		static const Real sa1;
		static const Real sa2;
		static const Real sa3;
		static const Real sa4;
		static const Real sa5;
		static const Real sa6;
		static const Real sa7;
		static const Real sa8;
        //
        // Coefficients for approximation to  erfc in [1/.35,28]
        //
		static const Real rb0;
		static const Real rb1;
		static const Real rb2;
		static const Real rb3;
		static const Real rb4;
		static const Real rb5;
		static const Real rb6;
		static const Real sb1;
		static const Real sb2;
		static const Real sb3;
		static const Real sb4;
		static const Real sb5;
		static const Real sb6;
		static const Real sb7;
    };

	template<class T>
	const Real ErrorFunction_t<T>::tiny = QL_EPSILON;

	template<class T>
	const Real ErrorFunction_t<T>::one = 1.00000000000000000000e+00; /* 0x3FF00000, 0x00000000 */
	/* c = (float)0.84506291151 */
	template<class T>
	const Real ErrorFunction_t<T>::erx = 8.45062911510467529297e-01; /* 0x3FEB0AC1, 0x60000000 */
	//
	// Coefficients for approximation to  erf on [0,0.84375]
	//
	template<class T>
	const Real ErrorFunction_t<T>::efx = 1.28379167095512586316e-01; /* 0x3FC06EBA, 0x8214DB69 */
	template<class T>
	const Real ErrorFunction_t<T>::efx8 = 1.02703333676410069053e+00; /* 0x3FF06EBA, 0x8214DB69 */
	template<class T>
	const Real ErrorFunction_t<T>::pp0 = 1.28379167095512558561e-01; /* 0x3FC06EBA, 0x8214DB68 */
	template<class T>
	const Real ErrorFunction_t<T>::pp1 = -3.25042107247001499370e-01; /* 0xBFD4CD7D, 0x691CB913 */
	template<class T>
	const Real ErrorFunction_t<T>::pp2 = -2.84817495755985104766e-02; /* 0xBF9D2A51, 0xDBD7194F */
	template<class T>
	const Real ErrorFunction_t<T>::pp3 = -5.77027029648944159157e-03; /* 0xBF77A291, 0x236668E4 */
	template<class T>
	const Real ErrorFunction_t<T>::pp4 = -2.37630166566501626084e-05; /* 0xBEF8EAD6, 0x120016AC */
	template<class T>
	const Real ErrorFunction_t<T>::qq1 = 3.97917223959155352819e-01; /* 0x3FD97779, 0xCDDADC09 */
	template<class T>
	const Real ErrorFunction_t<T>::qq2 = 6.50222499887672944485e-02; /* 0x3FB0A54C, 0x5536CEBA */
	template<class T>
	const Real ErrorFunction_t<T>::qq3 = 5.08130628187576562776e-03; /* 0x3F74D022, 0xC4D36B0F */
	template<class T>
	const Real ErrorFunction_t<T>::qq4 = 1.32494738004321644526e-04; /* 0x3F215DC9, 0x221C1A10 */
	template<class T>
	const Real ErrorFunction_t<T>::qq5 = -3.96022827877536812320e-06; /* 0xBED09C43, 0x42A26120 */
	//
	// Coefficients for approximation to  erf  in [0.84375,1.25]
	//
	template<class T>
	const Real ErrorFunction_t<T>::pa0 = -2.36211856075265944077e-03; /* 0xBF6359B8, 0xBEF77538 */
	template<class T>
	const Real ErrorFunction_t<T>::pa1 = 4.14856118683748331666e-01; /* 0x3FDA8D00, 0xAD92B34D */
	template<class T>
	const Real ErrorFunction_t<T>::pa2 = -3.72207876035701323847e-01; /* 0xBFD7D240, 0xFBB8C3F1 */
	template<class T>
	const Real ErrorFunction_t<T>::pa3 = 3.18346619901161753674e-01; /* 0x3FD45FCA, 0x805120E4 */
	template<class T>
	const Real ErrorFunction_t<T>::pa4 = -1.10894694282396677476e-01; /* 0xBFBC6398, 0x3D3E28EC */
	template<class T>
	const Real ErrorFunction_t<T>::pa5 = 3.54783043256182359371e-02; /* 0x3FA22A36, 0x599795EB */
	template<class T>
	const Real ErrorFunction_t<T>::pa6 = -2.16637559486879084300e-03; /* 0xBF61BF38, 0x0A96073F */
	template<class T>
	const Real ErrorFunction_t<T>::qa1 = 1.06420880400844228286e-01; /* 0x3FBB3E66, 0x18EEE323 */
	template<class T>
	const Real ErrorFunction_t<T>::qa2 = 5.40397917702171048937e-01; /* 0x3FE14AF0, 0x92EB6F33 */
	template<class T>
	const Real ErrorFunction_t<T>::qa3 = 7.18286544141962662868e-02; /* 0x3FB2635C, 0xD99FE9A7 */
	template<class T>
	const Real ErrorFunction_t<T>::qa4 = 1.26171219808761642112e-01; /* 0x3FC02660, 0xE763351F */
	template<class T>
	const Real ErrorFunction_t<T>::qa5 = 1.36370839120290507362e-02; /* 0x3F8BEDC2, 0x6B51DD1C */
	template<class T>
	const Real ErrorFunction_t<T>::qa6 = 1.19844998467991074170e-02; /* 0x3F888B54, 0x5735151D */
	//
	// Coefficients for approximation to  erfc in [1.25,1/0.35]
	//
	template<class T>
	const Real ErrorFunction_t<T>::ra0 = -9.86494403484714822705e-03; /* 0xBF843412, 0x600D6435 */
	template<class T>
	const Real ErrorFunction_t<T>::ra1 = -6.93858572707181764372e-01; /* 0xBFE63416, 0xE4BA7360 */
	template<class T>
	const Real ErrorFunction_t<T>::ra2 = -1.05586262253232909814e+01; /* 0xC0251E04, 0x41B0E726 */
	template<class T>
	const Real ErrorFunction_t<T>::ra3 = -6.23753324503260060396e+01; /* 0xC04F300A, 0xE4CBA38D */
	template<class T>
	const Real ErrorFunction_t<T>::ra4 = -1.62396669462573470355e+02; /* 0xC0644CB1, 0x84282266 */
	template<class T>
	const Real ErrorFunction_t<T>::ra5 = -1.84605092906711035994e+02; /* 0xC067135C, 0xEBCCABB2 */
	template<class T>
	const Real ErrorFunction_t<T>::ra6 = -8.12874355063065934246e+01; /* 0xC0545265, 0x57E4D2F2 */
	template<class T>
	const Real ErrorFunction_t<T>::ra7 = -9.81432934416914548592e+00; /* 0xC023A0EF, 0xC69AC25C */
	template<class T>
	const Real ErrorFunction_t<T>::sa1 = 1.96512716674392571292e+01; /* 0x4033A6B9, 0xBD707687 */
	template<class T>
	const Real ErrorFunction_t<T>::sa2 = 1.37657754143519042600e+02; /* 0x4061350C, 0x526AE721 */
	template<class T>
	const Real ErrorFunction_t<T>::sa3 = 4.34565877475229228821e+02; /* 0x407B290D, 0xD58A1A71 */
	template<class T>
	const Real ErrorFunction_t<T>::sa4 = 6.45387271733267880336e+02; /* 0x40842B19, 0x21EC2868 */
	template<class T>
	const Real ErrorFunction_t<T>::sa5 = 4.29008140027567833386e+02; /* 0x407AD021, 0x57700314 */
	template<class T>
	const Real ErrorFunction_t<T>::sa6 = 1.08635005541779435134e+02; /* 0x405B28A3, 0xEE48AE2C */
	template<class T>
	const Real ErrorFunction_t<T>::sa7 = 6.57024977031928170135e+00; /* 0x401A47EF, 0x8E484A93 */
	template<class T>
	const Real ErrorFunction_t<T>::sa8 = -6.04244152148580987438e-02; /* 0xBFAEEFF2, 0xEE749A62 */
	//
	// Coefficients for approximation to  erfc in [1/.35,28]
	//
	template<class T>
	const Real ErrorFunction_t<T>::rb0 = -9.86494292470009928597e-03; /* 0xBF843412, 0x39E86F4A */
	template<class T>
	const Real ErrorFunction_t<T>::rb1 = -7.99283237680523006574e-01; /* 0xBFE993BA, 0x70C285DE */
	template<class T>
	const Real ErrorFunction_t<T>::rb2 = -1.77579549177547519889e+01; /* 0xC031C209, 0x555F995A */
	template<class T>
	const Real ErrorFunction_t<T>::rb3 = -1.60636384855821916062e+02; /* 0xC064145D, 0x43C5ED98 */
	template<class T>
	const Real ErrorFunction_t<T>::rb4 = -6.37566443368389627722e+02; /* 0xC083EC88, 0x1375F228 */
	template<class T>
	const Real ErrorFunction_t<T>::rb5 = -1.02509513161107724954e+03; /* 0xC0900461, 0x6A2E5992 */
	template<class T>
	const Real ErrorFunction_t<T>::rb6 = -4.83519191608651397019e+02; /* 0xC07E384E, 0x9BDC383F */
	template<class T>
	const Real ErrorFunction_t<T>::sb1 = 3.03380607434824582924e+01; /* 0x403E568B, 0x261D5190 */
	template<class T>
	const Real ErrorFunction_t<T>::sb2 = 3.25792512996573918826e+02; /* 0x40745CAE, 0x221B9F0A */
	template<class T>
	const Real ErrorFunction_t<T>::sb3 = 1.53672958608443695994e+03; /* 0x409802EB, 0x189D5118 */
	template<class T>
	const Real ErrorFunction_t<T>::sb4 = 3.19985821950859553908e+03; /* 0x40A8FFB7, 0x688C246A */
	template<class T>
	const Real ErrorFunction_t<T>::sb5 = 2.55305040643316442583e+03; /* 0x40A3F219, 0xCEDF3BE6 */
	template<class T>
	const Real ErrorFunction_t<T>::sb6 = 4.74528541206955367215e+02; /* 0x407DA874, 0xE79FE763 */
	template<class T>
	const Real ErrorFunction_t<T>::sb7 = -2.24409524465858183362e+01; /* 0xC03670E2, 0x42712D62 */

    //                 x
    //              2      |
    //     erf(x)  =  ---------  | exp(-t*t)dt
    //           sqrt(pi) \|
    //                 0
    //
    //     erfc(x) =  1-erf(x)
    //  Note that
    //      erf(-x) = -erf(x)
    //      erfc(-x) = 2 - erfc(x)
    //
    // Method:
    //  1. For |x| in [0, 0.84375]
    //      erf(x)  = x + x*R(x^2)
    //          erfc(x) = 1 - erf(x)           if x in [-.84375,0.25]
    //                  = 0.5 + ((0.5-x)-x*R)  if x in [0.25,0.84375]
    //     where R = P/Q where P is an odd poly of degree 8 and
    //     Q is an odd poly of degree 10.
    //                       -57.90
    //          | R - (erf(x)-x)/x | <= 2
    //
    //
    //     Remark. The formula is derived by noting
    //          erf(x) = (2/sqrt(pi))*(x - x^3/3 + x^5/10 - x^7/42 + ....)
    //     and that
    //          2/sqrt(pi) = 1.128379167095512573896158903121545171688
    //     is close to one. The interval is chosen because the fix
    //     point of erf(x) is near 0.6174 (i.e., erf(x)=x when x is
    //     near 0.6174), and by some experiment, 0.84375 is chosen to
    //     guarantee the error is less than one ulp for erf.
    //
    //      2. For |x| in [0.84375,1.25], let s = |x| - 1, and
    //         c = 0.84506291151 rounded to single (24 bits)
    //  erf(x)  = sign(x) * (c  + P1(s)/Q1(s))
    //  erfc(x) = (1-c)  - P1(s)/Q1(s) if x > 0
    //            1+(c+P1(s)/Q1(s))    if x < 0
    //  |P1/Q1 - (erf(|x|)-c)| <= 2**-59.06
    //     Remark: here we use the taylor series expansion at x=1.
    //      erf(1+s) = erf(1) + s*Poly(s)
    //           = 0.845.. + P1(s)/Q1(s)
    //     That is, we use rational approximation to approximate
    //          erf(1+s) - (c = (single)0.84506291151)
    //     Note that |P1/Q1|< 0.078 for x in [0.84375,1.25]
    //     where
    //      P1(s) = degree 6 poly in s
    //      Q1(s) = degree 6 poly in s
    //
    //      3. For x in [1.25,1/0.35(~2.857143)],
    //  erfc(x) = (1/x)*exp(-x*x-0.5625+R1/S1)
    //  erf(x)  = 1 - erfc(x)
    //     where
    //      R1(z) = degree 7 poly in z, (z=1/x^2)
    //      S1(z) = degree 8 poly in z
    //
    //      4. For x in [1/0.35,28]
    //  erfc(x) = (1/x)*exp(-x*x-0.5625+R2/S2) if x > 0
    //          = 2.0 - (1/x)*exp(-x*x-0.5625+R2/S2) if -6<x<0
    //          = 2.0 - tiny        (if x <= -6)
    //  erf(x)  = sign(x)*(1.0 - erfc(x)) if x < 6, else
    //  erf(x)  = sign(x)*(1.0 - tiny)
    //     where
    //      R2(z) = degree 6 poly in z, (z=1/x^2)
    //      S2(z) = degree 7 poly in z
    //
    //      Note1:
    //     To compute exp(-x*x-0.5625+R/S), let s be a single
    //     precision number and s := x; then
    //      -x*x = -s*s + (s-x)*(s+x)
    //          exp(-x*x-0.5626+R/S) =
    //          exp(-s*s-0.5625)*exp((s-x)*(s+x)+R/S);
    //      Note2:
    //     Here 4 and 5 make use of the asymptotic series
    //            exp(-x*x)
    //      erfc(x) ~ ---------- * ( 1 + Poly(1/x^2) )
    //            x*sqrt(pi)
    //     We use rational approximation to approximate
    //  g(s)=f(1/x^2) = log(erfc(x)*x) - x*x + 0.5625
    //     Here is the error bound for R1/S1 and R2/S2
    //  |R1/S1 - f(x)|  < 2**(-62.57)
    //  |R2/S2 - f(x)|  < 2**(-61.52)
    //
    //      5. For inf > x >= 28
    //  erf(x)  = sign(x) *(1 - tiny)  (raise inexact)
    //  erfc(x) = tiny*tiny (raise underflow) if x > 0
    //          = 2 - tiny if x<0
    //
    //      7. Special case:
    //  erf(0)  = 0, erf(inf)  = 1, erf(-inf) = -1,
    //  erfc(0) = 1, erfc(inf) = 0, erfc(-inf) = 2,
    //      erfc/erf(NaN) is NaN

    template<class T> T ErrorFunction_t<T>::operator()(T x) const {

        T R,S,P,Q,s,y,z,r, ax;

        /* not portable!

        // The finite() function returns a non-zero value if value is
        // neither infinite nor a "not-a-number" (NaN) value,
        // and 0 otherwise.
        if (!_finite(x)) {
            //  The isnan() function returns a non-zero value if value is
            // "not-a-number" (NaN), and 0 otherwise.
            if (_isnan(x))
                return x;
            else
                return   ( x > 0 ? 1 : -1);
        }

        */

        ax = fabs(x);

        if(ax < 0.84375) {      /* |x|<0.84375 */
            if(ax < 3.7252902984e-09) { /* |x|<2**-28 */
                if (ax < DBL_MIN*16)
                    return 0.125*(8.0*x+efx8*x);  /*avoid underflow */
                return x + efx*x;
            }
            z = x*x;
            r = pp0+z*(pp1+z*(pp2+z*(pp3+z*pp4)));
            s = one+z*(qq1+z*(qq2+z*(qq3+z*(qq4+z*qq5))));
            y = r/s;
            return x + x*y;
        }
        if(ax <1.25) {      /* 0.84375 <= |x| < 1.25 */
            s = ax-one;
            P = pa0+s*(pa1+s*(pa2+s*(pa3+s*(pa4+s*(pa5+s*pa6)))));
            Q = one+s*(qa1+s*(qa2+s*(qa3+s*(qa4+s*(qa5+s*qa6)))));
            if(x>=0) return erx + P/Q; else return -erx - P/Q;
        }
        if (ax >= 6) {      /* inf>|x|>=6 */
            if(x>=0) return one-tiny; else return tiny-one;
        }

        /* Starts to lose accuracy when ax~5 */
        s = one/(ax*ax);

        if(ax < 2.85714285714285) { /* |x| < 1/0.35 */
            R = ra0+s*(ra1+s*(ra2+s*(ra3+s*(ra4+s*(ra5+s*(ra6+s*ra7))))));
            S=one+s*(sa1+s*(sa2+s*(sa3+s*(sa4+s*(sa5+s*(sa6+s*(sa7+s*sa8)))))));
        } else {    /* |x| >= 1/0.35 */
            R=rb0+s*(rb1+s*(rb2+s*(rb3+s*(rb4+s*(rb5+s*rb6)))));
            S=one+s*(sb1+s*(sb2+s*(sb3+s*(sb4+s*(sb5+s*(sb6+s*sb7))))));
        }
        r = exp( -ax*ax-0.5625 +R/S);
        if(x>=0) return one-r/ax; else return  r/ax-one;

    }

    typedef ErrorFunction_t<Real> ErrorFunction;

}


#endif
