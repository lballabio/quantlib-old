
// defines for runge kutta
#define MAXSTP 10000
#define TINY 1.0E-30

#define SAFETY 0.9
#define PGROW -0.2
#define PSHRINK -0.25
#define ERRCON 1.89E-4

// simulated annealing
#define MAX_DOUBLE 1E15 // big value for objective function that is used in case of NAN-values

// simple stochvol model
#define FOURIERINTEGRATIONBOUND 50.0 // the inverse Fourier Integral is computed from -this value to +this value
#define FOURIERACCURACY 1.0E-9	  	 // accuracy for the Fourier Integral calculation (simpson)
#define BLACKACCURACY 1.0E-5	 	 // accuracy for the Black implied vola calculation
#define SIMPLEMODELMCPATHS 1000000	 // number of paths for simple model mc calculation
#define SIMPLEMODELMCSTEPS 100		 // number of discretization steps (0 to maturity!) for simple model mc calculation

// effective parameters stoch vol model
#define LAMBDAACCURACY 1.0E-5 // accuracy for the effective volatility brent search (Piterbarg, 6.23)
#define ODEEPS 1.0E-4 // epsilon for ODE
#define ODEHSTART 0.01 // start step size for ODE
#define SKEWINTEGRALPOINTS 32 // number of points for gauss legendre integration to compute effective skew integral 
#define NUMDIFFH 1.0E-4 // h for numerical differentation Swap Rates by libors

// char Fct Heston
#define ODEEPS2 1.0E-4 // epsilon for ODE to compute characteristic function value

// spread option approximation Antonov
#define INTEGRALACCURACY 1.0E-4			// accuracy for the inner laplace integral (dxi2)
#define OUTERINTEGRALACCURACY 1.0		// accuracy for the outer laplace integral (dxi1)
#define INTEGRATIONBOUND 50.0		// 1 dim integrationb bound (zero strike case)
#define INTEGRATIONBOUNDU 2.0	    // laplace integral bound for u 
#define INTEGRATIONBOUNDV 10.0		// laplace integral bound for v (u,v) -> (xi1=u,xi2=alpha*u+v)
#define MAXINTEGRATIONSIZE 10		// the size parameter for gsl qawo integration
#define LAPLACEINTEGRALPOINTS 32	// the number of points used in legendre integral for inverse laplace outer integral

// spread option approximation Lutz
#define HERMITEPOINTS 64 // number of points for hermite integration (inner cmsso integral, lutz)
#define HSTEP 1.0E-4 // step to numerically compute the derivative of h (for newton algorithm)
#define HACC 1.0E-4 // accuracy for the zero of h
#define MAXEVALNEWTON 500 // max evaluation steps for newton algorithm (search zero of h)
#define DENSITYACCURACY 1.0E-8 // accuracy for computing gauss labatto integral for density f(v)
#define DENSITYLOWERINTEGRALBOUND 1.0E-6 // lower integration bound for computing density integral
#define INTVACCURACY 1.0E-6 // Accuracy for outer integral (by integrated variance)
#define INTVLOWERBOUND 0.1  // lower bound (is not (!) scaled with T) for outer integral
#define INTVUPPERBOUND 5.0  // upper bound (is scaled with T, i.e. value for T=1 here) for outer integral
#define MAXEVALGL 10000 // max function evaluations gauss labatto (density integral, outer integral (by int. variance)

// sigma parametrization
#define TINYBETA 1.0E-10 // if abs(beta) < this number then exp(beta*t) is replaced by 1.0 in computation of primitive function

// libor curve monte carlo
#define REFINEMENTSTEPS 4 // number of steps per rateTime-Interval
#define MINRATE 0.0010 // minimum possible interest rate
