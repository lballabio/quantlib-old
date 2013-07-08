
#define urand() ((float) random() / (2147483647 + 1.0))
/* define a function of generating uniform random variables */

extern const double pi=3.1415926535897932385; 

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

    Function: nrand
              nrandv                  
	
    Purpose: nrandv --- Using Box Muller method, generates a vector of i.i.d. 
             Gaussian random variables of given length. Each random variable 
             has mean zero and variance 1. 
             nrand --- The same method as above but generate two random 
                       variables 
	
    Input:   nrandv:
             1. int is the length of the vector.
             2. vec is a double array.

             nrand:
                vec is a double array.             

    Output: a vector of random variables

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

void nrand(double* vec)
{
	double R, theta;
	    R = sqrt(-2.*log((float) urand()));
	    theta = 2.*pi*urand();
	    vec[0] = R*cos(theta);
	    vec[1] = R*sin(theta);
}	

void nrandv(const int i, double* vec)
{
        int j;
        double R, theta;
        for ( j = 0 ; j < i-1 ; j += 2)
        {
            R = sqrt(-2.*log(urand()));
            theta = 2.*pi*urand();
            vec[j] = R*cos(theta);
            vec[j+1] = R*sin(theta);
        }
        if (j == i-2) return;
        R = sqrt(-2.*log(urand()));
        theta = 2.*pi*urand();
        vec[i-1] = R*cos(theta);
        vec[i-2] = R*sin(theta);
}
 






