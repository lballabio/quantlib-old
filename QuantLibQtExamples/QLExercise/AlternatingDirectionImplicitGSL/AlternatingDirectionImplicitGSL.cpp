// http://www.quantcode.com/modules/mydownloads/viewcat.php?cid=10&min=30&orderby=hitsD&show=5
// Asian option price using Alternating Direction Implicit (ADI)

#include <iostream>
#include <stdlib.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_qrng.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_integration.h>

using namespace std;

#define LENGTH(a) (sizeof(a)/sizeof(a[0]))

//********   Enter Input parameters **************
double S0=100; //Spot price (S0)
double K=100; // option strike
double sigma=0.5; //volatility
double r=0.1; //Risk free rate [r]
double T=1; //Maturity of Option in years (T)

double tol=0.000001; //tolerance required for SOR convergence
int timesteps=100; //no of time steps
int stocksteps=100; //no of points in stock axis
int AvgSteps=100; //no of points in variance axis
//*****************************************************

gsl_matrix* g_Vmat;
gsl_matrix* g_Vmatnew;
gsl_matrix* g_Vmatold;
double g_knew=0.0;
double g_kold=0.0;

void my_handler (const char * reason,
              const char * file,
              int line,
              int gsl_errno)
{
    printf(reason);
    system("PAUSE");
}


void ShowGslMatrix(gsl_matrix* mat,string matrix_name)
{
    return;
    int i,j;
    printf("matrix name: %s \n",matrix_name.c_str());

    for (i=0;i<mat->size1;i++)
    {
        string fmtstr;
        for (j=0;j<mat->size2;j++)
        {
            printf("%5.5f ",gsl_matrix_get(mat,i,j));
        }
        printf(" \n");
    }
    printf(" \n");
}

//function for convenience
double V(int i,int j,double k)
{
    if (fabs(k-g_knew) < 0.01)
    //if (k == g_knew)
        return gsl_matrix_get(g_Vmatnew,i,j);
    else
        return gsl_matrix_get(g_Vmatold,i,j);
}

//returns neighbouring points
void GetNeigbourIndices(gsl_vector* inArr,double x ,int& lowerX ,int& upperX )
{
    int N = inArr->size;
    if (x <= gsl_vector_get(inArr,0))
    {
        lowerX = 1;
        upperX = 1;
    }
    else if (x >= gsl_vector_get(inArr,N-1))
    {
        lowerX = N;
        upperX = N;
    }
    else
    {
        for (int i = 2; i<=N; i++)
        {
            if (x < gsl_vector_get(inArr,i-1))
            {
                lowerX = i - 1;
                upperX = i;
                break;
            }
            else if (x == gsl_vector_get(inArr,i-1))
            {
                lowerX = i;
                upperX = i;
                break;
            }
        }
    }
}

double GetBilinearInterpolation(gsl_vector* xAxis,gsl_vector* yAxis,
                                                                gsl_matrix* zSurface,double xcoord ,double ycoord)
{
    //first find 4 neighbouring points
    int nx = xAxis->size;
    int ny = yAxis->size;
    int lx; //index of x coordinate of adjacent grid point to left of P
    int ux; //index of x coordinate of adjacent grid point to right of P
    GetNeigbourIndices(xAxis, xcoord, lx, ux);
    int ly; //index of y coordinate of adjacent grid point below P
    int uy; //index of y coordinate of adjacent grid point above P
    GetNeigbourIndices( yAxis, ycoord, ly, uy);
    double fQ11 = gsl_matrix_get(zSurface,lx-1, ly-1);
    double fQ21 = gsl_matrix_get(zSurface,ux-1, ly-1);
    double fQ12 = gsl_matrix_get(zSurface,lx-1, uy-1);
    double fQ22 = gsl_matrix_get(zSurface,ux-1, uy-1);

    //if point exactly found on a node do not interpolate
    if ((lx == ux) && (ly == uy))
        return fQ11;
    double x = xcoord;
    double y = ycoord;
    double x1 = gsl_vector_get(xAxis,lx-1);
    double x2 = gsl_vector_get(xAxis,ux-1);
    double y1 = gsl_vector_get(yAxis,ly-1);
    double y2 = gsl_vector_get(yAxis,uy-1);

    //if xcoord lies exactly on an xAxis node do linear interpolation
    if (lx == ux)
        return fQ11 + (fQ12 - fQ11) * (y - y1) / (y2 - y1);
    //if ycoord lies exactly on an xAxis node do linear interpolation
    if (ly == uy)
        return fQ11 + (fQ22 - fQ11) * (x - x1) / (x2 - x1);

    double fxy = fQ11 * (x2 - x) * (y2 - y);
    fxy = fxy + fQ21 * (x - x1) * (y2 - y);
    fxy = fxy + fQ12 * (x2 - x) * (y - y1);
    fxy = fxy + fQ22 * (x - x1) * (y - y1);
    fxy = fxy / ((x2 - x1) * (y2 - y1));

    return fxy;
}


//int _tmain(int argc, _TCHAR* argv[])
int main()
{
    gsl_ieee_env_setup ();
    gsl_error_handler_t* old_handler = gsl_set_error_handler (&my_handler);
    //stores option values at current timestep
    g_Vmatnew = gsl_matrix_alloc(stocksteps+1,AvgSteps+1);
    //stores option values at previous timestep
    g_Vmatold = gsl_matrix_alloc(stocksteps+1,AvgSteps+1);
    //stores option values at previous iteration
    gsl_matrix* Vmatpreviter = gsl_matrix_alloc(stocksteps+1,AvgSteps+1);

    double S,A,payoff,savg;
    double Smin=0;
    double Smax=S0*2;
    double dS=(Smax-Smin)/stocksteps;
    cout << "dS=" << dS << endl;

    double Amin=0;
    double Amax=S0*2;
    double dA=(Amax-Amin)/AvgSteps;
    double dt=T/timesteps;
    cout << "dt=" << dt << endl;
    cout << "dA=" << dA << endl;

    //populate matrix Vmat with terminal condition values
    int i,j;
    double k=0.0;

    //S=Smin;
    A=Amin;
    for (j=0;j<=AvgSteps;j++)
    {
        for (i=0;i<=stocksteps;i++)
        {
            savg=A;
            payoff=max(savg-K,0.0);
            gsl_matrix_set(g_Vmatnew,i,j,payoff);
        }
        A=A+dA;
    }
    //ShowGslMatrix(g_Vmatnew,"g_Vmatnew");

    //advance timestep and loop on points excluding boundary
    double curt=T;
    for (k=0;k<timesteps;k++)
    {
        //***********  First Half Timestep *********************
        g_kold=k;
        g_knew=k+0.5;
        gsl_matrix_memcpy(g_Vmatold,g_Vmatnew);
        gsl_matrix_memcpy(Vmatpreviter,g_Vmatnew);
        //ShowGslMatrix(g_Vmatold,"g_Vmatold");
        curt=curt-dt*0.5;
        cout << "timstep k=" << k << " curt=" << curt << endl;
        S=Smin+dS;
        //set S explicit and A implicit
        for (i=1;i<=stocksteps-1;i++)
        {
            double errsum=1;
            int iter=0;
            //need to solve a set of equations to calculate
            //the unknown values which are on A axes
            while(errsum > tol)
            {
                iter++;
                A=Amin+dA;
                for (j=1;j<=AvgSteps-1;j++)
                {
                    //int k=0;
                    double rhs=0;
                    double denom=0;
                    double phi1=0;
                    double phi2=0;
                    if (S>A)
                        phi1=1;
                    else
                        phi2=1;
                    double tmpratio=1;
                    if (curt > 0) tmpratio=(S-A)/curt;

                    rhs = phi1*(tmpratio)*V(i,j+1,k+0.5)/dA
                        - phi2*(tmpratio)*V(i,j-1,k+0.5)/dA
                        + V(i,j,k)/(0.5*dt)
                        + 0.5*sigma*sigma*S*S * (V(i-1,j,k)-2*V(i,j,k)+V(i+1,j,k))/(dS*dS)
                        + r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
                        - 0.5*r*V(i,j,k);

                    denom = 1/(0.5*dt) + phi1*(tmpratio)/(dA) - phi2*(tmpratio)/(dA) + 0.5*r;
                    gsl_matrix_set(g_Vmatnew,i,j,rhs/denom);

                //ShowGslMatrix(g_Vmatold,"g_Vmatold");
                //ShowGslMatrix(g_Vmatnew,"g_Vmatnew");

                    A=A+dA;
                }
                //apply dirichlet boundary condition for A=0
                gsl_matrix_set(g_Vmatnew,i,0,0.0);
                //apply neumann boundary condition for A=inf
                //use option gamma=0 i.e., del2_V/del2_A=0
                gsl_matrix_set(g_Vmatnew,i,AvgSteps,
                    2*gsl_matrix_get(g_Vmatnew,i,AvgSteps-1)-gsl_matrix_get(g_Vmatnew,i,AvgSteps-2));
                errsum=0;
                //ShowGslMatrix(Vmatpreviter,"Vmatpreviter");
                //ShowGslMatrix(g_Vmatnew,"g_Vmatnew");
                for (j=0;j<=AvgSteps;j++)
                {
                    double nodeErr=gsl_matrix_get(Vmatpreviter,i,j)
                        -gsl_matrix_get(g_Vmatnew,i,j);
                    errsum=errsum + nodeErr*nodeErr;

                }
                for (j=0;j<=AvgSteps;j++)
                {
                    gsl_matrix_set(Vmatpreviter,i,j,gsl_matrix_get(g_Vmatnew,i,j));
                }
            }//continue SOR iterations until converge
            S=S+dS;
        }//loop over stock price nodes
        //ShowGslMatrix(g_Vmatold,"g_Vmatold");
        //ShowGslMatrix(g_Vmatnew,"g_Vmatnew");





        //***********  Next Half Timestep *********************
        g_kold=k+0.5;
        g_knew=k+1.0;
        gsl_matrix_memcpy(g_Vmatold,g_Vmatnew);
        gsl_matrix_memcpy(Vmatpreviter,g_Vmatnew);
        //ShowGslMatrix(g_Vmatold,"g_Vmatold");
        curt=curt-dt*0.5;
        cout << "timstep k=" << k << " curt=" << curt << endl;

        A=Amin+dA;
        //set S implicit and A explicit
        for (j=1;j<=AvgSteps-1;j++)
        {
            double errsum=1;
            int iter=0;
            //need to solve a set of equations to calculate
            //the unknown values which are on S axes
            while(errsum > tol)
            {
                iter++;
                S=Smin+dS;
                for (i=1;i<=stocksteps-1;i++)
                {
                    //int k=0;
                    double rhs=0;
                    double denom=0;
                    double phi1=0;
                    double phi2=0;
                    if (S>A)
                        phi1=1;
                    else
                        phi2=1;

                    double tmpratio=1;
                    if (curt > 0) tmpratio=(S-A)/curt;

                    rhs = 0.5*sigma*sigma*S*S * (V(i-1,j,k+1.0)+V(i+1,j,k+1.0))/(dS*dS)
                            + r*S*(V(i+1,j,k+1.0)-V(i-1,j,k+1.0))/(2*dS)
                            + V(i,j,k+0.5)/(0.5*dt)
                            + phi1*(tmpratio)*(V(i,j+1,k+0.5)-V(i,j,k+0.5))/dA
                            + phi2*(tmpratio)*(V(i,j,k+0.5)-V(i,j-1,k+0.5))/dA
                            - 0.5*r*V(i,j,k+0.5);

                    denom = 1/(0.5*dt) + sigma*sigma*S*S/(dS*dS) + 0.5*r;
                    gsl_matrix_set(g_Vmatnew,i,j,rhs/denom);
                    S=S+dS;
                }
                //apply dirichlet boundary condition for S=0
                gsl_matrix_set(g_Vmatnew,0,j,0.0);
                //apply neumann boundary condition for S=inf
                //use option gamma=0 i.e., del2_V/del2_S=0
                gsl_matrix_set(g_Vmatnew,stocksteps,j,
                    2*gsl_matrix_get(g_Vmatnew,stocksteps-1,j)-gsl_matrix_get(g_Vmatnew,stocksteps-2,j));
                errsum=0;
                //ShowGslMatrix(Vmatpreviter,"Vmatpreviter");
                //ShowGslMatrix(g_Vmatnew,"g_Vmatnew");
                for (i=0;i<=stocksteps;i++)
                {
                    double nodeErr=gsl_matrix_get(Vmatpreviter,i,j)
                        -gsl_matrix_get(g_Vmatnew,i,j);
                    errsum=errsum + nodeErr*nodeErr;

                }
                for (i=0;i<=stocksteps;i++)
                {
                    gsl_matrix_set(Vmatpreviter,i,j,gsl_matrix_get(g_Vmatnew,i,j));
                }
            }//continue SOR iterations until converge
            A=A+dA;

        }//loop over A nodes


        ShowGslMatrix(g_Vmatold,"g_Vmatold");
        ShowGslMatrix(g_Vmatnew,"g_Vmatnew");

    }//loop over time nodes

    ShowGslMatrix(g_Vmatnew,"g_Vmatnew");

    //interpolate

    gsl_matrix* resultsmat=gsl_matrix_alloc(g_Vmatnew->size1+1,g_Vmatnew->size2+1);
    gsl_matrix_set(resultsmat,0,0,0);
    gsl_vector* xAxis=gsl_vector_alloc(stocksteps);
    S=Smin;
    for (i=0;i<stocksteps;i++)
    {
        gsl_vector_set(xAxis,i,S);
        gsl_matrix_set(resultsmat,i+1,0,S);
        S=S+dS;
    }

    gsl_vector* yAxis=gsl_vector_alloc(AvgSteps);
    A=Amin;
    for (i=0;i<AvgSteps;i++)
    {
        gsl_vector_set(yAxis,i,A);
        gsl_matrix_set(resultsmat,0,i+1,A);
        A=A+dA;
    }

    for (i=0;i<stocksteps;i++)
        for (j=0;j<AvgSteps;j++)
            gsl_matrix_set(resultsmat,i+1,j+1,gsl_matrix_get(g_Vmatnew,i,j));
    //ShowGslMatrix(resultsmat,"resultsmat");
    double savg0=S0;
    double optionPrice=GetBilinearInterpolation(xAxis,yAxis,g_Vmatnew,S0,savg0);
    cout << "option price=" << optionPrice << endl;
    system("PAUSE");
    return EXIT_SUCCESS;
}



//*****  derivation of finite difference scheme  ***************

/*

****     S>A    *******


Step 1: subtitute finite difference formulas based on central difference scheme into the PDE
(V(i,j,k)-V(i,j,k-1))/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)-2*V(i,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*(V(i,j+1,k)-V(i,j,k))/(dA)
+ r*V(i,j,k) = 0


Step 2 : Group V(i,j,k) terms together
V(i,j,k)/dt
- 0.5*sigma*sigma*S*S * (-2*V(i,j,k))/(dS*dS)
+ r*V(i,j,k)
- ((S-A)/t)*(-V(i,j,k))/(dA)
-V(i,j,k-1)/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*(V(i,j+1,k))/(dA)
= 0


Step 2.1
V(i,j,k)/dt
+ sigma*sigma*S*S * V(i,j,k)/(dS*dS)
+ r*V(i,j,k)
+ ((S-A)/t)*V(i,j,k)/(dA)
-V(i,j,k-1)/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*V(i,j+1,k)/(dA)
= 0



V(i,j,k)[1/dt + sigma*sigma*S*S * (1/(dS*dS)) + r + ((S-A)/t)/(dA)]
= V(i,j,k-1)/dt
+ 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
+ r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
+ ((S-A)/t)*V(i,j+1,k)/(dA)




****     S<=A    *******


Step 1: subtitute finite difference formulas based on central difference scheme into the PDE
(V(i,j,k)-V(i,j,k-1))/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)-2*V(i,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*(V(i,j,k)-V(i,j-1,k))/(dA)
+ r*V(i,j,k) = 0


Step 2 : Group V(i,j,k) terms together
V(i,j,k)/dt
- 0.5*sigma*sigma*S*S * (-2*V(i,j,k))/(dS*dS)
+ r*V(i,j,k)
- ((S-A)/t)*(V(i,j,k))/(dA)
-V(i,j,k-1)/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*(-V(i,j-1,k))/(dA)
= 0


Step 2.1
V(i,j,k)/dt
+ sigma*sigma*S*S * V(i,j,k)/(dS*dS)
+ r*V(i,j,k)
- ((S-A)/t)*V(i,j,k)/(dA)
-V(i,j,k-1)/dt
- 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
- r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
+ ((S-A)/t)*V(i,j-1,k)/(dA)
= 0



V(i,j,k)[1/dt + sigma*sigma*S*S * (1/(dS*dS)) + r - ((S-A)/t)/(dA)]
= V(i,j,k-1)/dt
+ 0.5*sigma*sigma*S*S * (V(i-1,j,k)+V(i+1,j,k))/(dS*dS)
+ r*S*(V(i+1,j,k)-V(i-1,j,k))/(2*dS)
- ((S-A)/t)*V(i,j-1,k)/(dA)


*/
