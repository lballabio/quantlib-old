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

using namespace std;

#define LENGTH(a) (sizeof(a)/sizeof(a[0]))

//Calculates swaption price using Libor Market Model

//----------------------------- parameters
double tau = 0.5; //accrual period 
int g_alpha=4; //start time peg for swap
int g_beta=8; //end time peg for swap
double dt=0.05; //time grid step size
int N=20000; //No of MC simulations
//----------------------------------------------

int g_size;
gsl_rng *r_global;

double FlatCurve[]=
{
1.000000,
0.966736,
0.934579,
0.903492,
0.873439,
0.844385,
0.816298,
0.789145,
0.762895,
0.737519,
0.712986,
0.689270,
0.666342,
0.644177,
0.622750,
0.602035,
0.582009,
0.562649,
0.543934,
0.525841,
0.508349,
0.491440,
0.475093,
0.459290,
0.444012,
0.429243,
0.414964,
0.401161,
0.387817,
0.374917,
0.362446,
0.350390,
0.338735,
0.327467,
0.316574,
0.306044,
0.295864,
0.286022,
0.276508,
0.267311,
0.258419,
0.249823}; 

//double GBPCurve[]=
double P[]=
{
1.000000,
0.969514,
0.939441,
0.909913,
0.881024,
0.852807,
0.825482,
0.799100,
0.773438,
0.749042,
0.725408,
0.702527,
0.680361,
0.659402,
0.639171,
0.619580,
0.600668,
0.582455,
0.564873,
0.547888,
0.531492,
0.515651,
0.500360,
0.485543,
0.471240,
0.457861,
0.444977,
0.432554,
0.420575,
0.409019,
0.397888,
0.387341,
0.377196,
0.367435,
0.358056,
0.348978,
0.340292,
0.331614,
0.323265,
0.315460,
0.307945,
0.300321,
};

double volatility(double Tj,double T_o)
{
    double a = -0.05;
    double b = 0.5;
    double c = 1.5;
    double d = 0.15;
    return ((a + b * (Tj - T_o)) * std::exp(-c * (Tj - T_o)) + d);
}

double corr(double Tj,double Tk)
{
    double beta = 0.1;
    return std::exp(-beta * abs(Tj - Tk));
}


//Utility function to see matrices for debugging
void ShowGslMatrix(gsl_matrix* mat,string matrix_name)
{
    
    int i,j;
    string filename="C:\\Program Files\\GNU Octave 2.1.73\\octave_files\\"+matrix_name+".txt";
    FILE* outfile=fopen(filename.c_str(),"w");
    
    fprintf(outfile,"# Created by Octave 2.1.73, Sat Sep 23 12:59:49 2006 PST <dummy@dummy> \n");
    fprintf(outfile,"# name: %s \n",matrix_name.c_str());
    fprintf(outfile,"# type: matrix \n");
    fprintf(outfile,"# rows: %d \n",mat->size1);
    fprintf(outfile,"# columns: %d \n",mat->size2);
    
    for (i=0;i<mat->size1;i++)
    {
        string fmtstr;
        for (j=0;j<mat->size2-1;j++)
        {
            string tmp;
            fprintf(outfile,"%+10.4f ",gsl_matrix_get(mat,i,j));
            //fprintf(outfile,"% g",gsl_matrix_get(mat,i,j));
        }
        fprintf(outfile,"%10.4f \n",gsl_matrix_get(mat,i,mat->size2-1));
        //fprintf(outfile,"% g \r",gsl_matrix_get(mat,i,mat->size2-1));
    }  
    fclose(outfile);
}


//Returns swap rate
double GetSwapRate(gsl_vector* F,int alpha,int beta)
{
    int i,j;
    double tmp_sum=0;
    double SR=1;
    double tmp=1;
    for (j=alpha ; j< beta ;j++)
        tmp=tmp*(1/(1+tau*gsl_vector_get(F,j)));
    SR=1-tmp; 
    for (i=alpha ; i< beta;i++)
    {
        tmp=1;
        for (j=alpha;j<=i;j++)
            tmp=tmp*(1/(1+tau*gsl_vector_get(F,j)));	  
        tmp_sum=tmp_sum + (tau*tmp);
    }
    SR=SR/tmp_sum;
    return SR;
}


void DoMCSimulation()
{
    int i,j,k;
    double tmp;
    gsl_vector * F = gsl_vector_alloc (LENGTH(P));
    gsl_vector * curr_F = gsl_vector_alloc (LENGTH(P));
    gsl_vector * curr_logF = gsl_vector_alloc (LENGTH(P));
    gsl_vector * curr_logF_t = gsl_vector_alloc (LENGTH(P));    
    gsl_vector * finalFVec = gsl_vector_alloc (LENGTH(P));        
    gsl_vector * T = gsl_vector_alloc (LENGTH(P)-1);
    
    size_t maxidx= T->size;
    for (i=0; i < F->size ; i++)
        gsl_vector_set(F,i,(P[i]/P[i+1]-1)/tau);
    for (i=0; i < T->size ; i++)
        gsl_vector_set(T,i,i*tau);
   
    double SR=GetSwapRate(F,g_alpha,g_beta);
    
    gsl_matrix * corr_mat = gsl_matrix_alloc (g_size, g_size);
    
    for (i = 0; i < corr_mat->size1; i++)
    {
        for (j = 0; j < corr_mat->size2 ; j++)
        {
            gsl_matrix_set (corr_mat, i, j, 
                corr( gsl_vector_get(T,i), gsl_vector_get(T,j) )   );
        }
    }
    
    gsl_matrix * chol_mat = gsl_matrix_alloc (g_size, g_size);
    gsl_matrix_memcpy(chol_mat,corr_mat);
    gsl_linalg_cholesky_decomp(chol_mat);
    
    for (i = 1; i < chol_mat->size1; i++)
    {
        for (j = 0; j < i ; j++)
        {
            gsl_matrix_set(chol_mat,i,j,0);
        }
    }
    int timepoints=int((g_size*tau)/dt);
    gsl_matrix * rand_mat = gsl_matrix_alloc (1, g_size);
    gsl_matrix * mvar_rand_mat = gsl_matrix_alloc (1, g_size);
    gsl_matrix * libor_simulations = gsl_matrix_alloc (N,g_size);
    double rootDt=pow(dt,0.5);
    gsl_matrix* rand_nos_mat=gsl_matrix_alloc (timepoints, g_size);
    
    double payoff_sum=0;
    bool bAntiFlag=false;
    for (int scenario=0;scenario<N;scenario++)
    {
        for (i = 0; i < F->size; i++)
        {
            gsl_vector_set(curr_logF,i,log(gsl_vector_get(F,i)));
            gsl_vector_set(curr_logF_t,i,log(gsl_vector_get(F,i)));
        }
            
        double t=0;
        int counter=0;
        
        if (bAntiFlag == true)
            bAntiFlag=false;
        else
            bAntiFlag=true;
            
        while (t<(g_alpha*tau))
        {
            if (bAntiFlag == true)
            {
                for (j = 0; j < rand_mat->size2 ; j++)
                    gsl_matrix_set(rand_mat,0,j,gsl_ran_gaussian(r_global,1));
                double blas_alpha = 1., blas_beta = 0.;
                gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, blas_alpha, rand_mat, 
                    chol_mat, blas_beta, mvar_rand_mat);
            
                for (int tmpidx=0;tmpidx<mvar_rand_mat->size2;tmpidx++)
                      gsl_matrix_set(rand_nos_mat,counter,tmpidx,
                          gsl_matrix_get(mvar_rand_mat,0,tmpidx) );            
            }
            else
            {
                for (int tmpidx=0;tmpidx<rand_nos_mat->size2;tmpidx++)
                    gsl_matrix_set(mvar_rand_mat,0,tmpidx,
                          -gsl_matrix_get(rand_nos_mat,counter,tmpidx) );            
            }
            
            counter++;
            int nextResetIdx=int(floor(t/tau))+1;
            //evolve distribution for each libor
            for (k=nextResetIdx;k<g_beta;k++)
            {
                double drift_sum1=0;
                
                for (j=nextResetIdx;j<=k;j++)
                {
                    tmp = corr( gsl_vector_get(T,k),gsl_vector_get(T,j) )*
                        tau*volatility(gsl_vector_get(T,j),t)*
                        exp( gsl_vector_get(curr_logF_t,j) ) ;
                    tmp=tmp/( 1+tau*exp(gsl_vector_get(curr_logF_t,j)) );
                    drift_sum1+=tmp;
                }
                
                double dLogF=0;
                double vol_Tk_t=volatility(gsl_vector_get(T,k),t);
                //add drift part1
                //it will be removed 50% later on when we 
                //use predictor-corrector
                dLogF += vol_Tk_t*drift_sum1*dt;
                //add drift part2
                dLogF -= 0.5*vol_Tk_t*vol_Tk_t*dt;
                //add diffusion part
                dLogF += vol_Tk_t*gsl_matrix_get(mvar_rand_mat,0,k)*rootDt;
                
                //Apply Predictor-Corrector method to 
                //recalculate drift part and take average drift part
                double drift_sum2=0;
                for (j=nextResetIdx;j<=k;j++)
                {
                    //use new rate for discounting
                    tmp = corr( gsl_vector_get(T,k),gsl_vector_get(T,j) )*
                        tau*volatility(gsl_vector_get(T,j),t)*
                        exp( gsl_vector_get(curr_logF_t,j) + dLogF ) ;
                    tmp=tmp/( 1+tau*exp(gsl_vector_get(curr_logF_t,j)+dLogF) );
                    drift_sum2+=tmp;
                }
                //remove 50% of earlier drift part
                dLogF -= vol_Tk_t*drift_sum1*dt*0.5;
                //add 50% of newly calculated drift based on new rate
                dLogF += vol_Tk_t*drift_sum2*dt*0.5;
               
                gsl_vector_set(curr_logF,k,gsl_vector_get(curr_logF,k)+dLogF);
            }
       
            for (i = 0; i < F->size; i++)
                gsl_vector_set(curr_logF_t,i,gsl_vector_get(curr_logF,i));
            t=t+dt; 
        }
        for (i=0;i<g_size;i++)
            gsl_matrix_set(libor_simulations,scenario,i,
                exp(gsl_vector_get(curr_logF_t,i)) );
        for (i=0;i<g_size;i++)
            gsl_vector_set(finalFVec,i,exp(gsl_vector_get(curr_logF_t,i)));
        gsl_vector * DiscountCurve = gsl_vector_alloc (LENGTH(P));  
        gsl_vector_set(DiscountCurve,0,
            ( 1/ (1+tau*gsl_vector_get(finalFVec,0))) );
        
        for (i=1;i<g_size;i++)
            gsl_vector_set(DiscountCurve,i,
                (gsl_vector_get(DiscountCurve,i-1)/ 
                    (1+tau*gsl_vector_get(finalFVec,i))) );
        double payoff=0;
        for (i=g_alpha;i<g_beta;i++)
        {
           payoff=payoff+ 
               (SR- gsl_vector_get(finalFVec,i)) * tau*
                   gsl_vector_get(DiscountCurve,i);
        }     
        payoff_sum=payoff_sum+max(payoff,0.0);    
    }
    cout << "swaption price = " << 100*payoff_sum/N << endl;
    gsl_matrix_free(rand_mat);
    gsl_matrix_free(mvar_rand_mat);
}

void my_handler (const char * reason, 
              const char * file, 
              int line, 
              int gsl_errno)
{
    printf(reason);
    system("PAUSE");	
}
 
int main(int argc, char *argv[])
{
    g_size=g_beta;
    cout << "Swap start = " << g_alpha*0.5 << " year" << endl;
    cout << "Swap end = " << g_beta*0.5 << " year" << endl;
    gsl_ieee_env_setup ();
    gsl_error_handler_t* old_handler = gsl_set_error_handler (&my_handler); 
    gsl_rng_env_setup ();
    r_global = gsl_rng_alloc (gsl_rng_mt19937);
    gsl_rng_set(r_global, GetTickCount());
    DoMCSimulation();
    gsl_rng_free (r_global);
    system("PAUSE");	
    return 0;
}
