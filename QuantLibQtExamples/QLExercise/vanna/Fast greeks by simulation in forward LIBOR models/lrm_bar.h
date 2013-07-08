void spsenl(long M1){
/*This is the program for the Likelihood rate method to
  compute Delta of Barriar (knockout) option. */
  
long M=(long) M1;                             /* number of replication */
float nyear=5, delta=.25, epsilon=.25,B=1;           /* time structure */
int m=1, N=20;
float L0[N],lambda[N*m];
FILE *file_ptr;
int i,j,k,t,run,n,a=0,ind;
double p,L[N],Ln[N],res[N+1][N+2],z[m],B0[N],K[N],Bari[N];
double v,v1,discount,sen,lam,zz[N],b[N],Dd[N][N],am1[N][N];

/* 
     Input intial term structure of Libor rates: L0[n];
     and volatility strcutre lambda[n].
*/ 


   file_ptr=fopen("L0","rt");
   if(file_ptr==NULL)
   {
     printf("File not found");
   }
   for(i=0;i<N;i++)
   {
        fscanf(file_ptr,"%f", &(L0[i]));
    }
   fclose(file_ptr);
      file_ptr=fopen("lambda","rt");
   if(file_ptr==NULL)
   {
     printf("File not found");
   }
   for(i=0;i<(N*m);i++)
   {
        fscanf(file_ptr,"%f", &(lambda[i]));
   }
   fclose(file_ptr);

/* Recover bond price, B0[n], at time 0 from inputted libor rates; 
   and compute the inverse of the matrix Lambda. */

    j=0;
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           for( k=0;k<N+1;k++){ res[j][k]=0;}

    b[0]=1/lambda[0];
    for ( j=1; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           Bari[j]=1.2*L0[j];
           for( k=0;k<N+2;k++){ res[j][k]=0;}
           v=0;
           for(k=0;k<j;k++){ v+=b[k]*lambda[j-k];}
           b[k]=-v/lambda[0];
       }
     B=1/(1+delta*L0[0]); 

      for (i=1;i<N;i++) 
              {   for( j=0; j<N;j++)
                      { Dd[i][j]=0;}
                  Dd[i][i-1]=1/delta/B0[i]/L0[i];
                  Dd[i][i]=-B0[i-1]/delta/B0[i]/B0[i]/L0[i];
              }

     for(t=1;t<N;t++)
        {   for (i=t;i<N;i++)  
              { 
                 for( j=t; j<i+1;j++)
                   { sen=epsilon*delta*lambda[(i-t)]
                          *lambda[(j-t)]/(1+delta*L0[j])/(1+delta*L0[j]);
                     Dd[i][j-1]+=sen/delta/B0[j];
                     Dd[i][j]+=-sen*B0[j-1]/delta/B0[j]/B0[j];
                   }
               }
            for (k=0;k<t+1;k++)
               {     am1[t][k]=0;
                     for(j=1;j<t+1;j++)
                         {  am1[t][k]+=b[j-1]*Dd[t+1-j][k];}
               }

       }

/*  Starting simulation  */
     for ( run=1;run<=M;run++)
       {   ind=0;
            for( j=1;j<N;j++)
            {
                L[j]=L0[j];
            }    
            discount=1.;
            for( t=1;t<N;t++)
            {     nrandv(m,z);
                  for ( i=1; i<=m; i++)
                  {   if (ind<N) { zz[ind]=z[i-1];
                                   ind+=1;}     
                      v=0;
                      for(n=t; n<N;n++)
                      {
                          lam=lambda[(n-t)*m+i-1];
                          v=v+(delta*L[n]*lam)/(1+delta*L[n]);
                          L[n]=L[n]*exp((-lam/2+v)*lam*epsilon
                                    +lam*sqrt(epsilon)*z[i-1]);
                      } 

                  }

                 if ( L[t]>Bari[t] ) {t=N;}
                   else { 
                     discount=discount*(1+delta*L[t]);
                     for(i=0;i<t+1;i++)  
                     {    v=0;
                         for(j=1;j<t+1;j++)
                            { 
                               v+=zz[j-1]*am1[j][i];
                            }
                        sen=B*delta*max(L[t]-K[t],0)/discount*
                               v/sqrt(epsilon);
                      res[t][i]+=sen;
                      res[N-t][N+1-i]+=sen*sen;
                      }
                  }  
            }

        }


        for(i=1;i<N;i++)
        {
          printf("%d        ", i);
          for(j=0;j<i+1;j++)
             {  sen=res[i][j]/M;
                res[N-i][N+1-j]=sqrt(res[N-i][N+1-j]-sen*sen*M)/M;
                printf("%f  ",sen);
             }
          printf("\n         ");
          for(j=0;j<i+1;j++)
             { printf("%f  ",res[N-i][N+1-j]);}
          printf("\n \n");
          }        

}                        
                           
