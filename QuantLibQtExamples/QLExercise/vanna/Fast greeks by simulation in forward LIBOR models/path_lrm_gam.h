void spgam(long M1){
/*This is the program for the Mixed Pathwise and LRM  method to
  compute Gamme of Caplet. */


long M=(long) M1;       /* number of replication */
float nyear=5, delta=.25, epsilon=.25,B=1;  /* time structure */
int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,k,run,n,t,ind;

double L[N],res[N][N+3],z[m],B0[N],K[N],Del1[N][N],Del[N][N],Dd[N][N];
double v,w,discount,lam,sen,Dell[N][N],zz[N],b[N],v2,Ddd[N][N],p;
double am[N][N],vz[N];
double con[N];

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


    j=0;
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           for( k=0;k<N+1;k++){ res[j][k]=0;
                                Del1[j][k]=0;
                                Dd[j][k]=0;}
           res[j][N+1]=0;
           res[j][N+2]=0;
    b[0]=1/lambda[0];
    for ( j=1; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           for( k=0;k<N+1;k++){ res[j][k]=0;
                                Del1[j][k]=0;
                                Dd[j][k]=0;}
           res[j][N+1]=0;
           res[j][N+2]=0;
           v=0;
           for(k=0;k<j;k++){ v+=b[k]*lambda[j-k];}
           b[k]=-v/lambda[0];
           con[j]=0;
       }
     B=1/(1+delta*L0[0]); 
    con[0]=-1;
    con[1]=+1; 


        for (i=0;i<N;i++) 
              {   for( j=0; j<N;j++)
                      { Dell[i][j]=0;}
                   Dell[i][i-1]=1/delta/B0[i]/L0[i];
                   Dell[i][i]=-B0[i-1]/delta/B0[i]/B0[i]/L0[i];
              }
       

// this part need adjusment for m not equil 1!


     for(t=1;t<N;t++)
        {   for (i=t;i<N;i++)
              {
                 for( j=t; j<i+1;j++)
                   { sen=epsilon*delta*lambda[(i-t)]
                          *lambda[(j-t)]/(1+delta*L0[j])/(1+delta*L0[j]);
                     Dell[i][j-1]+=sen/delta/B0[j];
                     Dell[i][j]+=-sen*B0[j-1]/delta/B0[j]/B0[j];
                   }
               }
            for (k=0;k<t+1;k++)
               {     am[t][k]=0;
                     for(j=1;j<t+1;j++)
                         {  am[t][k]+=b[j-1]*Dell[t+1-j][k];}
               }
                 
       }


    for( t=1;t<N;t++)
      {  for (i=1;i<=m;i++)
          {   
              for (n=t;n<N;n++)
                  {   for(j=t;j<=n;j++)
                      {  v=delta*lambda[(n-t)*m+i-1]*lambda[(j-t)*m+i-1]
                                 /(1+delta*L0[j])/(1+delta*L0[j]);
                         Del1[n][j-1]+=v/delta/B0[j];
                         Del1[n][j]+=-v/delta*B0[j-1]/B0[j]/B0[j];
                         sen=-2*delta*v/(1+delta*L0[j]);
                         Dd[n][j-1]+=sen/delta/B0[j]/delta/B0[j];
                         Dd[n][j]+=sen*B0[j-1]/delta/B0[j]/B0[j]*B0[j-1]
                          /delta/B0[j]/B0[j]+v*B0[j-1]*2/delta/
                          B0[j]/B0[j]/B0[j];
                      } 
                  }
            }  
       }

/* Starting simulation */
      for (run=1;run<=M;run++)
      {    ind=0;
           for( j=1;j<N;j++) 
              { 
                 L[j]=L0[j];
               }    
           discount=1.;
           for( t=1;t<N;t++)
           {     nrandv(m,z);
                 for ( i=1; i<=m; i++)
                 {    if (ind<N) { zz[ind]=z[i-1];
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
                  discount=discount*(1+delta*L[t]);
                  for (k=0;k<t+1;k++)
                        {Del[t][k]=L[t]*epsilon*Del1[t][k];
                         Ddd[t][k]=L[t]*epsilon*Dd[t][k];
                         vz[k]=0;
                         for(j=1;j<t+1;j++)
                              {
                                vz[k]+=zz[j-1]*am[j][k];
                              }
                        }
                  Del[t][t-1]+=L[t]/L0[t]/delta/B0[t];
                  Del[t][t]+=-L[t]/L0[t]*B0[t-1]/delta/B0[t]/B0[t];
                  Ddd[t][t-1]+=-L[t]/delta/B0[t]/L0[t]/delta/
                                  L0[t]/B0[t];
                  Ddd[t][t]+=L[t]*(2*B0[t-1]/delta/B0[t]/B0[t]/L0[t]
                     /B0[t]-B0[t-1]*B0[t-1]/delta/delta/L0[t]/
                     L0[t]/B0[t]/B0[t]/B0[t]/B0[t]);

                  k=0;
                  v=0;
                  w=0;
                  for(n=k;n<t+1;n++)
                      {  v+=delta*Del[n][k]/(1+delta*L[n]);
                         w+=delta*Ddd[n][k]/(1+delta*L[n]); }
                  sen=B*delta/discount*(ind(L[t],K[t])*Del[t][k]-
                    max(L[t]-K[t],0)*v)+delta/discount*max(L[t]-K[t],0);
                  p=v;
                  sen=sen*vz[k]/sqrt(epsilon)+B*delta/discount*
                       (ind(L[t],K[t])*Ddd[t][k]-max(L[t]-K[t],0)*w)
                      +2*delta/discount*(ind(L[t],K[t])*Del[t][k]-
                    max(L[t]-K[t],0)*p)-con[t-k]*vz[k];

                  res[t][k]+=sen;
                  res[N-t][N+2-k]+=sen*sen;

                  for(k=1;k<t+1;k++)  
                      { 
                        v=0;
                        w=0;
                        v2=0;
                        for(n=k;n<t+1;n++)
                            {  v+=delta*Del[n][k]/(1+delta*L[n]);
                               w+=delta*Ddd[n][k]/(1+delta*L[n]); 
                            }
                        sen=B*delta/discount*(ind(L[t],K[t])*Del[t][k]-
                            max(L[t]-K[t],0)*v);
                        sen=sen*vz[k]/sqrt(epsilon)+B*delta/discount*
                          (ind(L[t],K[t])*Ddd[t][k]-max(L[t]-K[t],0)*w)
                           -con[t-k]*vz[k];

                        res[t][k]+=sen;
                        res[N-t][N+2-k]+=sen*sen;

                      }
  
            }
        }  

        for(i=1;i<N;i++) 
        {
         printf("%d        ", i);
          for(j=0;j<i+1;j++)
             {  sen=res[i][j]/M;
                res[N-i][N+2-j]=sqrt(res[N-i][N+2-j]-sen*sen*M)/M;
                printf("%f  ",sen);
             }
          printf("\n         ");
          for(j=0;j<i+1;j++)
             { printf("%f   ",res[N-i][N+2-j]);}
          printf("\n \n");
         }

}                        
                           
