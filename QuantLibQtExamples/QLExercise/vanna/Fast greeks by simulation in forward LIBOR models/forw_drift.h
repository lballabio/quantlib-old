void spsenl(long M1){
/* The program for Forward Drift method to compute Delta of Caplet */

long M=(long) M1;                         /* number of replication */
float nyear=5, delta=.25, epsilon=.25,B=1;       /* time structure */
int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,k,run,n,t;

double L[N],res[N][N+1],z[m],B0[N],K[N],Del[N][N],Dell[N][N];
double v,discount,lam,sen;

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

/* Recover bond price at time 0, B0[n], from inputted libor rates */
     for ( j=0; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           for(k=0;k<N+1;k++){ res[j][k]=0;}
       }      
      B=1/(1+delta*L0[0]); 
      
     for (i=1;i<N;i++)
        {  for(j=1;j<=i;j++)
               { Dell[i][j]=delta*lambda[i-1]*lambda[j-1]
                        /(1+delta*L0[j])/(1+delta*L0[j]);
               }
        }

/*  Starting simulation  */
      for (run=1;run<=M;run++)
      {
           for( j=1;j<N;j++) 
              { 
                 L[j]=L0[j];
               }    
           discount=1.;
           for( t=1;t<N;t++)
           {     nrandv(m,z);
                 for ( i=1; i<=m; i++)
                 {       
                      v=0;
                      for(n=t; n<N;n++)
                      {
                        lam=lambda[(n-t)*m+i-1];
                        v=v+(delta*L[n]*lam)/(1+delta*L[n]);
                        L[n]=L[n]*exp((-lam/2+v)*lam*epsilon
                                    +lam*sqrt(epsilon)*z[i-1]);
                      }
                  }
/* Computing Delta of Caplet */                  
                  discount=discount*(1+delta*L[t]);
                  for(k=1;k<t;k++)  
                      { Del[t][k]=L[t]*Dell[t][k]*t*m*epsilon;
                        v=0;
                        for(n=k;n<t+1;n++)
                            { v+=delta*Del[n][k]/(1+delta*L[n]);}
                        sen=B*delta*ind(L[t],K[t])/discount*Del[t][k]-
                            B*delta*max(L[t]-K[t],0)/discount*v;
                        res[t][k]+=sen;
                        res[N-t][N+1-k]+=sen*sen;
                      }
                  Del[t][t]=L[t]/L0[t]+L[t]*Dell[t][t]*t*m*epsilon;
                  v=delta*Del[t][t]/(1+delta*L[t]);
                  sen=B*delta*ind(L[t],K[t])/discount*Del[t][k]-
                       B*delta*max(L[t]-K[t],0)/discount*v;
                 res[t][t]+=sen;
                 res[N-t][N+1-t]+=sen*sen;
            }
        }  

        for(i=1;i<N;i++) 
        {
          printf("%d        ", i);
          for(j=1;j<i+1;j++)
             {  sen=res[i][j]/M;
                res[N-i][N+1-j]=sqrt(res[N-i][N+1-j]-sen*sen*M)/M;
                printf("%f  ",sen);
             }
          printf("\n         ");
          for(j=1;j<i+1;j++)
             { printf("%f   ",res[N-i][N+1-j]);}
          printf("\n \n");
         }

}                        
                           
