void spsenl(long M1){
/* The program for Pathwise Exact method to compute Delta of Caplet */      


long M=(long) M1;         /* number of replication */
float nyear=5, delta=.25, epsilon=.25; /* time structure */
int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,k,run,n,t;

double L[N],Del[N][N],res[N][N+1],z[m],B0[N],K[N],B=1;
double v,discount,lam,sen,v1[N],v2;

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

/* Recover bond price at time 0 from inputted libor rates */
     for ( j=0; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           for(k=0;k<N+1;k++){ res[j][k]=0;}
       }      
      B=1/(1+delta*L0[0]); 


/*  Starting simulation  */
      for (run=1;run<=M;run++)
      {
           for( j=1;j<N;j++) 
              { 
                 L[j]=L0[j];
                 for (k=0;k<N;k++){ Del[j][k]=0;}
                 Del[j][j]=1;
               }    
           discount=1.;
           for( t=1;t<N;t++)
           {     nrandv(m,z);

/* Computing L_n and Del_nk  */
                 for ( i=1; i<=m; i++)
                 {       
                      v=0;
                      for(j=1;j<N;j++){v1[j]=0;}
                      for(n=t; n<N;n++)
                      {
                        lam=lambda[(n-t)*m+i-1];
                        v=v+(delta*L[n]*lam)/(1+delta*L[n]);
                        v2=exp((-lam/2+v)*lam*epsilon  
                             +lam*sqrt(epsilon)*z[i-1]);
                        for(j=1;j<n+1;j++)
                        {  v1[j]+=(delta*lam)/((1+delta*L[j])*
                                   (1+delta*L[j]))*Del[n][j];
                           Del[n][j]=Del[n][j]*v2
                                     +L[n]*v2*v1[j]*lam*epsilon;
                         }
                        L[n]=L[n]*v2;
                      }
                  }

/* Computing Delta of Caplet */

                  discount=discount*(1+delta*L[t]);
                  for(k=1;k<t+1;k++)  
                      { v=0;
                        for(n=k;n<t+1;n++)
                            { v+=delta*Del[n][k]/(1+delta*L[n]);}
                        sen=B*delta*ind(L[t],K[t])/discount*Del[t][k]-
                            B*delta*max(L[t]-K[t],0)/discount*v;
                        res[t][k]+=sen;
                        res[N-t][N+1-k]+=sen*sen;
                      }
            }
/* end of one replication */
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
                           
