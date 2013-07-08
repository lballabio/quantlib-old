void spl(long M1){
/* This is the progamm of pathwise approxiamtion method to compute
   Vega. Assuming Del_Lambda/Del_The=1.  */



long M=(long) M1;
float nyear=5, delta=.25, epsilon=.25,B=1;
int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,run,n,t,k;

double L[N],res[N][2],z[m],B0[N],K[N],sumz,dd;
double v,w,discount,lam,sen,c,Del_dft[N],Del[N];


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


     for ( j=0; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           B=B0[j];
           K[j]=L0[j];
           res[j][0]=0;
           res[j][1]=0; 
           Del_dft[j]=0;
           Del[j]=0;
       }      
      B=1/(1+delta*L0[0]);

      for( t=1;t<N;t++)  
      {  
        for ( i=1; i<=m; i++)
           {   w=0;  
               v=0;
               for(n=t; n<N;n++)
               {
                 lam=lambda[(n-t)*m+i-1];
                 v+=(delta*L0[n]*lam)/(1+delta*L0[n]);
                 w+=delta*L0[n]/(1+delta*L0[n]); 
                 Del_dft[n]+=(v+w*lam)*epsilon-epsilon*lam;
               }
           }
      }      
      
      for (run=1;run<=M;run++)
      {    sumz=0;
           dd=0;
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
                        v+=(delta*L[n]*lam)/(1+delta*L[n]);
                        L[n]=L[n]*exp((-lam/2+v)*lam*epsilon
                                    +lam*sqrt(epsilon)*z[i-1]);
                      }
                      sumz+=z[i-1];
                  }
                  discount=discount*(1+delta*L[t]);
                  Del[t]=L[t]*(Del_dft[t]+sqrt(epsilon)*sumz);
                  dd+=delta*Del[t]/(1+delta*L[t]);

                  v=0;
/*                  for(j=1;j<t+1;j++)
                      {v+=delta*Del[j]/(1+delta*L[j]);}
*/
                  sen=B*delta*ind(L[t],K[t])/discount*Del[t]-
                         B*delta*max(L[t]-K[t],0)/discount*dd;
                  res[t][0]+=sen;
                  res[t][1]+=sen*sen;
             }  
        }  

        for(i=1;i<N;i++) 
        {
          printf("%d        ", i);
          sen=res[i][0]/M;
          res[i][1]=sqrt(res[i][1]-sen*sen*M)/M;
          printf("%.10f     %.10f\n",sen,res[i][1]);
        }
}                        
                           
