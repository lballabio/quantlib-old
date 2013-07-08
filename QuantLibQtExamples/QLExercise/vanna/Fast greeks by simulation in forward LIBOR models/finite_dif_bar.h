void spsenl(long M1){
/*This is the program for the finite different method to
  compute Delta of barriar (knockout) option. */
  
  
long M=(long) M1;                  /* number of replication */
float nyear=5, delta=.25, epsilon=.25,B=1;/* time structure */

float delb=0.0002;       /* initial setup of incerement of B0  */
int kd=10;      /* Fixed the number of initial bond (or libor rates) we
                   want to compute the derivative with respect to.   */

int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,run,n,t,k,flag,flagu;

double L[N],Lu[N],Lu0[N],res[N][2],z[m],B0[N],K[N],Bu[N];
double v,vu,discount,disu,lam,sen,c,cu,B1;
double Bari[N];

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
   set barriar Bari[n].    */    
     for ( j=0; j<N; j++)
      {
           B0[j]=B/(1+delta*L0[j]);
           Bu[j]=B0[j];
           B=B0[j];
           K[j]=L0[j];
           res[j][0]=0;
           res[j][1]=0; 
           Lu0[j]=L0[j];
           Bari[j]=1.2*L0[j];
       }      
      B=1/(1+delta*L0[0]);
     Bu[kd-1]=Bu[kd-1]+delb;
     B1=Bu[0];

     Lu0[kd-1]=(Bu[kd-2]/Bu[kd-1]-1)/delta;
     Lu0[kd]=(Bu[kd-1]/Bu[kd]-1)/delta;

/*  Starting simulation  */
      for (run=1;run<=M;run++)
      {    flag=0;
           flagu=0;
           for( j=1;j<N;j++) 
              { 
                 L[j]=L0[j];
                 Lu[j]=Lu0[j];
               }    
           discount=1.;
           disu=1;
           for( t=1;t<N;t++)
           {     nrandv(m,z);
                 for ( i=1; i<=m; i++)
                 {       
                      v=0;
                      vu=0;
                      for(n=t; n<N;n++)
                      {
                        lam=lambda[(n-t)*m+i-1];
                        v+=(delta*L[n]*lam)/(1+delta*L[n]);
                        vu+=(delta*Lu[n]*lam)/(1+delta*Lu[n]);
                        L[n]=L[n]*exp((-lam/2+v)*lam*epsilon
                                    +lam*sqrt(epsilon)*z[i-1]);
                        Lu[n]=Lu[n]*exp((-lam/2+vu)*lam*epsilon   
                                    +lam*sqrt(epsilon)*z[i-1]);
                      }
                  }
                  discount=discount*(1+delta*L[t]);
                  disu=disu*(1+delta*Lu[t]);
                  if ( L[t]>Bari[t] ) {flag=1;}
                  if (flag==1){ c=0;}
                  else {c=B*delta*max(L[t]-K[t],0)/discount;}
                  if ( Lu[t]>Bari[t] ) {flagu=1;}
                  if (flagu==1){ cu=0;}
                  else {cu=B1*delta*max(Lu[t]-K[t],0)/disu;}
                 
                  sen=(cu-c)/delb;               
                  res[t][0]+=sen;
                  res[t][1]+=sen*sen;
                 
                  if ( (flag==1) && (flagu==1) ) {t=N;}
             }  
        }  



      for(i=1;i<N;i++) 
        {
          printf("%d        ", i);
          sen=res[i][0]/M;
          res[i][1]=sqrt(res[i][1]-sen*sen*M)/M;
          printf("%.8f     %.8f\n",sen,res[i][1]);
        }

}                        
                           
