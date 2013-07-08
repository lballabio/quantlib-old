void spsenl(long M1){
/*This is the program for the finite different method to
  compute Gamme of Caplet. */
  
long M=(long) M1;
float nyear=5, delta=.25, epsilon=.25,B=1,delb=0.003;
int m=1, N=20;
float L0[N],lambda[N];
FILE *file_ptr;
int i,j,run,n,t,k,kd=10,mean;

double L[N],Lu[N],Lu0[N],Ld[N],Ld0[N],res[N][2],z[m],B0[N],K[N],Bu[N],Bd[N];
double v,vu,vd,discount,disu,disd,lam,sen,c,cu,cd,B1,B2,rr[N],cor[N];


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
           Bu[j]=B0[j];
           Bd[j]=B0[j];
           B=B0[j];
           K[j]=L0[j];
           res[j][0]=0;
           res[j][1]=0; 
           Lu0[j]=L0[j];
           Ld0[j]=L0[j];
       }      
      B=1/(1+delta*L0[0]);
     Bu[kd-1]=Bu[kd-1]+delb;
     Bd[kd-1]=Bd[kd-1]-delb;
     B1=Bu[0];
     B2=Bd[0];

     Lu0[kd-1]=(Bu[kd-2]/Bu[kd-1]-1)/delta;
     Ld0[kd-1]=(Bd[kd-2]/Bd[kd-1]-1)/delta;

     Lu0[kd]=(Bu[kd-1]/Bu[kd]-1)/delta;
     Ld0[kd]=(Bd[kd-1]/Bd[kd]-1)/delta;
      
      for (run=1;run<=M;run++)
      {
           for( j=1;j<N;j++) 
              { 
                 L[j]=L0[j];
                 Lu[j]=Lu0[j];
                 Ld[j]=Ld0[j];
               }    
           discount=1.;
           disu=1;
           disd=1;
           for( t=1;t<N;t++)
           {     nrandv(m,z);
                 for ( i=1; i<=m; i++)
                 {       
                      v=0;
                      vu=0;
                      vd=0;
                      for(n=t; n<N;n++)
                      {
                        lam=lambda[(n-t)*m+i-1];
                        v+=(delta*L[n]*lam)/(1+delta*L[n]);
                        vu+=(delta*Lu[n]*lam)/(1+delta*Lu[n]);
                        vd+=(delta*Ld[n]*lam)/(1+delta*Ld[n]); 
                        L[n]=L[n]*exp((-lam/2+v)*lam*epsilon
                                    +lam*sqrt(epsilon)*z[i-1]);
                        Lu[n]=Lu[n]*exp((-lam/2+vu)*lam*epsilon   
                                    +lam*sqrt(epsilon)*z[i-1]);
                        Ld[n]=Ld[n]*exp((-lam/2+vd)*lam*epsilon   
                                    +lam*sqrt(epsilon)*z[i-1]);
                      }
                  }
                  discount=discount*(1+delta*L[t]);
                  disu=disu*(1+delta*Lu[t]);
                  disd=disd*(1+delta*Ld[t]);
                  c=B*delta*max(L[t]-K[t],0)/discount;
                  cu=B1*delta*max(Lu[t]-K[t],0)/disu;
                  cd=B2*delta*max(Ld[t]-K[t],0)/disd;
                  sen=(cu+cd-2*c)/(delb*delb);
                  res[t][0]+=sen;
                  res[t][1]+=sen*sen;
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
                           
