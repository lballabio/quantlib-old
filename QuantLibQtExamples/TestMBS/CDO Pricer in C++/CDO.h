#ifndef _CDO__
#define _CDO__

 
#include "datecl.h"
#include <vector>
#include <map>
#include <iostream>
#include "MatrixUtil.h"

#define N 1
#define NUM_TRANCHES 3
#define NUM_REF 100
#define numSim 10000
#define MATURITY 5
#define ZC 0.05
#define ENTITY_NOTIONAL 10000
#define SPREAD 150

class Tranche
{
	 public:
		Tranche(double C, double D, string desc) : 
		  lowerAttachment(C*ENTITY_NOTIONAL*NUM_REF), 
		upperAttachment(D*ENTITY_NOTIONAL*NUM_REF), desc_(desc) {}

		vector<double> cash_Flow(int expiry, vector<double> defTime, 
								double rec, double capital,double C,	
								double D, int numReference)
		{
			vector<double> results;
			double PV_def=0;
			double PV_premium=0;
			// loss given default for each credit
			double* loss = new double[numReference];             		
			double tot_loss = 0;       // cumulative portfolio loss
			// stores the accumulated loss at each payment date
			double* periodic_loss = new double[numReference];	
			// outstanding tranche capital at each payment date
			double* out_capital = new double[numReference];					
			double* fee = new double[numReference];               
			double total_fee = 0;
			double indicator = 0;
			double disc_fact_def = 0;
			double absorbed_loss = 0;
			double r = 0.0;
			int c = 0;
			int j = 0;
			int num = N;

			// calculate the total loss in the k-th simulation
			for (int i = 0; i < numReference; i++)
			{
			// if the simulated default time for the generic credit 
			// is < than the CDO maturity, there is a loss
				if (defTime[i] < expiry)     		   
				{
					loss[i] = (1- rec)*capital;
					// sum the individual losses
					tot_loss = tot_loss + loss[i]; 		
				}
			}
			// DEFAULT LEG SIMULATION %%
			// if the loss is below the lower treshold C there's no  
			// default payment
			if (tot_loss < C)
					PV_def=0;  // if the loss is above C and below D 
								// there's a default payment
			else if ((tot_loss >= C) && (tot_loss < D))
			{
				for (int i = 0; i < numReference; i++)
				{
					if (defTime[i] < expiry)
					{
						// we memorize the cumulative losses
						indicator=indicator+loss[i]; 
						// tranche begins to absorb losses in excess of C
						if (indicator > C)           
						{
							if (c == 0)
							{
								disc_fact_def=0;
								r = ZC;
								// discount factor at default 
								disc_fact_def= exp(-defTime[i]*r);	
								// only the loss exceeding C is       
								// absorbed (look at the footnote 
								// 32 in the simulation algorithm
								// p.51 of my paper)
								PV_def=PV_def+(indicator-C)*
									disc_fact_def;						
								c = 1;
							}
							else
							{
								disc_fact_def=0;
								r= ZC;
								disc_fact_def= exp(-defTime[i]*r);
								PV_def=PV_def+loss[i]*disc_fact_def;
							}
						}
					}
				}
			}
			// if portfolio losses are exceeding D, the C-D% tranche  
			// only absorb losses up to D
			else if (tot_loss > D)  
			{ 
				for (int i = 0; i < numReference; i++)
				{
					if (defTime[i] < expiry)
					{
						indicator= indicator + loss[i];
						// check if losses are in the C-D% range
						if ((indicator > C) && (indicator < D))  				   		  
						{	
							if (c == 0)
							{
								r= ZC;
								disc_fact_def = exp(-defTime[i]*r);
								PV_def=PV_def+(indicator-C)*disc_fact_def;
								c=1;	  
							}
							else
							{
								r= ZC;
								disc_fact_def= exp(-defTime[i]*r);
								PV_def=PV_def+loss[i]*disc_fact_def;
							}
						}
						// look at footnote 33 p. 52 of my paper
						else if (indicator > D)   
						{ 
							if (c == 1)
							{
								r= ZC;  
								disc_fact_def= exp(-defTime[i]*r);
								// look at footnote 33 p. 52 
								// of my paper
								absorbed_loss = D - 
								(indicator-loss[i]);						   
								PV_def=PV_def+(absorbed_loss*
								disc_fact_def);	
								c=2;
							}
						}
					}
				}
			}
			results.push_back(PV_def);


			// PREMIUM LEG SIMULATION 
	
			for (int i= 0; i < expiry; i++)
			{
				periodic_loss[i]=0;
				for (int j =  0; j < numReference; j++)
				{
				if (defTime[j] < i)
						// calculated the accumulated portfolio losses		
					periodic_loss[i] = periodic_loss[i]+(1-rec)*capital;  
				}
				// outstanding capital at each payment date
				out_capital[i] = min(max(D-periodic_loss[i],0),D-C);					
				fee[i]= exp(-ZC*i)*out_capital[i]; 
				PV_premium=PV_premium+fee[i]; // DV01
			}

			results.push_back(PV_premium);
			results.push_back(indicator/(ENTITY_NOTIONAL*NUM_REF));
   
			return results;	
		}

		void setExpectedLoss(double loss) { expectedLoss_ = loss; }
		double getExpectedLoss() const { return expectedLoss_; }
		double getUpperAttachment() { return upperAttachment; }
		double getLowerAttachment() { return lowerAttachment; }
		void setSpread(double spread) { spread_ = spread; }
		double getSpread() const { return spread_; }
		void setDesc(string desc) { desc_ = desc; }
		string getDesc() const { return desc_; }

private:
		double expectedLoss_;
		double upperAttachment;
		double lowerAttachment;
		double recovery;
		double spread_;
		string desc_;
		double totalLoss;     // cumulative portfolio loss
		double out_capital;   // outstanding tranche capital at each 
							  // payment date
		double periodic_loss; // stores the accumulated loss at each 
							  // payment date
};

class CDO
{
	public:
		CDO() : T(MATURITY), numReference_(NUM_REF) { }
		double priceTranche(int numReference)
		{
			MatrixUtil mu;
			double C = 0.0;
			double D = 0.0;
			double spread = (double) SPREAD/10000;
			Array2D<double> R1(numReference,numReference);
			int size = R1.dim1();
			std::cout << "pool size = " << size << endl;
			vector<double> defaultTime;
			vector<double> normaldev;
			vector<double>::iterator iter;
			vector<double> val;
			map<double,int> expMap;

			double y = 0;
			double tau = 0.0;
			double fees = 0.0;
			double loss = 0.0;
			double PV_default = 0.0;
			double* dev = new double[numReference];
			int rec_cycle = 0;

			Array1D<double> hazard(N);
			Array1D<double> R(N);
			Array1D<double> expectLoss(N);
			Array1D<double> recovery(N);
			Array2D<double> corr(numReference,numReference);
			Array2D<double> S_fees(N,NUM_TRANCHES);
			Array2D<double> S_default(N,NUM_TRANCHES);
			Array2D<double> M_fees(N,NUM_TRANCHES);
			Array2D<double> M_default(N,NUM_TRANCHES);
			Array2D<double> totLoss(N,NUM_TRANCHES);
			Array2D<double> expLoss(N,NUM_TRANCHES);
			//std::cout.precision(5);
			
			for (int j = 0; j < N; j++)
			{
				// start the loop for each of the three tranches	
				for (int u = 0; u < NUM_TRANCHES; u++) 
				{
					S_fees[j][u] = 0;
					S_default[j][u] = 0;
					M_fees[j][u] = 0;
					M_default[j][u] = 0;
					totLoss[j][u] = 0;
				}
				hazard[j] = 0.03;   // hazard rate
				recovery[j] = 0.40; // recovery rate
			}
			
			for (int R_cycle = 0; R_cycle < N; R_cycle++)
			{
				for (int xx = 0; xx < numReference; xx++)
				{
					for (int yy = 0; yy < numReference; yy++)
					{
						if (xx == yy)
							corr[xx][yy] = 1.0;
						else
						{
							corr[xx][yy] = 0.2*(R_cycle+1); 
								// populate the correlation 
								// matrix
							corr[yy][xx] = 0.2*(R_cycle+1);
							}
							R1[xx][yy] = corr[xx][yy];
					}
			 
				}
		
				for (int i = 0; i < numSim; i++)
				{	
					defaultTime.clear();
					defaultTime.empty();
					defaultTime = mu.genCholesky4(R1);
					
					for (int j = 0; j < N; j++)
					{
						int l = 0;
						for (iter = defaultTime.begin(); iter != defaultTime.end(); iter++)
						{
							defaultTime[l] = (double) *iter/hazard[j];					
							l++;
						}
						// tau = *iter/hazard[expMap[*iter]];
						// std::cout << "tau = " << tau << endl;
						// generate the vector of default time by 
						// dividing by the corresponding 
						// hazard rate
				 
						sort(defaultTime.begin(),
							defaultTime.end());
						// start the loop for each of the three 
						// tranches
						for (int u= 0; u < NUM_TRANCHES; u++) 
						{
							// calculate the premium and default leg 
							C = tranche[u].getLowerAttachment();
							D = tranche[u].getUpperAttachment();
							val= tranche[u].cash_Flow(MATURITY,
								defaultTime,recovery[j],ENTITY_NOTIONAL,
								C,D,numReference);
							iter = val.begin();
							PV_default = *iter;
							iter++;
							fees = *iter;
							iter++;
							loss = *iter;
							totLoss[j][u] = totLoss[j][u] + loss;
							S_fees[j][u] = S_fees[j][u] + fees;
							S_default[j][u] = S_default[j][u] + PV_default;
						}
					}
					defaultTime.empty();
					defaultTime.clear();
				}
				
				for (rec_cycle = 0; rec_cycle < N; rec_cycle++)
				{
					for (int u = 0; u < NUM_TRANCHES; u++)
					{
						// average DV01					
						M_fees[rec_cycle][u]=S_fees[rec_cycle][u]/numSim;     
						// average default leg			
						M_default[rec_cycle][u]=S_default[rec_cycle][u]/numSim;
						expLoss[rec_cycle][u] = totLoss[rec_cycle][u]/numSim;
					}
				}
			 
				for (rec_cycle = 0; rec_cycle < N; rec_cycle++)
				{
					for (int u = 0; u < NUM_TRANCHES; u++)
					{
						spread= ((M_default[rec_cycle][u])/(M_fees[rec_cycle][u]))*10000; 
						// B\E spread for the 0-3% tranche
						loss = expLoss[rec_cycle][u];
						tranche[u].setSpread(spread);
						tranche[u].setExpectedLoss(loss);
						std::cout << tranche[u].getDesc() << " " << tranche[u].getSpread() << " " << tranche[u].getExpectedLoss() << endl;
					}
				}
			}
			delete [] dev;
			return 0.0;
		}
		void addTranche(Tranche t) { tranche.push_back(t); }
		int getNumTranches() { return numTranches_; }
		int getNumReferences() { return numReference_; }
		virtual ~CDO() { }
	private:
		//Date maturity;
		vector<Tranche> tranche;
		int T;
		int numReference_;
		int numTranches_;
};

#endif
















