#include "Basket.h"
#include <map>
#define NUM 5
#define step 0.25

void Basket::bootstrap(vector<double> price, vector<double> coupon, vector<double> period)
{
	copy(price.begin(),bondPrice.begin(),bondPrice.end());
	copy(coupon.begin(),coupons.begin(),coupons.end());
	copy(period.begin(),period.begin(),period.end());

	spotRates[0] = (((coupons[0] + faceValue)/bondPrice[0])-1)*2;
	discountSum[0] = pow(1/(1 + spotRates[0]),period[0]);

	for (int i = 1; i <= bondPrice.size(); i++)
	{
		spotRates[i] = 2*(pow((coupons[i] + faceValue)/(bondPrice[i] - coupons[i]*discountSum[i-1]),(1/period[i]))-1);	
		discountSum[i] = discountSum[i-1] + pow(1/(1 + spotRates[i]),period[i]);
	}
}

/*
SymmetricMatrix Basket::generateCorrelationMatrix(SymmetricMatrix& C)
{
	return Cholesky(C);
}
*/

void Basket::setkthtoDefault(int kthDef) {
	kthToDefault = kthDef;
}
void Basket::setTradeDate(Date d1) {
	tradeDate = d1;
}

void Basket::setEffectiveDate(Date d1) {
	effectiveDate = d1;
}

void Basket::setMaturityDate(Date d1) {
	maturityDate = d1;
}

double Basket::priceBasket()
{
	int i, j, k;
	MRNG mrng;  // Mersenne generator
	MatrixUtil mu;
	//double* dev = new double[NUM];
	double* z = new double[NUM];
	double aveInflow = 0.0;
	double aveOutflow = 0.0;
	vector<double> defaultTime;
	vector<double> normaldev;
	TNT::Array1D<double> dev(NUM);
	double minTime = 0.0;
	double basketPrice = 0.0;
	double sum, outflow = 0.0;
	double inflow = 0.0;
	int m = exposure.size();  // number of exposure in the basket
	int def = 0;
	double total_protection_leg = 0.0;
	double total_fixed_leg = 0.0;
	Array2D<double> R(NUM,NUM);
	map<double,int> expMap;
	std::cout.precision();

	for (int i = 0; i < NUM; i++)
	{
		for (int j = 0; j < NUM; j++)
		{
			if (i == j)
				R[i][j] = 1;
			//else
			//{
			//	R[i][j] = R[j][i] = 0.62;
			//}
		}
	}
	
 
	R[0][1] = R[1][0] = 0.44180;
	R[0][2] = R[2][0] = 0.90208;
	R[0][3] = R[3][0] = 0.83975;
	R[1][2] = R[2][1] = 0.67615;
	R[1][3] = R[3][1] = 0.68552;
	R[2][3] = R[3][2] = 0.84178;

	for (int i = 1; i <= NUM_SIM; i++)
	{
	    sum = 0.0;
		normaldev.empty();
		defaultTime.empty();
		normaldev.clear();
		defaultTime.clear();

		// for each name in the basket
		for (int j = 0; j < m; j++)
		{
			dev = mu.genCholesky(R);
			//dev = mu.genCorrelatedDeviates(R1);
			//dev = mu.genEigenValue(R);
			normaldev.push_back(mu.normalCalc(dev[j]));
			double y = calcDefaultTime(-log(normaldev[j]),j);
			defaultTime.push_back(y);
			expMap[defaultTime[j]] = j;
		}

		vector<double>::iterator iter;
		vector<double>::iterator defIter;
		
		sort(defaultTime.begin(),defaultTime.end());
		defIter = defaultTime.begin();
		minTime = *defIter;

		switch (kthToDefault)
		{
			case 1:
			{
				k = expMap[minTime];
				break;
			}
			case 2:
			{
				defIter++;	
				minTime = *defIter;
				k = expMap[minTime];
				break;
			}
			case 3:
			{
				defIter++;
				defIter++;
				minTime = *defIter;
				k = expMap[minTime];
				break;
			}
			default:
				k = expMap[minTime];
				break;
		}
		
		Date effectiveDate = "today";
		
		int months = (int) (minTime - (minTime/100))*12;
		int years = (int) minTime / 100;
		Date defTime = effectiveDate.AddYears(years);
		defTime = effectiveDate.AddMonths(months);
		int diff = defTime - exposure[k].effectiveDate;

		if (defTime > exposure[k].maturityDate)
		{                     
		 //inflow = inflow + 0;								        // receive nothing
		    total_fixed_leg = exposure[k].totalPay;
		    //cout << "total_fixed_leg = " << total_fixed_leg << endl;
		    //accumulate(exposure[k].payments.begin(),exposure[k].payments.end(),(double)0.0);
	       	//printf("total = %0.3f\n",total_fixed_leg);						
			//(exposure[k].notional)*(exposure[k].spread)*((double)(diff)/360);
            outflow = outflow + total_fixed_leg;   // pay total discounted spread on notional														   // = sum of notional*(num days/360)*(spread)
		}
        else  
        {
				total_protection_leg = ((exposure[k].notional)*(1 - exposure[k].recoveryRate))*exp(-(exposure[k].riskfreeRate)*((double)diff/365));
				//printf("total_protection_leg = %0.3f\n",total_protection_leg);
				inflow = inflow + total_protection_leg;
				//cout << "inflow1 = " << inflow << endl;
				//printf("inflow = %0.3f\n",exp(-(exposure[k].riskfreeRate)*((double)diff/365)));
				//	(discount rate corresponding to default time)        
			    outflow = outflow + calcSumDiscPayments(defTime,k);   
			//cout << "outflow = " << outflow << endl;
			// sum of discounted protection payments up to time of default// = (value of 1 basis pt)*(min default 
                // time of all names in basket)           
		}	
	}
	std::cout << "inflow = " << inflow << endl;
	std::cout << "outflow = " << outflow << endl;
	aveInflow = (double) inflow/M;
	aveOutflow = (double) outflow/M;

	basketPrice = (aveInflow/aveOutflow)*(exposure[k].spread)*10000;
	std::cout << "basketPrice = " << basketPrice << endl;

	return 0;
}

/*
double Basket::priceBasket()
{
	int i, j, k, defExp;
	MRNG mrng;  // Mersenne generator
	MatrixUtil mu;
	double* dev = new double[NUM];
	double* z = new double[NUM];

	double aveInflow = 0.0;
	double aveOutflow = 0.0;
	vector<double> defaultTime;
	vector<double> normaldev;
	double minTime = 0.0;
	double basketPrice = 0.0;
	double sum, outflow = 0.0;
	double inflow = 0.0;
	int m = exposure.size();  // number of exposure in the basket
	int def = 0;
	double total_protection_leg, total_fixed_leg = 0.0;
	TNT::Array2D<double> R(3,3);
	std::map<double,int> expMap;
	std::cout.precision(10);

	// flat correlation structure
	//R << 1
	//  << 0.62 << 1
	//  << 0.50 << 0.30 << 1;
	//  << 0.62 << 0.62 << 0.62 << 1
	//  << 0.62 << 0.62 << 0.62 << 0.62 << 1;

	for (int i = 1; i <= 500000; i++)
	{
	    sum = 0.0;
		double* z = new double[m];
		//dev = mu.genCorrelatedDeviates(R,0.25,z);
		
		normaldev.empty();
		defaultTime.empty();
		normaldev.clear();
		defaultTime.clear();
		normaldev.empty();
		defaultTime.empty();
		// for each name in the basket
		for (int j = 0; j < m; j++)
		{
			dev = mu.genCholesky(R); //,step,z);
			//dev = mu.genCorrelatedDeviates_Cholesky(R,0.25,z);
			normaldev.push_back(mu.normalCalc(dev[j]));
			//cout << "normal = " << normaldev[j] << endl;
			if (normaldev[j] < exposure[j].hazardRate)
				def = 1;
			else
				def = 0;
			sum = sum + def;
			//defaultTime.push_back(-(log(1-normaldev[j]))/((exposure[j].hazRate[1])));
			double y = calcDefaultTime(-log(normaldev[j]),j);
			defaultTime.push_back(y);
			expMap[defaultTime[j]] = j;
		}
		vector<double>::iterator iter;
		vector<double>::iterator defIter;
		
		sort(defaultTime.begin(),defaultTime.end());
		defIter = defaultTime.begin();
		//iter = min_element(defaultTime.begin(),defaultTime.end());
		//cout << "iter = " << *iter << endl;
		minTime = *defIter;
		//sort(defaultTime.begin(),defaultTime.end());
		switch (kthToDefault)
		{
			case 1:
			{
				k = expMap[minTime];
				break;
			}
			case 2:
			{
				defIter++;	
				minTime = *defIter;
				k = expMap[minTime];
				break;
			}
			case 3:
			{
				defIter++;
				defIter++;
				minTime = *defIter;
				k = expMap[minTime];
				break;
			}
			default:
				k = expMap[minTime];
				break;
		}
		
		Date effectiveDate = "today";
		
		int months = (int) (minTime - (minTime/100))*12;
		int years = (int) minTime / 100;
		Date defTime = effectiveDate.AddYears(years);
		defTime = effectiveDate.AddMonths(months);

		//cout << "defTime = " << defTime << endl;
		//cout << "exposureMat = " << exposure[k].maturityDate << endl;
		int diff = defTime - exposure[k].effectiveDate;
		//printf("diff = %d\n",diff);

		if (defTime > exposure[k].maturityDate)
		{                     
				//inflow = inflow + 0;								// receive nothing
				total_fixed_leg = exposure[k].totalPay;
				//cout << "total_fixed_leg = " << total_fixed_leg << endl;
				//accumulate(exposure[k].payments.begin(),exposure[k].payments.end(),(double)0.0);
				//printf("total = %0.3f\n",total_fixed_leg);
					//(exposure[k].notional)*(exposure[k].spread)*((double)(diff)/360);
                outflow = outflow + total_fixed_leg;   // pay total discounted spread on notional														   // = sum of notional*(num days/360)*(spread)
		}
        else  
        {
				

				total_protection_leg = ((exposure[k].notional)*(1 - exposure[k].recoveryRate))*exp(-(exposure[k].riskfreeRate)*((double)diff/365));
				//printf("total_protection_leg = %0.3f\n",total_protection_leg);
				inflow = inflow + total_protection_leg;
				//cout << "inflow1 = " << inflow << endl;
				//printf("inflow = %0.3f\n",exp(-(exposure[k].riskfreeRate)*((double)diff/365)));
				//	(discount rate corresponding to default time)        
			    outflow = outflow + calcSumDiscPayments(defTime,k);   
				//cout << "outflow = " << outflow << endl;
				// sum of discounted protection payments up to time of default// = (value of 1 basis pt)*(min default 
                // time of all names in basket)           
		}	
		z = NULL;
	}
	std::cout << "inflow = " << inflow << endl;
	std::cout << "outflow = " << outflow << endl;
	aveInflow = (double) inflow/M;
	aveOutflow = (double) outflow/M;

	basketPrice = (aveInflow/aveOutflow)*(exposure[k].spread)*10000;
	std::cout << "basketPrice = " << basketPrice << endl;

	return 0;
}
*/

double Basket::calcSumDiscPayments(Date defDate, int j)
{

	double sum = 0.0;
	double discPay = 0.0;


	for (int i = 0; i < exposure[j].getPaySize(); i++)
	{
		if (exposure[j].paymentDates[i] < defDate)
		{
			int length = defDate - exposure[j].paymentDates[i];
			discPay = exposure[j].payments[i]*exp(-(exposure[j].riskfreeRate)*((double)length/365));
			sum = sum + discPay;
		}
	}
	return sum;
}

int Basket::getNumExposures() {
	return numExposures;
}

double Basket::calcDefaultTime(double dev, int j) {
	
	double tau = 0.0;
	double hazard_sum[10];
	double o[10] = {0.0};

	int k = exposure[0].hazRate.size()-1;
	int i = 0;
	double y = 0.0;

	
	for (int i = 0; i <= k; i++)
	{
		if (i == 0)
			hazard_sum[0] = exposure[j].hazRate[0];
		else
			hazard_sum[i] = hazard_sum[i-1] + exposure[j].hazRate[i];

		if (hazard_sum[i] >= dev)
		{	
			if (i == 0)
			{
				tau = dev/exposure[j].hazRate[0];
				o[j] = 1.0;
			}
			else
			{
				tau = ((dev - hazard_sum[i-1])/(exposure[j].hazRate[i]))+i;
				o[j] = 1.0;
			}
			break;
		}
	}
	if ((hazard_sum[k] <= dev) && (o[j] != 1))
		tau = ((dev - hazard_sum[k-1])/(exposure[j].hazRate[k])) + k;

	return tau;
}