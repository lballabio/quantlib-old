#include <strstream>
#include <iostream>
#include <fstream>
#include "Basket.h"
#define SIZE_X 1000
using namespace std;

void main()
{
	CDO cdo;
	
	std::cout << "Pricing CDO..." << endl;
	Tranche tranche1(0,0.03,"equity");
	Tranche tranche2(0.03,0.14,"mezzanine");
	Tranche tranche3(0.14,0.50,"senior");
	Tranche tranche4(0.50,1.00,"senior");
	
	cdo.addTranche(tranche1);
	cdo.addTranche(tranche2);
	cdo.addTranche(tranche3);
	cdo.addTranche(tranche4);
	cdo.priceTranche(100);
}