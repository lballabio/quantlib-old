/*! \file swaptionSmileMurexExport.hpp
    \brief Export swaption volatility structure to a xml file readable by Murex
	since Murex expects absolute volatilities but stores spreads w.r.t. a reference atm surface
	it is possible to import this reference atm surface from xml and use it to compute absolute
	volatilities via reference atm + given volstructure smile spread
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <iostream>
#include <fstream>
#include <boost/property_tree/detail/rapidxml.hpp>
#include <map>

#ifndef quantlib_swaptionSmileMurexExport_hpp
#define quantlib_swaptionSmileMurexExport_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

		/*! Exporter is set up by swaption volatility structure, xml output path, 
		    Murex specific nickname (e.g. MDS), date (e.g. TODAY), swap (e.g. EUR SW VOL),
			option names, swap names (must match exactly Murex names, e.g. 3M, 6M, 1Y, including case sensitivity),
			strike spreads (e.g. -0.02, -0.01, -0.05, 0.025, 0.0, 0.025, 0.05, 0.01, 0.02),
			optionally an in path for the reference atm surface which is used to compute absolute volatilities from smile spreads */

	bool swaptionSmileMurexExport(boost::shared_ptr<SwaptionVolatilityStructure> vol,
				string outPath, string nickname, string date, string swap, 
				vector<string>& optionNames, vector<string>& swapNames,
				vector<Period>& optionTenors, vector<Period>& swapTenors,
				vector<double> strikeSpreads, string referenceAtmInPath="");

}


#endif

