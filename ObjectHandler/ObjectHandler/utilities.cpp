
/*
 Copyright (C) 2004 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ObjectHandler/utilities.hpp>
#include <ObjectHandler/exception.hpp>
#include <sstream>
#include <fstream>

using namespace std;

string toUpper(const string& s) {
  string upper(s);
  for(size_t i=0; i<s.length(); i++)
    upper[i] = toupper(upper[i]);
  return upper;
}

string AnyToString(const any_ptr &a) {
	ostringstream s;
	if (a->type() == typeid(int))
		s << boost::any_cast<int>(*a);
	else if (a->type() == typeid(double))
		s << boost::any_cast<double>(*a);
	else if (a->type() == typeid(string))
		s << boost::any_cast<string>(*a);
	else
		throw Exception("AnyToString: unrecognized type");
	return s.str();
}

// FIXME
string logFileName;	// "" = logging disabled

int setLogFile(const string &newLogFileName) {
	ofstream logFile;
	if (!newLogFileName.length()) {
		logFileName = "";
		return 0;
	}
	if (!logFileName.compare(string(newLogFileName)))
		return 1;	// continue logging to same file
	logFile.open(newLogFileName.c_str(), ios::app);
	if (logFile.is_open()) {
		logFile << "logging enabled" << endl;
		logFile.close();
		logFileName = newLogFileName;
		return 1;
	} else {
		logFileName = "";
		throw Exception("setLogFile: error opening logfile");
	}
}

void logMessage(const string &msg) {
	ofstream log1;
	if (logFileName.length()) {
		log1.open(logFileName.c_str(), ios::app);
		if (log1.is_open()) {
			log1 << msg << endl;
			log1.close();
		} else // error - disable logging
			logFileName = "";
	}
}
