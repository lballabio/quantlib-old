
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

#define NUM_FUNCS 5
#define NUM_ATTS 6

static LPSTR func[NUM_FUNCS][NUM_ATTS] = {
	// utils
	{" qlQuery",			" RC",		" QL_QUERY",			" ", " 1", " QuantLib"},
	{" qlLogfile",			" RC",		" QL_LOGFILE",			" ", " 1", " QuantLib"},
	// options
	{" qlBlackscholes",		" REEEENN#"," QL_BLACKSCHOLES",		" ", " 1", " QuantLib"},
	{" qlOption",			" RCCENNN#"," QL_OPTION",			" ", " 1", " QuantLib"},
	{" qlOptionSetEngine",	" RCCN",	" QL_OPTION_SETENGINE",	" ", " 1", " QuantLib"},
};
