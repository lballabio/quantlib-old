
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

#ifdef WIN32
#pragma warning(disable: 4786)
#pragma warning(disable: 4503)
#endif

#include <Addins/Calc/qladdin.hpp>

QLAddin::QLAddin() throw () : m_refcount( 0 ) { 
//	qladdin
	funcMap[ STRFROMANSI( "incrementNum" ) ]
		=  STRFROMANSI( "QL_TEST1" );
//	options
	funcMap[ STRFROMANSI( "qlBlackScholes" ) ]
		=  STRFROMANSI( "QL_BLACKSCHOLES" );
	funcMap[ STRFROMANSI( "qlOption" ) ]
		=  STRFROMANSI( "QL_OPTION" );
	funcMap[ STRFROMANSI( "qlOptionSetEngine" ) ]
		=  STRFROMANSI( "QL_OPTION_SETENGINE" );
//	functions
	funcMap[ STRFROMANSI( "qlQuery" ) ]
		=  STRFROMANSI( "QL_QUERY" );
	funcMap[ STRFROMANSI( "qlLogfile" ) ]
		=  STRFROMANSI( "QL_LOGFILE" );
}