
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

// this file generated automatically by autogen.py on Mon Jan 31 11:40:03 2005
// editing this file manually is not recommended

#ifdef WIN32
#pragma warning(disable: 4786)
#pragma warning(disable: 4503)
#endif

#include <Addins/Calc/qladdin.hpp>

QLAddin::QLAddin() throw () : m_refcount( 0 ) {

    //functions

    funcMap[ STRFROMANSI( "qlQuery" ) ]
        =  STRFROMANSI( "QL_QUERY" );
    funcMap[ STRFROMANSI( "qlLogfile" ) ]
        =  STRFROMANSI( "QL_LOGFILE" );

    //options

    funcMap[ STRFROMANSI( "qlStochasticProcess" ) ]
        =  STRFROMANSI( "QL_STOCHASTIC_PROCESS" );
    funcMap[ STRFROMANSI( "qlOptionVanilla" ) ]
        =  STRFROMANSI( "QL_OPTION_VANILLA" );
    funcMap[ STRFROMANSI( "qlOptionAsianC" ) ]
        =  STRFROMANSI( "QL_OPTION_ASIAN_C" );
    funcMap[ STRFROMANSI( "qlOptionAsianD" ) ]
        =  STRFROMANSI( "QL_OPTION_ASIAN_D" );
    funcMap[ STRFROMANSI( "qlOptionBarrier" ) ]
        =  STRFROMANSI( "QL_OPTION_BARRIER" );
    funcMap[ STRFROMANSI( "qlOptionSetEngine" ) ]
        =  STRFROMANSI( "QL_OPTION_SETENGINE" );

}

