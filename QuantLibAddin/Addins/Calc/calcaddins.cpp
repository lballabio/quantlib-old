/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*	
 Copyright (C) 2004, 2005, 2006, 2008 Eric Ehlers
 Copyright (C) 2009 Roland Lichters

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <calcaddins.hpp>

//XCalcAddins

/******************************************************************************
 * Start implementing the methods here ...
 * Eric has put these into separate cpp files for each function.
 * I have just kept the basic examples (method1,...,method5 here. 
 *****************************************************************************/

using namespace QuantLib;

/*
long CalcAddins_impl::methodOne() throw (RuntimeException) {
//     Settings::instance().evaluationDate() = Date (27, November, 2009);
//     Date today = Settings::instance().evaluationDate();
//     Date next = TARGET().advance(today, 10*Years, Following);
//     return  next - today;
    return 1;
}

long CalcAddins_impl::methodTwo(long dummy) throw (RuntimeException) {
    return 2 + dummy;
}

double CalcAddins_impl::methodThree(long dummy, double m) 
    throw (RuntimeException) {
    return m * (2 + dummy);
}

double CalcAddins_impl::methodFour(double m) throw (RuntimeException) {
    double u = (double)rand() / RAND_MAX; // uniform on (0;1)
    return - m * log(1.0 - u); // exponential with mean m
}

//It's an array operation, should be called like : {=METHODFIVE(A1:B4)}
Sequence<Sequence<long> > 
CalcAddins_impl::methodFive(const Sequence<Sequence<long> > &aValList)
    throw (RuntimeException) {
    long n1, n2;
    long nE1 = aValList.getLength();
    long nE2;
    Sequence<Sequence<long> > temp = aValList;
    for( n1 = 0 ; n1 < nE1 ; n1++ ) {
        Sequence<long> rList = temp[ n1 ];
        nE2 = rList.getLength();
        for( n2 = 0 ; n2 < nE2 ; n2++ )
            rList[ n2 ] += 4;
        temp[n1]=rList;
    }
    return temp;
}
*/
/*****************************************************************************
 * ... and stop implementing the methods here.
 *****************************************************************************/

// XAddIn
/*
 * Not used by Calc, so return an empty string
 */
OUString CalcAddins_impl::getProgrammaticFuntionName(
                      const OUString& aDisplayName) throw (RuntimeException) {
    return OUString(); // not used by calc ?
}

OUString CalcAddins_impl::getDisplayFunctionName(
                 const OUString& aProgrammaticName) throw (RuntimeException) {
    return funcMap[ aProgrammaticName ];
}

OUString CalcAddins_impl::getFunctionDescription(
                  const OUString& aProgrammaticName) throw (RuntimeException) {
    return funcDesc[ aProgrammaticName ];
}

OUString CalcAddins_impl::getDisplayArgumentName(
                     const OUString& aProgrammaticName, ::sal_Int32 nArgument) 
    throw (RuntimeException) {
    std::map< STRING, std::vector < STRING > >::const_iterator i =
        argName.find(aProgrammaticName);
    if (i == argName.end())
        return STRFROMASCII("no help available");
    else {
        std::vector < STRING >v = i->second;
        return v[nArgument];
    }
}

OUString CalcAddins_impl::getArgumentDescription(
                                        const OUString& aProgrammaticName, 
                                        ::sal_Int32 nArgument) 
    throw (RuntimeException) {
    std::map< STRING, std::vector < STRING > >::const_iterator i =
        argDesc.find(aProgrammaticName);
    if (i == argDesc.end())
        return STRFROMASCII("no help available");
    else {
        std::vector < STRING >v = i->second;
        return v[nArgument];
    }
}

OUString CalcAddins_impl::getProgrammaticCategoryName(
                  const OUString& aProgrammaticName) throw (RuntimeException) {
    return STRFROMASCII( "Add-In" );
}

OUString CalcAddins_impl::getDisplayCategoryName(
                  const OUString& aProgrammaticName) throw (RuntimeException) {
    return STRFROMASCII( "Add-In" );
}

//XServiceName
OUString CalcAddins_impl::getServiceName() throw (RuntimeException) {
    return STRFROMASCII( _serviceName_ );
}

//XServiceInfo
static OUString getImplementationName_CalcAddins_impl() 
    throw (RuntimeException) {
    return STRFROMASCII( _implName_ );
}

OUString CalcAddins_impl::getImplementationName() throw (RuntimeException) {
    return getImplementationName_CalcAddins_impl();
}

::sal_Bool CalcAddins_impl::supportsService(OUString const & serviceName) 
    throw (RuntimeException) {
    return serviceName.compareToAscii( _serviceName_ ) == 0 || 
        serviceName.compareToAscii( _AddserviceName_ ) == 0;
}

static Sequence<OUString> getSupportedServiceNames_CalcAddins_impl() 
    throw (RuntimeException) {
    Sequence< OUString > name(2);
    name[0] = STRFROMASCII( _serviceName_ );
    name[1] = STRFROMASCII( _AddserviceName_ );
    return name;
}

Sequence< OUString > CalcAddins_impl::getSupportedServiceNames() 
    throw (RuntimeException) {
    return getSupportedServiceNames_CalcAddins_impl();
}

//XLocalizable
void CalcAddins_impl::setLocale(const lang::Locale& eLocale) 
    throw (RuntimeException) {
    locale = eLocale;
}

lang::Locale CalcAddins_impl::getLocale() throw (RuntimeException) {
    return locale;
}

static Reference< XInterface > 
SAL_CALL create_CalcAddins_impl(Reference<XComponentContext> const & xContext) 
    SAL_THROW( () ) {
    return static_cast< ::cppu::OWeakObject * > ( new CalcAddins_impl );
}

static struct ::cppu::ImplementationEntry s_component_entries[] = {
    { create_CalcAddins_impl, getImplementationName_CalcAddins_impl,
      getSupportedServiceNames_CalcAddins_impl, 
      ::cppu::createSingleComponentFactory, 0, 0 },
    { 0, 0, 0, 0, 0, 0 }
};


extern "C" {
	void SAL_CALL component_getImplementationEnvironment(
                                            sal_Char const ** ppEnvTypeName, 
                                            uno_Environment ** ppEnv) {
		*ppEnvTypeName = CPPU_CURRENT_LANGUAGE_BINDING_NAME;
	}

	sal_Bool SAL_CALL component_writeInfo(lang::XMultiServiceFactory * xMgr, 
                                          registry::XRegistryKey * xRegistry) { 
		return ::cppu::component_writeInfoHelper( xMgr, xRegistry, 
                                                  s_component_entries );
	}

	void * SAL_CALL component_getFactory( sal_Char const * implName,
		lang::XMultiServiceFactory * xMgr, registry::XRegistryKey * xRegistry) {
		return ::cppu::component_getFactoryHelper(implName, xMgr, xRegistry, 
                                                  s_component_entries );
	}
}
