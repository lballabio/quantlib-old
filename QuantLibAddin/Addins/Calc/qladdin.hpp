
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

#include <com/sun/star/sheet/addin/XQL.hpp>
#include <com/sun/star/sheet/XAddIn.hpp>
#include <com/sun/star/lang/XServiceInfo.hpp>
#include <com/sun/star/lang/XServiceName.hpp>
#include <com/sun/star/lang/XTypeProvider.hpp>
#include <Addins/Calc/qldefs.hpp>
#include <functional>
#include <map>
#include <string>

struct OUSTRCOMP : public std::binary_function< STRING, STRING, bool > {
    bool operator()(STRING s1, STRING s2) { return ((s1.compareTo(s2) < 0)); }
};

class QLAddin :    public CSS::sheet::addin::XQL,
                    public CSS::sheet::XAddIn,
                    public CSS::lang::XServiceName,
                    public CSS::lang::XServiceInfo,
                    public CSS::lang::XTypeProvider {
    std::map< STRING, STRING, OUSTRCOMP > funcMap;
    // XInterface
    oslInterlockedCount m_refcount;
public:
    CSS::lang::Locale            aFuncLoc;
    static STRING                getImplementationName_Static();
    static SEQ( STRING )        getSupportedServiceNames_Static();
    // XInterface
    virtual CSS::uno::Any SAL_CALL queryInterface(
        CSS::uno::Type const & type) THROWDEF_RTE;
    virtual void SAL_CALL acquire() throw ();
    virtual void SAL_CALL release() throw ();
    // XAddIn
    virtual STRING SAL_CALL        getProgrammaticFuntionName(
        const STRING& aDisplayName) THROWDEF_RTE;
    virtual STRING SAL_CALL        getDisplayFunctionName(
        const STRING& aProgrammaticName ) THROWDEF_RTE;
    virtual STRING SAL_CALL        getFunctionDescription(
        const STRING& aProgrammaticName ) THROWDEF_RTE;
    virtual STRING SAL_CALL        getDisplayArgumentName(
        const STRING& aProgrammaticFunctionName, sal_Int32 nArgument ) THROWDEF_RTE;
    virtual STRING SAL_CALL        getArgumentDescription(
        const STRING& aProgrammaticFunctionName, sal_Int32 nArgument ) THROWDEF_RTE;
    virtual STRING SAL_CALL        getProgrammaticCategoryName(
        const STRING& aProgrammaticFunctionName ) THROWDEF_RTE;
    virtual STRING SAL_CALL        getDisplayCategoryName(
        const STRING& aProgrammaticFunctionName ) THROWDEF_RTE;
    // XServiceName
    virtual STRING SAL_CALL        getServiceName(  ) THROWDEF_RTE;
    // XServiceInfo
    virtual STRING SAL_CALL        getImplementationName(  ) THROWDEF_RTE;
    virtual sal_Bool SAL_CALL    supportsService(
        const STRING& ServiceName ) THROWDEF_RTE;
    virtual SEQ( STRING ) SAL_CALL    getSupportedServiceNames(  ) THROWDEF_RTE;
    // XTypeProvider
    virtual SEQ( CSS::uno::Type ) SAL_CALL getTypes() THROWDEF_RTE;
    virtual SEQ( sal_Int8 ) SAL_CALL getImplementationId() THROWDEF_RTE;
    // XLocalizable
    virtual void SAL_CALL        setLocale(
        const CSS::lang::Locale& eLocale ) THROWDEF_RTE;
    virtual CSS::lang::Locale SAL_CALL getLocale(  ) THROWDEF_RTE;
    // XQlUno
    QLAddin() throw ();

#include <Addins/Calc/autogen.hpp>

};
