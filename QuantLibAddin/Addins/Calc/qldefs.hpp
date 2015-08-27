
/*
 Copyright (C) 2004 Eric Ehlers

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

#ifndef qla_calc_defs_hpp
#define qla_calc_defs_hpp

//#include <com/sun/star/sheet/addin/XQL.hpp>
#include <com/sun/star/sheet/XAddIn.hpp>
#include <com/sun/star/lang/XServiceInfo.hpp>
#include <com/sun/star/lang/XServiceName.hpp>
#include <com/sun/star/lang/XTypeProvider.hpp>

#define CSS                 ::com::sun::star
#define SEQ(c)              CSS::uno::Sequence< c >
#define SEQSEQ(c)           CSS::uno::Sequence< CSS::uno::Sequence< c > >
#define STRING              ::rtl::OUString
#define STRFROMANSI(s)      STRING( s, strlen( s ), RTL_TEXTENCODING_MS_1252 )
#define STRFROMASCII(s)     STRING::createFromAscii( s )
#define THROWDEF_RTE        throw(CSS::uno::RuntimeException)
#define THROWDEF_RTE_IAE    throw(CSS::uno::RuntimeException,CSS::lang::IllegalArgumentException)
#define THROW_RTE           throw CSS::uno::RuntimeException()
#define THROW_MSG(m,r)      throw(CSS::uno::RuntimeException( m , r))
#define REF(c)              CSS::uno::Reference< c >
#define ANY                 CSS::uno::Any

#endif

