/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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

/*! \file
    \brief Class ConvertOper - Perform datatype conversions of OPERs
*/

#ifndef oh_convert_oper_hpp
#define oh_convert_oper_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <oh/variant.hpp>
#include <ohxl/xloper.hpp>
#include <string>

namespace ObjectHandler {

    //! Perform datatype conversions of OPERs
    /*! This class implements a thin wrapper around the Excel OPER datatype,
        supporting datatype conversions and other functionality required by 
        ObjectHandler.

        This class is intended solely for use with OPERs that have been received
        as arguments to an Addin function, i.e. where memory management of the
        OPER is performed by Excel.

        The same interface is supported by classes ConvertOper and Variant, allowing
        any conversion algorithm that is based on VariantToScalar to be applied to
        either class, or to other user-defined types which support the same interface.
    */
    class DLL_API ConvertOper {
    public:

        //! Constructor - initialize the variant.
        ConvertOper(const OPER &xIn, const bool &decayVectorToScalar = true);

        //! \name Inspectors
        //@{
        //! Return the datatype of the underlying value.
        Type type() const;
        //! Indicate whether the OPER value is missing.
        bool missing() const;
        //! Indicate whether the OPER contains an error value.
        bool error() const;
        //@}

        //! \name Conversion Operators
        //@{
        //! Convert the OPER to a long.
        operator long() const;
        //! Convert the OPER to a double.
        operator double() const;
        //! Convert the OPER to a boolean.
        operator bool() const;
        //! Convert the OPER to a std::string.
        operator std::string() const;
        //! Convert the OPER to a Variant.
        operator Variant() const;
        //@}

        //! Return a const pointer to the underlying OPER.
        const OPER *operator->() const { return oper_; }
        //! Return a const pointer to the underlying OPER.
        const OPER *get() const { return oper_; }

    private:
        // The underlying OPER.
        const OPER *oper_;
        // A utility function for converting strings.
        static std::string strConv(const OPER *xString);
    };

}

#endif

