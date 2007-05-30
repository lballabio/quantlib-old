
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
    \brief Class Xloper - Perform RAII for Excel's XLOPER datatype.
*/

#ifndef oh_xloper_hpp
#define oh_xloper_hpp

#include <xlsdk/xlsdkdefines.hpp>

namespace ObjectHandler {

    //! Perform RAII for Excel's XLOPER datatype.
    /*! This class implements a thin wrapper for an Excel XLOPER value.
        The class is intended specifically to manage XLOPERs which are declared
        on the stack and passed to Excel in order to receive the return value.

        ~Xloper calls xlFree on the underlying XLOPER, relieving the client code
        from the need to manage this e.g. in the event of the stack unwinding after
        an exception.
    */
    class DLL_API Xloper {        
    public:

        //! \name Structors
        //@{
        //! Constructor - initialize the type of the underling XLOPER to zero.
        /*! Client code may consult this value to determine whether or not
            any memory has subsequently been allocated to the XLOPER by Excel.
        */
        Xloper() { xloper_.xltype = 0; }

        //! Destructor - call xlFree on the XLOPER, if memory has been allocated.
        ~Xloper() {
            if (xloper_.xltype)
                Excel(xlFree, 0, 1, &xloper_);
        }
        //@}

        //! \name Inspectors
        //@{
        //! operator& - return the address of the underlying XLOPER.
        XLOPER *operator&() { return &xloper_; }
        //! operator-> - return the address of the underlying XLOPER.
        const XLOPER *operator->() const { return &xloper_; }
        //! operator() - return a const reference to the underlying XLOPER.
        const XLOPER &operator()() const { return xloper_; }
        //@}

    private:

        XLOPER xloper_;

    };

}

#endif

