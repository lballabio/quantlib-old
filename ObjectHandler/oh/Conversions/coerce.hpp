
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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
    \brief Class Coerce - Support for automatic conversion of datatypes
*/

#ifndef oh_conversions_coerce_hpp
#define oh_conversions_coerce_hpp

#include <oh/exception.hpp>
#include <sstream>

namespace ObjectHandler {

    //! Base class in support of datatype coercion.
    /*! ObjectHandler supports the concept of coercion - a function expects an 
        argument of a given datatype, the user inputs a value of some other type and
        if possible the value is converted automatically.

        A coercion is a list of one-to-one conversions.  Each conversion is attempted
        in sequence and the return value of the first successful conversion is used.
        If none of the conversions succeeds then an exception is thrown.
    */
    template <class TypeIn, class TypeOut>
    class Coerce {
    public:

        //! Operator () - Coerce the value from the type provided to the type required.
        /*! Each of the defined conversions is attempted in turn and the function returns
            on the first successful conversion.

            If no conversion succeeds an exception is thrown.
        */
        TypeOut operator()(const TypeIn &in) {

            //OH_REQUIRE(!inputMissing(in), "invalid input");

            TypeOut out;
            for (Conversion *conversion = getConversions();
                    *conversion; ++conversion) {
                if ((*conversion)(in, out)) 
                    return out;
            }

            OH_FAIL("Unable to coerce value to type " 
                << typeid(TypeOut).name());
        }

        //! Override for operator () - special case where user has provided a default value.
        /*! If the user has provided a default value then call function inputMissing() to
            inspect the input parameter and determine whether it represents a null value.
            If inputMissing() returns true then use the default value provided, otherwise
            execute the normal implementation of operator ().
        */
        TypeOut operator()(const TypeIn &in, const TypeOut &defaultValue) {
            if (inputMissing(in)) {
                return defaultValue;
            } else {
                return this->operator()(in);
            }
        }

        virtual ~Coerce() {}

    protected:
        //! Typedef for a pointer to a conversion function.
        typedef bool (*Conversion)(const TypeIn&, TypeOut&);
        //! Retrieve the list of conversions defined for this instance of Coerce.
        /*! Pure virtual function which must be overridden as appropriate
            by each derived class.
        */
        virtual Conversion *getConversions() = 0;
        //! Function to indicate whether a given input is null.
        /*! This base class provides a dummy definition of this function returning
            false, indicating that all inputs are always populated.

            Derived classes may override this function to inspect the input argument
            and determine whether it represents some kind of null value to indicate
            that no input was provided.
        */
        virtual bool inputMissing(const TypeIn&) { return false; }
    };

}

#endif

