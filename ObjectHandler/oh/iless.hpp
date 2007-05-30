
/*
 Copyright (C) 2007 Ferdinando Ametrano

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
    \brief is_less comparison functors and less predicates
*/

#ifndef oh_less_hpp
#define oh_less_hpp

#include <boost/algorithm/string/compare.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <algorithm>

namespace ObjectHandler {

    //! std::string specialized case insensitive version of less
    /*!
        Case insensitive comparison predicate.
        Comparison is done using specified locales.
    */
    class my_iless : public std::binary_function<std::string, std::string, bool> {
      public:
        //! Constructor
        /*!
            \param loc locales used for comparison
        */
        my_iless(const std::locale& loc=std::locale()) : loc_(loc) {}
        //! Function operator
        /*!
            Compare two operands applying operator<. Case is ignored.
        */
        bool operator()(const std::string& Arg1,
                        const std::string& Arg2) const {
            std::string::const_iterator InputEnd=Arg1.end();
            std::string::const_iterator TestEnd=Arg2.end();

            std::string::const_iterator it=Arg1.begin();
            std::string::const_iterator pit=Arg2.begin();
            char char1, char2;
            for(;
                it!=InputEnd && pit!=TestEnd;
                ++it, ++pit)
            {
                char1 = std::toupper(*it, loc_);
                char2 = std::toupper(*pit, loc_);
                if (char1 < char2)
                    return true;
                else if (char2 < char1)
                    return false;
            }

            // always have comparison functions return false for equal values
            return (it==InputEnd) && (pit!=TestEnd);
        }
      private:
        std::locale loc_;
	};

}

#endif

