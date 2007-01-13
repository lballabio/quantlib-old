
/*
 Copyright (C) 2007 Ferdinando Ametrano

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

/*! \file
    \brief is_less comparison functors and less predicates
*/

#ifndef oh_less_hpp
#define oh_less_hpp

#include <boost/algorithm/string/compare.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <algorithm>


namespace ObjHandler {

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

// the following would need cleaning up before committing to boost
#ifdef DONOTCOMPILE

namespace ObjHandler {

    //  is_less functor  -----------------------------------------------//

    //! is_less comparison functor
    /*!
        Standard STL less only handle comparison between arguments of the
        same type. This is a less restrictive version which wraps operator<.
    */
    template< typename T1, typename T2 >
    struct is_less : public std::binary_function<T1, T2, bool> {
        //! Function operator
        /*!
            Compare two operands applying operator<.
        */
        bool operator()(const T1& Arg1, const T2& Arg2) const {
		  return (Arg1 < Arg2);
        }
	};

    //! case insensitive version of is_less
    /*!
        Case insensitive comparison predicate.
        Comparison is done using specified locales.
    */
    template< typename T1, typename T2 >
    class is_iless : public std::binary_function<T1, T2, bool> {
      public:
        //! Constructor
        /*!
            \param loc locales used for comparison
        */
        is_iless(const std::locale& loc=std::locale()) : loc_(loc) {}
        //! Function operator
        /*!
            Compare two operands applying operator<. Case is ignored.
        */
        bool operator()(const T1& Arg1, const T2& Arg2) const {
		  return (std::toupper(Arg1, loc_) < std::toupper(Arg2, loc_));
        }
      private:
        std::locale loc_;
	};

    //  less predicate  -----------------------------------------------//

    //! 'Less' predicate
    /*!
        This predicate holds when the test container is equal to the
        input container i.e. all elements in both containers are same.
        When the optional predicate is specified, it is used for character-wise
        comparison.

        \param Input An input sequence
        \param Test A test sequence
        \param Comp An element comparison predicate
        \return The result of the test

    */
    template<typename Range1T, typename Range2T, typename PredicateT>
    inline bool less( 
        const Range1T& Input, 
        const Range2T& Test,
        PredicateT Comp)
    {
        typedef BOOST_STRING_TYPENAME 
            range_const_iterator<Range1T>::type Iterator1T;
        typedef BOOST_STRING_TYPENAME 
            range_const_iterator<Range2T>::type Iterator2T;
            
        typedef BOOST_STRING_TYPEVALUE
            range_value<Range1T>::type Type1T;
        typedef BOOST_STRING_TYPEVALUE 
            range_value<Range2T>::type Type2T;

        Iterator1T InputEnd=end(Input);
        Iterator2T TestEnd=end(Test);

        Iterator1T it=begin(Input);
        Iterator2T pit=begin(Test);

        Type1T type1;
        Type2T type2;

        for(;
            it!=InputEnd && pit!=TestEnd;
            ++it,++pit)
        {
            type1 = *it;
            type2 = *pit;
            if (Comp(type1, type2))
                return true;
            else if (Comp(type2, type1))
                return false;
        }

        // always have comparison functions return false for equal values
        return (it==InputEnd) && (pit!=TestEnd);
    }

    //! 'Less' predicate
    /*!
        \overload
    */
    template<typename Range1T, typename Range2T>
    inline bool less(
        const Range1T& Input, 
        const Range2T& Test)
    {
        return less(Input, Test, is_less());
    }

    //! 'Less' predicate ( case insensitive )
    /*!
        This predicate holds when the test container is equal to the
        input container i.e. all elements in both containers are same.
        Elements are compared case insensitively.

        \param Input An input sequence
        \param Test A test sequence
        \param Loc A locale used for case insensitive comparison
        \return The result of the test

    */
    template<typename Range1T, typename Range2T>
    inline bool iless( 
        const Range1T& Input, 
        const Range2T& Test,
        const std::locale& Loc=std::locale())
    {
        return less(Input, Test, (is_iless(Loc)));
    }

}

namespace boost {
    // pull names to the boost namespace
    using ObjHandler::is_less;
    using ObjHandler::is_iless;
    using ObjHandler::less;
    using ObjHandler::iless;
}

#endif

#endif
