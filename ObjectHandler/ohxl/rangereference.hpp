/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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
    \brief Class RangeReference - Parse range references in string format
*/

#include <ohxl/ohxldefines.hpp>
#include <boost/regex.hpp>
#include <string>

namespace ObjectHandler {

    //! A utility class for parsing range references in string format.
    /*! The constructor to this class accepts strings returned from
        Excel functions such as xlfGetName and xlfReftext.  Strings are
        parsed and their tokens stored in separate variables to facilitate
        comparisons such as determing whether one range contains another.

        Excel returns a variety of strings to represent references.
        The following formats are supported:

        Normal:
        \code
            "[Book1.xls]Sheet1!R1C1"
        \endcode

        Some excel functions prepend an '=' sign:
        \code
            "=[Book1.xls]Sheet1!R1C1"
        \endcode

        If book or sheet name contains spaces then Excel encloses the text
        in single quotes:
        \code
            "'[Bo ok1.xls]Sheet1'!R1C1"
        \endcode

        If book name contains dots then Excel encloses the text
        in single quotes:
        \code
            "'[Bo.ok1.xls]Sheet1'!R1C1"
        \endcode

        For a new book that hasn't yet been saved, excel omits the .xls suffix:
        \code
            "[Book1]Sheet1!R1C1"
        \endcode

        Sometimes the string includes the filesystem path:
        \code
            "='C:\\path\\to\\[Book1.xls]Sheet1'!R1C1"
        \endcode

        If the book contains a single sheet with the same name as the book,
        then the sheet name is omitted altogether and the book name is not
        enclosed in []s:
        \code
            "same_name.xls!R1C1"
            "'same name.xls'!R1C1"
        \endcode

        In all cases, the cell reference, represented above as R1C1, may also
        be given as R1C1:R9C9 i.e. a range consisting of multiple cells, in
        which case the RangeReference constructor sets multicell_ to true.
    */
    class RangeReference {
        friend std::ostream &operator<<(std::ostream&, const RangeReference&);
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the RangeReference object.
        /*! Parse the input string and store its component tokens in separate
            variables e.g. bookName_, sheetName_, etc.
        */
        RangeReference(const std::string &address);
        //@}

        //! \name Operators and Comparisons
        //@{
        //! Test two RangeReference objects for equality.
        /*! The two objects may in fact represent the same range even if they
            were initialized with different strings and this operator handles
            that case.
        */
        bool operator==(const RangeReference&) const;
        //! Determine whether this range contains the given one.
        /*! This functionality is presently used for processing error messages -
            An error message may be associated with a multi-cell range and the
            user can query any subset of that range and still get the relevant
            error message.
        */
        bool contains(const RangeReference&) const;
        //@}

        //! \name Error Messages
        //@{
        //! Assign an error message to this range.
        void setErrorMessage(const std::string &errorMessage);
        //! Retrieve the error message associated with this range.
        /*! This function is wrapped by end user function ohRangeRetrieveError().
        */
        const std::string errorMessage() const { return errorMessage_; }
        //@}

    private:
        static void initializeRegexes();
        bool initStandard();
        bool initSpecial();

        std::string address_;
        std::string bookName_;
        std::string sheetName_;
        bool multicell_;
        int rowStartNum_;
        int colStartNum_;
        int rowEndNum_;
        int colEndNum_;
        std::string errorMessage_;
        static boost::regex regexStandard_;
        static boost::regex regexSpecial_;
        static bool regexesInitialized_;
    };

    std::ostream &operator<<(std::ostream&, const RangeReference&);

}

