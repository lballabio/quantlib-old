
/*
 Copyright (C) 2006 Eric Ehlers

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
    \brief Object class
*/

#include <string>

namespace ObjHandler {
    //! A utility class for parsing range references in string format.
    /*! The constructor to this class accepts strings returned from
        Excel functions such as xlfGetName and xlfReftext.  Strings are
        parsed and their tokens stored in separate variables to facilitate
        comparisons such as determing whether one range contains another.

        Excel returns a variety of strings to represent references.
        The following formats are supported:

        Normal:
            "[Book1.xls]Sheet1!R1C1"
        Some excel functions prepend an '=' sign:
            "=[Book1.xls]Sheet1!R1C1"
        If book or sheet name contains spaces then Excel encloses the text
        in single quotes:
            "'[Bo ok1.xls]Sheet1'!R1C1"
        For a new book that hasn't yet been saved, excel omits the .xls suffix:
            "[Book1]Sheet1!R1C1"
        Sometimes the string includes the filesystem path:
            "='C:\\path\\to\\[Book1.xls]Sheet1'!R1C1"
        If the book contains a single sheet with the same name as the book,
        then the sheet name is omitted altogether and the book name is not
        enclosed in []s:
            "same_name.xls!R1C1"
            "'same name.xls'!R1C1"

        In all cases, the cell reference, represented above as R1C1, may also
        be given as R1C1:R9C9 i.e. a range consisting of multiple cells, in 
        which case the RangeReference constructor sets multicell_ to true.
    */

    class RangeReference {
    public:
        RangeReference(const std::string &address);
        bool operator==(const RangeReference&) const;
        bool contains(const RangeReference&) const;
        friend std::ostream &operator<<(std::ostream&, const RangeReference&);
    private:
        std::string addressOriginal_;
        std::string addressUpper_;
        std::string bookName_;
        std::string sheetName_;
        bool multicell_;
        int rowStartNum_;
        int colStartNum_;
        int rowEndNum_;
        int colEndNum_;
        bool init1();
        bool init2();
    };

std::ostream &operator<<(std::ostream&, const RangeReference&);

}

