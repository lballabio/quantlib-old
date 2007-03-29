
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#include <ohxl/rangereference.hpp>
#include <oh/exception.hpp>
#include <oh/utilities.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>

#ifdef OHXL_ENABLE_GARBAGE_COLLECTION

namespace ObjHandler {

    RangeReference::RangeReference(const std::string &address)
    : addressOriginal_(address) {

        addressUpper_ = boost::algorithm::to_upper_copy(address);

        if (!init1() && !init2()) {
            std::ostringstream err;
            err << "the string '" << address << "' is not a valid range reference";
            throw Exception(err.str().c_str());
        }
    }

    bool RangeReference::init1() {
        // try to handle case where book name != sheet name
        static boost::regex r(
          "=?'?.*\\[([\\w\\s-]+)(?:\\.XLS)?\\]([\\w\\s]+)'?!R(\\d*)C(\\d*)(?::R(\\d*)C(\\d*))?");
        boost::smatch m; 
        if (!boost::regex_match(addressUpper_, m, r))
            return false;

        bookName_ = m[1];
        sheetName_ = m[2];
        rowStartNum_ = boost::lexical_cast<int>(m[3]);
        colStartNum_ = boost::lexical_cast<int>(m[4]);
        if (m[5].matched) {
            multicell_ = true;
            rowEndNum_ = boost::lexical_cast<int>(m[5]);
            colEndNum_ = boost::lexical_cast<int>(m[6]);
        } else {
            multicell_ = false;
            rowEndNum_ = -1;
            colEndNum_ = -1;
        }
        return true;
    }

    bool RangeReference::init2() {
        // try to handle case of book containing single sheet w/same name
        static boost::regex r(
          "=?'?([\\w\\s]+)(?:\\.XLS)'?!R(\\d*)C(\\d*)(?::R(\\d*)C(\\d*))?");
        boost::smatch m; 
        if (!boost::regex_match(addressUpper_, m, r))
            return false;

        bookName_ = m[1];
        sheetName_ = m[1];
        rowStartNum_ = boost::lexical_cast<int>(m[2]);
        colStartNum_ = boost::lexical_cast<int>(m[3]);
        if (m[4].matched) {
            multicell_ = true;
            rowEndNum_ = boost::lexical_cast<int>(m[4]);
            colEndNum_ = boost::lexical_cast<int>(m[5]);
        } else {
            multicell_ = false;
            rowEndNum_ = -1;
            colEndNum_ = -1;
        }
        return true;
    }

    bool RangeReference::contains(const RangeReference &r) const {
        if (*this == r) 
            return true;

        if (this->bookName_ != r.bookName_)
            return false;
        if (this->sheetName_ != r.sheetName_)
            return false;
        if (this->multicell_) {
            if (r.multicell_) {
                return r.rowStartNum_ >= this->rowStartNum_
                    && r.rowEndNum_ <= this->rowEndNum_
                    && r.colStartNum_ >= this->colStartNum_
                    && r.colEndNum_ <= this->colEndNum_;
            } else {
                return r.rowStartNum_ >= this->rowStartNum_
                    && r.rowStartNum_ <= this->rowEndNum_
                    && r.colStartNum_ >= this->colStartNum_
                    && r.colStartNum_ <= this->colEndNum_;
            }
        } else {
            if (r.multicell_)
                return false;
            return r.rowStartNum_ == this->rowStartNum_
                && r.colStartNum_ == this->colStartNum_;
        }
    }

    bool RangeReference::operator==(const RangeReference &r) const {
        if (this->addressOriginal_ == r.addressOriginal_)
            return true;
        return this->bookName_ == r.bookName_
            && this->sheetName_ == r.sheetName_
            && this->multicell_ == r.multicell_
            && this->rowStartNum_ == r.rowStartNum_
            && this->colStartNum_ == r.colStartNum_
            && this->rowEndNum_ == r.rowEndNum_
            && this->colEndNum_ == r.colEndNum_;
    }

    std::ostream &operator<<(std::ostream &out, const RangeReference &r) {
        out << "originalAddress = " << r.addressOriginal_ << std::endl;
        out << "bookName =        " << r.bookName_ << std::endl;
        out << "sheetName =       " << r.sheetName_ << std::endl;
        out << "multicell =       " << r.multicell_ << std::endl;
        out << "rowStartNum =     " << r.rowStartNum_ << std::endl;
        out << "colStartNum =     " << r.colStartNum_ << std::endl;
        out << "rowEndNum =       " << r.rowEndNum_ << std::endl;
        out << "colEndNum =       " << r.colEndNum_ << std::endl;
        return out;
    }

}

#endif // OHXL_ENABLE_GARBAGE_COLLECTION

