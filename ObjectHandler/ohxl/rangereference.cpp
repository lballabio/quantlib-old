
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

#include <ohxl/rangereference.hpp>
#include <ohxl/configuration.hpp>
#include <oh/exception.hpp>
#include <oh/utilities.hpp>
#include <boost/lexical_cast.hpp>

namespace ObjectHandler {

    boost::regex RangeReference::regexStandard_;
    boost::regex RangeReference::regexSpecial_;
    bool RangeReference::regexesInitialized_ = false;

    RangeReference::RangeReference(const std::string &address)
        : address_(address) {

        if (!regexesInitialized_) initializeRegexes();

        OH_REQUIRE(initStandard() || initSpecial(),
            "The string '" << address << "' is not a valid range reference");
    }

    void RangeReference::initializeRegexes() {

        std::ostringstream strStandard, strSpecial;
        strStandard << "=?'?.*\\[([\\w\\s-]+)(?:\\.XLS)?\\]([\\w\\s]+)'?!" 
            << Configuration::instance().rowCharacter() << "(\\d*)" 
            << Configuration::instance().colCharacter() << "(\\d*)(?::" 
            << Configuration::instance().rowCharacter() << "(\\d*)" 
            << Configuration::instance().colCharacter() << "(\\d*))?";
        strSpecial << "=?'?([\\w\\s]+)(?:\\.XLS)'?!" 
            << Configuration::instance().rowCharacter() << "(\\d*)" 
            << Configuration::instance().colCharacter() << "(\\d*)(?::" 
            << Configuration::instance().rowCharacter() << "(\\d*)" 
            << Configuration::instance().colCharacter() << "(\\d*))?";

        regexStandard_ = strStandard.str();
        regexSpecial_ = strSpecial.str();

        regexesInitialized_ = true;
    }

    bool RangeReference::initStandard() {
        // try to handle the usual case where book name != sheet name

        boost::smatch m; 
        if (!boost::regex_match(address_, m, regexStandard_))
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

    bool RangeReference::initSpecial() {
        // try to handle special case of book containing single sheet w/same name

        boost::smatch m; 
        if (!boost::regex_match(address_, m, regexSpecial_))
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

    void RangeReference::setErrorMessage(const std::string &errorMessage, const bool &append) {
        if (append) {
            std::ostringstream err;
            err << errorMessage_ << std::endl << std::endl << errorMessage;
            errorMessage_ = err.str();
        } else {
            errorMessage_ = errorMessage;
        }
    }

    bool RangeReference::operator==(const RangeReference &r) const {
        if (this->address_ == r.address_)
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
        out << "address =     " << r.address_ << std::endl;
        out << "bookName =    " << r.bookName_ << std::endl;
        out << "sheetName =   " << r.sheetName_ << std::endl;
        out << "multicell =   " << r.multicell_ << std::endl;
        out << "rowStartNum = " << r.rowStartNum_ << std::endl;
        out << "colStartNum = " << r.colStartNum_ << std::endl;
        out << "rowEndNum =   " << r.rowEndNum_ << std::endl;
        out << "colEndNum =   " << r.colEndNum_ << std::endl;
        return out;
    }

}

