
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#ifndef qla_calc_calcutils_hpp
#define qla_calc_calcutils_hpp

// convert boost::any to Calc Any
ANY anyToANY(const ObjHandler::any_ptr &a);
SEQSEQ( ANY ) getArray(ObjHandler::Properties properties, STRING handle);
std::string OUStringToString(const STRING& s1);
ANY stringToANY(const std::string &s);

template < typename T >
class Conversion {
public:

    static std::vector < T >convertVector(const SEQSEQ(T)& s) {
        std::vector < T >ret;
        if (!s.getLength())
            return ret;
        if (!s[0].getLength())
            return ret;
        for (int i=0; i<s.getLength(); i++)
            for (int j=0; j<s[i].getLength(); j++)
                ret.push_back(s[i][j]);
        return ret;
    }

    static std::vector < std::string >convertStrVector(const SEQSEQ(STRING)& s) {
        std::vector < std::string > ret;
        if (!s.getLength())
            return ret;
        if (!s[0].getLength())
            return ret;
        for (int i=0; i<s.getLength(); i++)
            for (int j=0; j<s[i].getLength(); j++)
                ret.push_back(OUStringToString(s[i][j]));
        return ret;
    }

    static std::vector < std::vector < T > >convertMatrix(const SEQSEQ(double)& s) {
        std::vector < std::vector < T > >ret;
        if (!s.getLength())
            return ret;
        if (!s[0].getLength())
            return ret;
        for (int i=0; i<s.getLength(); i++) {
            std::vector < T >row;
            for (int j=0; j<s[i].getLength(); j++)
                row.push_back(s[i][j]);
            ret.push_back(row);
        }
        return ret;
    }

};

#endif

