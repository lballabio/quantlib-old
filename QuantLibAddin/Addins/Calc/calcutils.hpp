
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

ANY anyToANY(const ObjHandler::any_ptr &a); // convert boost::any to Calc Any
SEQSEQ( ANY ) getArray(ObjHandler::Properties properties,
                       STRING handle);
std::string OUStringToString(const STRING& s1);
ANY stringToANY(const std::string &s);

template < typename T >
class Conversion {
public:

    static void convertArray(const SEQSEQ(T)& s, T* &a, long &sz) {
        SEQ(T) s2 = s[0];
        sz = s.getLength() * s2.getLength();
        a = new T[sz];
        for (int i=0; i<s.getLength(); i++){
            s2 = s[i];
            for (int j=0; j<s2.getLength(); j++)
                a[i * s2.getLength() + j] = s2[j];
        }
    }

    static void convertArray(const SEQSEQ(STRING)& s, char** &a, long &sz) {
        SEQ(STRING) s2 = s[0];
        sz = s.getLength() * s2.getLength();
        a = new char*[sz];
        for (int i=0; i<s.getLength(); i++){
            s2 = s[i];
            for (int j=0; j<s2.getLength(); j++) {
                int idx = i * s2.getLength() + j;
                a[idx] = new char[s2.getLength() + 1];
                sprintf(a[idx], OUStringToString(s2[j]).c_str());
            }
        }
    }

    static void convertMatrix(const SEQSEQ(double)& s, T** &a, long &r, long &c) {
        SEQ(T) s2 = s[0];
        r = s.getLength();
        c = s2.getLength();
        a = new T*[r];
        for (int i=0; i<s.getLength(); i++) {
            s2 = s[i];
            a[i] = new T[c];
            for (int j=0; j<s2.getLength(); j++)
                a[i][j] = s2[j];
        }
    }

};

#endif

