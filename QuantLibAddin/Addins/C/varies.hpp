
/*
 Copyright (C) 2004 Eric Ehlers

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

#ifndef qla_varies_hpp
#define qla_varies_hpp

    void propertiesToVaries(const ObjHandler::Properties &properties, 
            VariesList *variesList);

template < typename T >
class Conversion {
public:

    static std::vector < T > convertVector(const T* &a, const long &sz) {
        std::vector < T > ret;
        for (int i=0; i<sz; i++)
            ret.push_back(a[i]);
        return ret;
    }

    static std::vector < std::string > convertVector(char** &a, const long &sz) {
        std::vector < std::string > ret;
        for (int i=0; i<sz; i++)
            ret.push_back(std::string(a[i]));
        return ret;
    }

    static std::vector < std::vector < T > >convertMatrix(T** &a, const long &r, const long &c) {
        std::vector < std::vector < T > > ret;
        for (int i=0; i<r; i++) {
            std::vector < T > row;
            for (int j=0; j<c; j++)
                row.push_back(a[i][j]);
            ret.push_back(row);
        }
        return ret;
    }

};

#endif

