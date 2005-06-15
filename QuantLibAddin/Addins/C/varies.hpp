
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

#ifndef qla_varies_hpp
#define qla_varies_hpp

void propertyVectorToVariesList(const ObjHandler::Properties &properties, 
        VariesList *variesList);
boost::any variesToBoostAny(const Varies &v);
void boostAnyToVaries(const boost::any &a, Varies *v);
void boostAnyVectorToVaries(const std::vector < boost::any >&a, VariesList *v);
void boostAnyMatrixToVaries(const std::vector < std::vector < boost::any > >&a, VariesList *v);

template < typename T >
class Conversion {
public:

    static std::vector < T > convertVector(T* &a, const long &sz) {
        std::vector < T > v;
        for (int i=0; i<sz; i++)
            v.push_back(a[i]);
        return v;
    }

    static std::vector < std::string > convertVector(char** &a, const long &sz) {
        std::vector < std::string > v;
        for (int i=0; i<sz; i++)
            v.push_back(std::string(a[i]));
        return v;
    }

    static std::vector < boost::any > convertVector(VariesList &a, const long &sz) {
        std::vector < boost::any > v;
        for (int i=0; i<a.count; i++)
            v.push_back(variesToBoostAny(a.varies[i]));
        return v;
    }

    static std::vector < bool > convertVector(unsigned char *a, const long &sz) {
        std::vector < bool > v;
        for (int i=0; i<sz; i++)
            v.push_back(a[i]);
        return v;
    }

    static std::vector < std::vector < T > >convertMatrix(
            T** &a, const long &r, const long &c) {
        std::vector < std::vector < T > > vv;
        for (int i=0; i<r; i++) {
            std::vector < T > v;
            for (int j=0; j<c; j++)
                v.push_back(a[i][j]);
            vv.push_back(v);
        }
        return vv;
    }

    static std::vector < std::vector < bool > >convertMatrix(
            unsigned char **a, const long &r, const long &c) {
        std::vector < std::vector < bool > > vv;
        for (int i=0; i<r; i++) {
            std::vector < bool > v;
            for (int j=0; j<c; j++)
                v.push_back(a[i][j]);
            vv.push_back(v);
        }
        return vv;
    }

    static std::vector < std::vector < std::string > >convertMatrix(
            char** &a, const long &r, const long &c) {
        std::vector < std::vector < std::string > >vv;
        for (int i=0; i<r; i++) {
            std::vector < std::string >v;
            for (int j=0; j<c; j++)
                v.push_back(std::string(a[i * c + j]));
            vv.push_back(v);
        }
        return vv;
    }

    static std::vector < std::vector < boost::any > >convertMatrix(
            const VariesList &vl, const long &r, const long &c) {
        QL_REQUIRE(vl.count == r * c, "invalid size");
        std::vector < std::vector < boost::any > >vv;
        for (int i=0; i<r; i++) {
            std::vector < boost::any >v;
            for (int j=0; j<c; j++)
                v.push_back(variesToBoostAny(vl.varies[i * c + j]));
            vv.push_back(v);
        }
        return vv;
    }

    // FIXME functions below not yet implemented
    static void convertArray(const std::vector < T > &, T**) {
    }

    static void convertArray(const std::vector < bool > &, unsigned char**) {
    }

    static void convertArray(const std::vector < T > &, char**) {
    }

    static void convertArrayArray(const std::vector < std::vector < T > >&, T**) {
    }

    static void convertArrayArray(const std::vector < std::vector < bool > >&, unsigned char**) {
    }

    static void convertArrayArray(const std::vector < std::vector < T > >&, char**) {
    }

};

#endif

