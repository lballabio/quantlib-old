/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005, 2008 Plamen Neykov

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

#include <ohxl/conversions/scalartooper.hpp>
#include <ohxl/conversions/vectortooper.hpp>
#include <ohxl/conversions/matrixtooper.hpp>
#include <vector>
#include <string>
#include <oh/property.hpp>

namespace ObjectHandler {

    DLL_API void scalarToOper(const long &value, OPER &xLong, bool dllToFree, bool expandVector) {
        xLong.xltype = xltypeNum;
        xLong.val.num = value;
    }

    DLL_API void scalarToOper(const double &value, OPER &xDouble, bool dllToFree, bool expandVector) {
        xDouble.xltype = xltypeNum;
        xDouble.val.num = value;
    }

    DLL_API void scalarToOper(const bool &value, OPER &xBoolean, bool dllToFree, bool expandVector) {
        xBoolean.xltype = xltypeBool;
        xBoolean.val.boolean = value;
    }

    DLL_API void scalarToOper(const char *value, OPER &xChar, bool dllToFree, bool expandVector) {
		scalarToOper(std::string(value), xChar, dllToFree, expandVector);
    }

    DLL_API void scalarToOper(const std::string &value, OPER &xString, bool dllToFree, bool expandVector) {
        // Must use type unsigned char (BYTE) to process the 0th byte of Excel byte-counted string
        unsigned char len = __min(XL_MAX_STR_LEN - 1, value.length());
        xString.val.str = new char[len + 1];
        xString.xltype = xltypeStr;
        if (dllToFree)
            xString.xltype |= xlbitDLLFree;
        xString.val.str[0] = len;
        if (len)
            strncpy(xString.val.str + 1, value.c_str(), len);
    }

    void setError(OPER &oper, WORD val) {
        oper.xltype = xltypeErr;
        oper.val.err = val; 
    }

    class VariantToOper : public boost::static_visitor<> {
    public:
        VariantToOper(OPER &oper, bool expand) : oper_(oper), m_expand(expand) {}
        VariantToOper(const VariantToOper& op) : oper_(op.oper_), m_expand(op.m_expand) {}

        void operator()(const empty_property_tag&) { setError(oper_, xlerrNA); }

        template <typename T>
        void operator() (const T& t) { scalarToOper(t, oper_); }

        template <typename T>
        void operator()(const std::vector<T>& v) {
            if(m_expand)
                vectorToOper(v, oper_);
            else
                scalarToOper("<VECTOR>", oper_);
        }
        
        template<typename T>
        void operator()(const std::vector<std::vector<T> >& v) {
            if(m_expand)
                matrixToOper(v, oper_);
            else
                scalarToOper("<MATRIX>", oper_);
        }

    private:
        OPER &oper_;
        bool m_expand;
    };

    DLL_API void scalarToOper(const ObjectHandler::property_t &value, OPER &xVariant, bool dllToFree, bool expandVector) {
        VariantToOper variantToOper(xVariant, expandVector);
        boost::apply_visitor(variantToOper, value);
    }

    /*template <class T>
    void wrapScalarToOper(const boost::any &value, OPER &xAny) {
        scalarToOper(boost::any_cast<T>(value), xAny);
    }

    template <class T1, class T2>
    void wrapScalarToOper(const boost::any &value, OPER &xAny) {
        scalarToOper(static_cast<T2>(boost::any_cast<T1>(value)), xAny);
    }

    template <class T>
    void wrapVectorToOper(const boost::any &value, OPER &xAny) {
        vectorToOper(boost::any_cast<std::vector<T> >(value), xAny);
    }

    template <class T>
    void wrapMatrixToOper(const boost::any &value, OPER &xAny) {
        matrixToOper(boost::any_cast<std::vector<std::vector<T> > >(value), xAny);
    }

    void setVectorString(OPER &xAny) {
        scalarToOper(std::string("<VECTOR>"), xAny);
    }

    void setMatrixString(OPER &xAny) {
        scalarToOper("<MATRIX>", xAny);
    }

    DLL_API void scalarToOper(const boost::any &value, OPER &xAny, bool dllToFree, bool expandVector) {
        if (value.type() == typeid(OPER)) {
            OPER xTemp = boost::any_cast<OPER>(value);
            Excel(xlCoerce, &xAny, 1, &xTemp);
        } else if (value.type() == typeid(OPER*)) {
            OPER *xTemp = boost::any_cast<OPER*>(value);
            Excel(xlCoerce, &xAny, 1, xTemp);
        } else if (value.type() == typeid(unsigned int)) {
            wrapScalarToOper<unsigned int, long>(value, xAny);
        } else if (value.type() == typeid(int)) {
            wrapScalarToOper<int, long>(value, xAny);
        } else if (value.type() == typeid(long)) {
            wrapScalarToOper<long>(value, xAny);
        } else if (value.type() == typeid(double)) {
            wrapScalarToOper<double>(value, xAny);
        } else if (value.type() == typeid(bool)) {
            wrapScalarToOper<bool>(value, xAny);
        } else if (value.type() == typeid(std::string)) {
            wrapScalarToOper<std::string>(value, xAny);
        } else if (value.type() == typeid(Variant)) {
            wrapScalarToOper<Variant>(value, xAny);
        } else if (value.type() == typeid(std::vector<long>)) {
            if (expandVector)
                wrapVectorToOper<long>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<double>)) {
            if (expandVector)
                wrapVectorToOper<double>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<bool>)) {
            if (expandVector)
                wrapVectorToOper<bool>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<std::string>)) {
            if (expandVector)
                wrapVectorToOper<std::string>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<boost::any>)) {
            if (expandVector)
                wrapVectorToOper<boost::any>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<Variant>)) {
            if (expandVector)
                wrapVectorToOper<Variant>(value, xAny);
            else
                setVectorString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<long> >)) {
            if (expandVector)
                wrapMatrixToOper<long>(value, xAny);            
            else
                setMatrixString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<double> >)) {
            if (expandVector)
                wrapMatrixToOper<double>(value, xAny);            
            else
                setMatrixString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<bool> >)) {
            if (expandVector)
                wrapMatrixToOper<bool>(value, xAny);            
            else
                setMatrixString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<std::string> >)) {
            if (expandVector)
                wrapMatrixToOper<std::string>(value, xAny);            
            else
                setMatrixString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<boost::any> >)) {
            if (expandVector)
                wrapMatrixToOper<boost::any>(value, xAny);            
            else
                setMatrixString(xAny);
        } else if (value.type() == typeid(std::vector<std::vector<Variant> >)) {
            if (expandVector)
                wrapMatrixToOper<Variant>(value, xAny);            
            else
                setMatrixString(xAny);
        } else {
            setError(xAny, xlerrValue);
        }
    }*/

}

