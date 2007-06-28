
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <ohxl/Conversions/scalartooper.hpp>
#include <ohxl/Conversions/vectortooper.hpp>
#include <ohxl/Conversions/matrixtooper.hpp>

namespace ObjectHandler {

    DLL_API void scalarToOper(const long &value, OPER &xLong, bool dllToFree) {
        xLong.xltype = xltypeNum;
        xLong.val.num = value;
    }

    DLL_API void scalarToOper(const double &value, OPER &xDouble, bool dllToFree) {
        xDouble.xltype = xltypeNum;
        xDouble.val.num = value;
    }

    DLL_API void scalarToOper(const bool &value, OPER &xBoolean, bool dllToFree) {
        xBoolean.xltype = xltypeBool;
        xBoolean.val.boolean = value;
    }

    DLL_API void scalarToOper(const std::string &value, OPER &xString, bool dllToFree) {
        int len = __min(XL_MAX_STR_LEN, value.length());
        xString.val.str = new char[ len + 1 ];
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
        VariantToOper(OPER &oper) : oper_(oper) {}
        void operator()(const long &val) { scalarToOper(val, oper_); }
        void operator()(const double &val) { scalarToOper(val, oper_); }
        void operator()(const bool &val) { scalarToOper(val, oper_); }
        void operator()(const std::string &val) { scalarToOper(val, oper_); }
        // FIXME cater for error etc.
        void operator()(const ObjectHandler::Other&) { setError(oper_, xlerrNA); }
    private:
        OPER &oper_;
    };

    DLL_API void scalarToOper(const ObjectHandler::Variant &value, OPER &xVariant, bool dllToFree) {
        VariantToOper variantToOper(xVariant);
        boost::apply_visitor(variantToOper, value.variant());
    }

    template <class T>
    void wrapScalarToOper(const boost::any &value, OPER &xAny) {
        scalarToOper(boost::any_cast<T>(value), xAny);
    }

    template <>
    void wrapScalarToOper<unsigned int>(const boost::any &value, OPER &xAny) {
        wrapScalarToOper<long>(value, xAny);
    }

    template <>
    void wrapScalarToOper<int>(const boost::any &value, OPER &xAny) {
        wrapScalarToOper<long>(value, xAny);
    }

    template <class T>
    void wrapVectorToOper(const boost::any &value, OPER &xAny) {
        vectorToOper(boost::any_cast<std::vector<T> >(value), xAny);
    }

    template <class T>
    void wrapMatrixToOper(const boost::any &value, OPER &xAny) {
        matrixToOper(boost::any_cast<std::vector<std::vector<T> > >(value), xAny);
    }

    DLL_API void scalarToOper(const boost::any &value, OPER &xAny, bool dllToFree) {
        if (value.type() == typeid(OPER)) {
            OPER xTemp = boost::any_cast<OPER>(value);
            Excel(xlCoerce, &xAny, 1, &xTemp);
        } else if (value.type() == typeid(OPER*)) {
            OPER *xTemp = boost::any_cast<OPER*>(value);
            Excel(xlCoerce, &xAny, 1, xTemp);
        } else if (value.type() == typeid(unsigned int)) {
            wrapScalarToOper<unsigned int>(value, xAny);
        } else if (value.type() == typeid(int)) {
            wrapScalarToOper<int>(value, xAny);
        } else if (value.type() == typeid(long)) {
            wrapScalarToOper<long>(value, xAny);
        } else if (value.type() == typeid(double)) {
            wrapScalarToOper<double>(value, xAny);
        } else if (value.type() == typeid(bool)) {
            wrapScalarToOper<bool>(value, xAny);
        } else if (value.type() == typeid(std::string)) {
            wrapScalarToOper<std::string>(value, xAny);
        } else if (value.type() == typeid(ObjectHandler::Variant)) {
            wrapScalarToOper<ObjectHandler::Variant>(value, xAny);
        } else if (value.type() == typeid(std::vector<long>)) {
            wrapVectorToOper<long>(value, xAny);
        } else if (value.type() == typeid(std::vector<double>)) {
            wrapVectorToOper<double>(value, xAny);
        } else if (value.type() == typeid(std::vector<bool>)) {
            wrapVectorToOper<bool>(value, xAny);
        } else if (value.type() == typeid(std::vector<std::string>)) {
            wrapVectorToOper<std::string>(value, xAny);
        } else if (value.type() == typeid(std::vector<boost::any>)) {
            wrapVectorToOper<boost::any>(value, xAny);
        } else if (value.type() == typeid(std::vector<std::vector<long> >)) {
            wrapMatrixToOper<long>(value, xAny);            
        } else if (value.type() == typeid(std::vector<std::vector<double> >)) {
            wrapMatrixToOper<double>(value, xAny);            
        } else if (value.type() == typeid(std::vector<std::vector<bool> >)) {
            wrapMatrixToOper<bool>(value, xAny);            
        } else if (value.type() == typeid(std::vector<std::vector<std::string> >)) {
            wrapMatrixToOper<std::string>(value, xAny);            
        } else if (value.type() == typeid(std::vector<std::vector<boost::any> >)) {
            wrapMatrixToOper<boost::any>(value, xAny);            
        } else {
            setError(xAny, xlerrValue);
        }
    }

}

