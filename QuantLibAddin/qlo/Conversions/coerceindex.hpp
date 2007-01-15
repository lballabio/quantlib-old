
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

#ifndef qlo_conversions_coerceindex_hpp
#define qlo_conversions_coerceindex_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <qlo/typefactory.hpp>

namespace ObjHandler {

    // CoerceIndex - accept a string which is the ID of either
    // an Enumerated Class or an object in the repository, 
    // and return the appropriate index object

    template <class enumClass, class qlClass>
    bool indexFromRegistry(
            const std::string &in,
            boost::shared_ptr<qlClass> &out) {
        if (!QuantLibAddin::Create<boost::shared_ptr<QuantLib::Index> >().checkType(in))
            return false;
        boost::shared_ptr<QuantLib::Index> indexPointer = 
            QuantLibAddin::Create<boost::shared_ptr<QuantLib::Index> >()(in);
        out = boost::dynamic_pointer_cast<qlClass>(indexPointer);
        OH_REQUIRE(out, "Unable to convert enumerated class with ID " << in <<
            " to class " << typeid(qlClass).name());
        return true;
    }

    template <class qloClass, class qlClass>
    bool indexFromRepository(
            const std::string &in,
            boost::shared_ptr<qlClass> &out) {
        OH_GET_REFERENCE(ret, in, qloClass, qlClass)
        out = ret;
        return true;
    }

    template <class qloClass, class qlClass>
    class CoerceIndex : public ObjHandler::Coerce<
            std::string,
            boost::shared_ptr<qlClass> > {
        typedef typename ObjHandler::Coerce<
            std::string, 
            boost::shared_ptr<qlClass> >::Conversion Conversion; 
        Conversion *getConversions() {
            static Conversion conversions[] = {
                indexFromRegistry<qlClass>, 
                indexFromRepository<qloClass, qlClass>, 
                0
            };
            return conversions; 
        };
    };

    template <class qloClass, class qlClass>
    inline std::vector<boost::shared_ptr<qlClass> > CoerceIndexVector(
        const std::vector<std::string> &ids) {
        std::vector<boost::shared_ptr<qlClass> > ret;
        ret.reserve(ids.size());
        std::vector<std::string>::const_iterator i;
        for (i = ids.begin(); i != ids.end(); ++i)
            ret.push_back(CoerceIndex<qloClass, qlClass>()(*i));
        return ret;
    }

}

#endif

