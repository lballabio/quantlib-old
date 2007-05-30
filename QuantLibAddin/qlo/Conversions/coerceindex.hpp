
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

#ifndef qlo_conversions_coerceindex_hpp
#define qlo_conversions_coerceindex_hpp

#include <oh/Conversions/coerce.hpp>
#include <oh/exception.hpp>
#include <oh/Enumerations/typefactory.hpp>
#include <qlo/Enumerations/Factories/indexfactory.hpp>

namespace QuantLibAddin {

    // CoerceIndex - accept a string which is the ID of either
    // an Enumerated Class or an object in the repository, 
    // and return the appropriate index object

    template <class EnumerationClass, class LibraryClass>
    bool indexFromRegistry(
            const std::string &in,
            boost::shared_ptr<LibraryClass> &out) {

        if (!ObjectHandler::Create<boost::shared_ptr<QuantLib::Index> >().checkType(in))
            return false;
        boost::shared_ptr<QuantLib::Index> indexPointer = 
            ObjectHandler::Create<boost::shared_ptr<QuantLib::Index> >()(in);

        out = boost::dynamic_pointer_cast<LibraryClass>(indexPointer);
        OH_REQUIRE(out, "Unable to convert enumerated class with ID " << in <<
            " to class " << typeid(LibraryClass).name());
        return true;
    }

    template <class ObjectClass, class LibraryClass>
    bool indexFromRepository(
            const std::string &in,
            boost::shared_ptr<LibraryClass> &out) {

        OH_GET_REFERENCE(ret, in, ObjectClass, LibraryClass)
        out = ret;
        return true;
    }

    template <class ObjectClass, class LibraryClass>
    class CoerceIndex : public ObjectHandler::Coerce<
            std::string,
            boost::shared_ptr<LibraryClass> > {

        typedef typename ObjectHandler::Coerce<
            std::string, 
            boost::shared_ptr<LibraryClass> >::Conversion Conversion;

        Conversion *getConversions() {
            static Conversion conversions[] = {
                indexFromRegistry<LibraryClass>, 
                indexFromRepository<ObjectClass, LibraryClass>, 
                0
            };
            return conversions; 
        };

    };

    template <class ObjectClass, class LibraryClass>
    inline std::vector<boost::shared_ptr<LibraryClass> > CoerceIndexVector(
            const std::vector<std::string> &ids) {

        std::vector<boost::shared_ptr<LibraryClass> > ret;
        ret.reserve(ids.size());

        for (std::vector<std::string>::const_iterator i = ids.begin(); 
                i != ids.end(); ++i)
            ret.push_back(CoerceIndex<ObjectClass, LibraryClass>()(*i));
        return ret;

    }

}

#endif

