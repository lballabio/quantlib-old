
/*
 Copyright (C) 2007 Eric Ehlers

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

/*! \file
    \brief Class SerializationFactory - A Singleton wrapping the boost::serialization interface
*/

#ifndef oh_serialization_factory_hpp
#define oh_serialization_factory_hpp

#include <oh/ohdefines.hpp>
#include <oh/object.hpp>
#include <oh/valueobject.hpp>
#include <map>
#include <string>

namespace ObjectHandler {

    //! A Singleton wrapping the boost::serialization interface
    /*! The pure virtual functions in this class must be implemented as appropriate
        for client applications.
    */
    class DLL_API SerializationFactory {

    public:

        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        SerializationFactory();
        //! Destructor - de-initialize the singleton.
        virtual ~SerializationFactory();
        //! Client applications access the global object via a call to SerializationFactory::instance().
        static SerializationFactory &instance();
        //@}

        //! \name Serialization - public interface
        //@{
        //! Serialize the given Object list to the path indicated.
		virtual int saveObject(
            const std::vector<boost::shared_ptr<Object> >&,
            const char *path,
            bool forceOverwrite) const = 0;
        //! Deserialize an Object list from the path indicated.
		virtual int loadObject(
            const char *path,
            bool overwriteExisting) const = 0;
        //@}

    protected:

		 //! A pointer to the SerializationFactory instance, used to support the Singleton pattern.
        static SerializationFactory *instance_;
        //! Define the type for a factory creator function.
        typedef boost::shared_ptr<Object> (*Creator)(const boost::shared_ptr<ValueObject>&);
        //! Register a Creator with the Factory.
        void registerCreator(const std::string &className, const Creator &creator);
		// A map of Creators for each supported class.
        typedef std::map<std::string, Creator> CreatorMap;
		// Cannot export std::map across DLL boundaries, so instead of a data member
        // use a private member function that wraps a reference to a static variable.
        CreatorMap &creatorMap_() const;
    };

    boost::shared_ptr<Object> createRange(const boost::shared_ptr<ValueObject>&);

}

#endif

