/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2008 Nazcatech sprl Belgium

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
#include <list>

#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>

namespace ObjectHandler {

    typedef std::pair<std::string, boost::shared_ptr<ObjectHandler::Object> > StrObjectPair;
    typedef std::set<std::string> Category;
    typedef std::list<StrObjectPair> HandlesList;

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
            const std::string &path,
            bool forceOverwrite);

		virtual int saveObject(
			const std::vector<std::string>& handlesList,
            const std::string &path,
            bool forceOverwrite,
			bool includeGroups = true);

        //! Deserialize an Object list from the path indicated.
        virtual std::vector<std::string> loadObject(
            const std::string &directory,
            const std::string &pattern,
            bool recurse,
            bool overwriteExisting);

        //! Write the object(s) to the given string.
        virtual std::string saveObjectString(
            const std::vector<boost::shared_ptr<Object> >&,
            bool forceOverwrite);
        //! Load object(s) from the given string.
        virtual std::vector<std::string> loadObjectString(
            const std::string &xml,
            bool overwriteExisting);
        //@}

        //! \name Object Creation
        //@{
        //! Recreate an Object from its ValueObject
        boost::shared_ptr<Object> recreateObject( 
            boost::shared_ptr<ObjectHandler::ValueObject> valueObject) const ;
        //! Recreate an Object from its ValueObject and store it in the Repository
        /*! This function calls recreateObject to recreate the Object from its
            ValueObject then stores the newly created Object in the Repository
            with a call to Repository::storeObject().
        */
        StrObjectPair restoreObject(
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting) const;
        //@}

      protected:

        virtual void processPath(
            const std::string &path,
            bool overwriteExisting,
            std::vector<std::string> &processedIDs);
        /*virtual std::string processObject(
            const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject,
            bool overwriteExisting);*/
        virtual void register_out(boost::archive::xml_oarchive &ar,
            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >& valueObjects) = 0;
        virtual void register_in(boost::archive::xml_iarchive &ar,
            std::vector<boost::shared_ptr<ObjectHandler::ValueObject> >& valueObjects) = 0;

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

}

#endif

