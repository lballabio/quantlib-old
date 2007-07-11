
/*
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

/*! \file
    \brief Class Repository - Maintain a store of Objects
*/

#ifndef oh_repository_hpp
#define oh_repository_hpp

#include <oh/object.hpp>
#include <oh/ohdefines.hpp>
#include <oh/iless.hpp>
#include <map>

//! ObjectHandler
/*! Namespace for ObjectHandler functionality.
*/
namespace ObjectHandler {

    //! Maintain a store of Objects.
    /*! The client application may store, retrieve, and delete Objects in
        the Repository.

        This class is implemented using a Singleton design pattern.  Rather
        than the Meyers Singleton used elsewhere, Repository uses a specialized
        Singleton supporting inheritance so that the Repository can be customized
        for specific platforms.

        This class is designed so that it can be exported across DLL
        boundaries on the Windows platform.
    */
    class DLL_API Repository {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - initialize the singleton.
        Repository();
        //! Destructor - de-initialize the singleton.
        virtual ~Repository();
        //! Client applications access the global object via a call to Repository::instance().
        static Repository &instance();
        //@}

        //! \name Object Management
        //@{
        //! Store an Object with the given ID.
        /*! Any existing Object with that ID is deleted.

            Storage of objects uses "case-preserving" behavior:
            \code
                storeObject("MyObject")         // store "MyObject"
                retrieveObject("MyObJeCt")      // retrieve "MyObject"
                storeObject("MYOBJECT")         // overwrite "MyObject"
            \endcode
        */
        virtual std::string storeObject(const std::string &objectID,
                                        const boost::shared_ptr<Object> &object);

        //! Template member function to retrieve the Object with given ID.
        /*! Retrieve the object with the given ID and recast it to the desired type.
            Throw an exception if no Object exists with that ID.
            This template passes the work off to function retrieveObjectImpl which
            may be overridden in derived classes.
        */
        template <class T>
        void retrieveObject(boost::shared_ptr<T> &ret,
                            const std::string &id) {
            boost::shared_ptr<Object> object = retrieveObjectImpl(id);
            ret = boost::dynamic_pointer_cast<T>(object);
            OH_REQUIRE(ret, "Error retrieving object with id '"
                << id << "' - unable to convert reference to type '"
                << typeid(T).name() << "'");
        }

        //! Override of template function retrieveObject.
        /*! Specialized for the case where the client has requested a reference to
            class Object and no recast is necessary.
        */
        void retrieveObject(boost::shared_ptr<Object> &ret,
                            const std::string &id);

        //! Default implementation of retrieveObjectImpl.
        /*! Retrieves the Object with the given ID.
            Throws an exception if no Object exists with that ID.
            This member function may be overridden in base classes which customize
            the Repository class for platform-specific functionality.
        */
        virtual boost::shared_ptr<Object> retrieveObjectImpl(const std::string &objectID) const;

        //! Delete the Object with the given ID.
        /*! Take no action if no Object exists with that ID.
        */
        virtual void deleteObject(const std::string &objectID);

        //! Delete all of the Objects in the Repository.
        /*! Take no action if the Repository is already empty.
        */
        virtual void deleteAllObjects(const bool &deletePermanent = false);
        //@}

		virtual void saveObject(const std::vector<boost::shared_ptr<ObjectHandler::Object> > &objectList, const std::string &path);
		virtual void saveObject(const boost::shared_ptr<ObjectHandler::Object> &object, const std::string &path);
		virtual std::vector<boost::shared_ptr<ObjectHandler::Object> > loadObject(const std::vector<std::string> &idList, const std::string &path);
		virtual boost::shared_ptr<ObjectHandler::Object> loadObject(const std::string &objectID, const std::string &path);

        //! \name Logging
        //@{
        //! Log the indicated Object to the given stream.
        /*! If no Object exists with that ID then an appropriate message is written
            to the stream.
        */
        virtual void dumpObject(const std::string &objectID, std::ostream&);
        //! Log dump of the Repository.
        /*! Write all of the Objects in the Repository to the given output stream.
        */
        virtual void dump(std::ostream&);
        //@}

        //! \name Utilities
        //@{
        //! Count of all the Objects in the Repository.
        virtual int objectCount();

        //! List the IDs of all the Objects in the Repository.
        /*! Returns an empty list if the Repository is empty.
        */
        virtual const std::vector<std::string> listObjectIDs(
            const std::string &regex = "");
        //@}

        //! Define the type of the structure used to store the Objects.
        /*! The Repository class cannot declare a private data member of type
            ObjectMap, because std::map cannot be exported across DLL boundaries on
            the Windows platform.  Instead the map is declared as a static variable
            in the cpp file.
        */
        typedef std::map<std::string, boost::shared_ptr<Object>, my_iless> ObjectMap;

    protected:
        //! A pointer to the Repository instance, used to support the Singleton pattern.
        static Repository *instance_;
    };

    //! Convert a vector of strings to a vector of objects.
    template <class ObjectClass>
    std::vector<boost::shared_ptr<ObjectClass> > getObjectVector(
	        const std::vector<std::string> &objectIDs) {

	    std::vector<boost::shared_ptr<ObjectClass> > ret;
        ret.reserve(objectIDs.size());

	    for (std::vector<std::string>::const_iterator i = objectIDs.begin();
		        i != objectIDs.end(); ++i) {
		    OH_GET_OBJECT(objectPointer, *i, ObjectClass);
		    ret.push_back(objectPointer);
	    }
	    return ret;
    }

    //! Convert a vector of strings to a vector of library objects.
    template <class ObjectClass, class LibraryClass>
    std::vector<boost::shared_ptr<LibraryClass> > getLibraryObjectVector(
	        const std::vector<std::string> &objectIDs) {

	    std::vector<boost::shared_ptr<LibraryClass> > ret;
        ret.reserve(objectIDs.size());

	    for (std::vector<std::string>::const_iterator i = objectIDs.begin();
		        i != objectIDs.end(); ++i) {
		    OH_GET_REFERENCE(objectPointer, *i, ObjectClass, LibraryClass);
		    ret.push_back(objectPointer);
	    }
	    return ret;
    }

}

#endif

