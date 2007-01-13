
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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

/*! \file
    \brief ObjectHandlerclass
*/

#ifndef oh_objecthandler_hpp
#define oh_objecthandler_hpp

#include <oh/object.hpp>
#include <oh/objhandlerdefines.hpp>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {

    //! Global Object repository.
    /*! Maintains a repository of Objects.  Objects may be 
        created/amended/destroyed by the client application.
    */
    class DLL_API ObjectHandler {
    public:
        ObjectHandler();
        virtual ~ObjectHandler();
        static ObjectHandler &instance();

        //! \name storing / retrieving / deleting Objects
        //@{
        //! Store Object with given ID.
        /*! Any existing Object with that ID is deleted.
        */
        virtual std::string storeObject(const std::string &objectID, 
                                        const boost::shared_ptr<Object> &object);

        //! Retrieve Object with given ID.
        /*! Throws exception if no Object exists with that ID.
        */
        virtual boost::shared_ptr<Object> retrieveObjectImpl(const std::string &objectID) const;

        template <class T>
        void retrieveObject(boost::shared_ptr<T> &ret,
                            const std::string &id) {
            boost::shared_ptr<Object> object = retrieveObjectImpl(id);
            ret = boost::dynamic_pointer_cast<T> (object);
            if (!ret)
                throw Exception("Error retrieving object with id '"
                    + id + "' - unable to convert reference to type '" 
                    + typeid(T).name() + "'");
        }

        void retrieveObject(boost::shared_ptr<Object> &ret,
                            const std::string &id) {
            ret = retrieveObjectImpl(id);
        }

        //! Delete Object with given ID.
        /*! Does nothing if no Object exists with that ID.
        */
        virtual void deleteObject(const std::string &objectID);

        //! Delete all Objects in repository.
        /*! Does nothing if repository is already empty.
        */
        virtual void deleteAllObjects(const bool &deletePermanent = false);

        //@}
        //! \name utilities
        //@{
        //! Log dump of ObjectHandler.
        /*! Write all objects in ObjectHandler 
            to output stream.
        */
        virtual void dump(std::ostream&);

        //! Count of all Objects in repository.
        virtual const int objectCount();

        //! List IDs of all Objects in repository.
        /*! Returns empty list if repository is empty.
        */
        virtual const std::vector<std::string> listObjectIDs(
            const std::string &regex = "");
        //@}
    protected:
        static ObjectHandler *instance_;
        void checkName(const std::string &objectID);
    };

}

    // TODO move into the ObjHandler namespace
    // convert a vector of strings to a vector of objects
    template <class ObjectClass>
    std::vector<boost::shared_ptr<ObjectClass> > getObjectVector(
	    const std::vector<std::string> &objectIDs) {
	    std::vector<boost::shared_ptr<ObjectClass> > ret;
        ret.reserve(objectIDs.size());
	    for (std::vector<std::string>::const_iterator i = objectIDs.begin();
		     i != objectIDs.end();
             ++i) {
		    OH_GET_OBJECT(objectPointer, *i, ObjectClass);
		    ret.push_back(objectPointer);
	    }
	    return ret;
    }

#endif
