
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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
    \brief ObjectHandlerXL class
*/

#ifndef ohxl_objecthandlerxl_hpp
#define ohxl_objecthandlerxl_hpp

#include <oh/objecthandler.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/objhandlerxldefines.hpp>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {

#ifdef OHXL_ENABLE_GARBAGE_COLLECTION
    class CallingRange;
#endif // OHXL_ENABLE_GARBAGE_COLLECTION

    //! Global Object repository.
    /*! Maintains a repository of Objects.
        Objects may be created/amended/destroyed
        by the client application.
    */
    class DLL_API ObjectHandlerXL : public ObjectHandler {
    public:
    static ObjectHandlerXL &instance();

        //! Store Object with given ID.
        /*! This function is optimized for the base case where the calling range
            contains a single formula.

            Additional behavior is supported:
            - multiple functions nested within a single calling range - with the
              possibility that functions to construct Objects are intermixed with 
              functions from another source e.g. Excel or some other user addin
            - "anonymous objects" i.e. the user has not supplied an ID
              and a default value is generated

            Performance in such special cases is sub-optimal.
        */
        virtual std::string storeObject(const std::string &objectID, 
                                        const boost::shared_ptr<Object> &object);
        virtual boost::shared_ptr<Object> retrieveObjectImpl(
                                        const std::string &objectID) const {
#ifdef OHXL_ENABLE_GARBAGE_COLLECTION
            return ObjectHandler::retrieveObjectImpl(getStub(objectID));
#else
            return ObjectHandler::retrieveObjectImpl(objectID);
#endif // OHXL_ENABLE_GARBAGE_COLLECTION
        }

        virtual void deleteObject(const std::string &objectID);
        virtual void deleteAllObjects(const bool &deletePermanent = false);

        void logError(const std::string &message, const bool &append = false);
        std::string retrieveError(const XLOPER *range);
        virtual void collectGarbage(const bool &deletePermanent = false);

#ifdef OHXL_ENABLE_GARBAGE_COLLECTION

        //! Reset the calling cell.
        /*! This function resets the status of the calling cell to "not busy" and
            should be called by non-constructor functions in case they wrap constructors
            which set the caller's status to "busy".
        */
        virtual void resetCaller(const bool &createIfNone = false);

        void clearError();
        virtual void dump(std::ostream&);

#endif // OHXL_ENABLE_GARBAGE_COLLECTION

    private:

        virtual std::string generateObjectID();
        static unsigned long objectIDCount_;

#ifdef OHXL_ENABLE_GARBAGE_COLLECTION

        std::string getStub(const std::string &objectID) const;
        boost::shared_ptr<CallingRange> getCallingRange(bool createIfNone = false);
        //void clearCallingRange(boost::shared_ptr<CallingRange> callingRange);

#endif // OHXL_ENABLE_GARBAGE_COLLECTION

    };

}

#endif

