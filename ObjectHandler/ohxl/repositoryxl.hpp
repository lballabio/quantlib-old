
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
    \brief RepositoryXL class
*/

#ifndef ohxl_repositoryxl_hpp
#define ohxl_repositoryxl_hpp

#include <oh/repository.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/ohxldefines.hpp>
#include <ohxl/objectxl.hpp>

//! ObjectHandler
/*! name space for the Object Handler
*/
namespace ObjectHandler {

    //! Global Object repository.
    /*! Maintains a repository of Objects.
        Objects may be created/amended/destroyed
        by the client application.
    */
    class DLL_API RepositoryXL : public Repository {
    public:
    static RepositoryXL &instance();

        virtual std::string storeObject(const std::string &objectID, 
                                        const boost::shared_ptr<Object> &object);
        virtual boost::shared_ptr<Object> retrieveObjectImpl(
            const std::string &objectID) const;

        virtual void deleteObject(const std::string &objectID);

        void logError(const std::string &message, const bool &append = false);
        std::string retrieveError(const XLOPER *range);
        void clearError();
        virtual void dump(std::ostream&);
        virtual void dumpObject(const std::string &objectID, std::ostream&);
        void collectGarbage(const bool &deletePermanent = false);
    private:
        boost::shared_ptr<CallingRange> getCallingRange();

    };

}

#endif

