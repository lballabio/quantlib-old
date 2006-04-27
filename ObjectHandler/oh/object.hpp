
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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
    \brief Object class
*/

#ifndef oh_object_hpp
#define oh_object_hpp

#include <boost/shared_ptr.hpp>
#include <boost/any.hpp>
#include <oh/valueobject.hpp>
#include <vector>
#include <stack>
#include <string>
#include <iostream>

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors 
   (Metrowerks, for example) also #define _MSC_VER
*/
#if defined BOOST_MSVC       // Microsoft Visual C++
#pragma warning(disable:4231)
#endif

namespace ObjHandler {
    //! Interface for Objects to be stored in the ObjectHandler.
    /*! Objects are constructed by the client and passed to
        ObjHandler::storeObject() for storage in the repository.
    */
    class Object {        
    public:
        //! Processing for the identifier for this Object.
        /*! This base class is empty and may be overridden
            to provide platform-specific behavior.
        */
        class InstanceName {
        public:
            InstanceName(const std::string &fullName) : fullName_(fullName) {}
            virtual ~InstanceName() {}
            const std::string &getStubName() {
                return stubName_;
            }
            const std::string &getFullName() {
                return fullName_;
            }
            const std::string &getKey() {
                return key_;
            }
            virtual bool isValid() {
                return true;
            }
        protected:
            InstanceName() {}
            std::string stubName_;
            std::string fullName_;
            std::string key_;
        };

        //! \name Constructors & Destructors
        //@{
        //! Default constructor.
        /*! Construct an Object.
            To store the resulting Object in the ObjectHandler, call
                ObjectHandler::instance().storeObject(handle, object);
        */
        Object() {};
        //! Default destructor.
        virtual ~Object() {};
        //@}
        //! \name Object interrogation
        //@{
        //! Acquire a reference to underlying Object.
        /*! Returns a reference to the client object stored in the Object.
            This is a shared pointer to void which must be recast appropriately.
        */
        virtual boost::shared_ptr<void> getReference() const = 0;
        //! Retrieve vector of property names.
        /*! Returns empty vector if Object has no properties.
        */
        std::vector < std::string > propertyNames() const;
        //! Retrieve value of given property.
        /*! Throws exception if Object has no property by that name.
        */
        boost::any propertyValue(const std::string &propertyName) const;
        friend std::ostream &operator<<(std::ostream&, const Object &object);
        //@}

        void setProperties(const boost::shared_ptr<ValueObject>& p) {
            mProps = p;
        }

        void setInstanceName(const boost::shared_ptr < InstanceName > &instanceName) {
            instanceName_ = instanceName;
        }
        const std::string &getStubName() {
            return instanceName_->getStubName();
        }
        const std::string &getFullName() {
            return instanceName_->getFullName();
        }
        const std::string &getKey() {
            return instanceName_->getKey();
        }
        bool isValid() {
            return instanceName_->isValid();
        }
    private:
        boost::shared_ptr<ValueObject> mProps;
        Object& operator= (const Object&);
        Object(const Object&);
        boost::shared_ptr< InstanceName > instanceName_;
    };

    template < typename T >
    std::ostream& operator<<(std::ostream& out, std::vector < T > &v) {
        out << std::endl;
        for (typename std::vector< T >::const_iterator i = v.begin(); i != v.end(); i++)
            out << *i << std::endl;
        return out;
    }

}

#endif

