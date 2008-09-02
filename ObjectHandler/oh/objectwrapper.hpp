/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
Copyright (C) 2006 Plamen Neykov

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
\brief Class Object - Define interface for Objects to be stored in the Repository
*/

#ifndef oh_objectwrapper_hpp
#define oh_objectwrapper_hpp

#include <ostream>
#include <oh/object.hpp>
#include <oh/observable.hpp>
#include <oh/serializationfactory.hpp>
#include <oh/utilities.hpp>

namespace ObjectHandler {

    //! ObjectWrapper holds a reference to Object.  ObjectWrapper inherits from Observer and Observable.
    /*! 
    ObjectWrapper holds a reference to Object which is be stored in the ObjectHandle. the relation between 
    Object and ObjectWrapper is 1:1;
    ObjectWrapper inherits from Observer and Observable.  The Dirty property belongs to ObjectWrapper.  
    ObjectWrapper recreates Object when necessary to what the Dirty property is true
    */
    class ObjectWrapper : public Observer, public Observable {        
    public:
        //! \name Structors
        //@{

        //! Default constructor.
        /*! Construct an Object.
        To using the Observer / Observable design pattern to ensure that all object dependencies are enforced, 
        this class wrapped the result object which will be stored in the ObjectHandler
        */
        ObjectWrapper(const boost::shared_ptr<Object>& object);

        //! Empty virtual destructor.
        virtual ~ObjectWrapper() {
            unregisterAllWith();
        }
        //@}


        //! \name property: dirty
        //@{
        //! Query the value of the "dirty" flag.
        /*! the Object property Dirty=false, the object is up to date; or the object is invalid.
        */
        bool dirty() const {
            return dirty_;
        }
        //@}

        //! \name recreate a object
        //@{
        //! the object will be recreated when Dirty=true.
        /*! When this function is invoked, the stored Object is recreated with a call to SerializationFactory::recreateObject() 
        */
        void reCreate();
        //@}

        //! \name update interface
        //@{
        //! modified the Object property Dirty from false to true!
        /*! The observable object notifies its all Observers to change their property 'dirty'
        */
        virtual void update();
        //@}

        //! \name get a object
        //@{
        //! Return a copy of the reference to the Object contained by ObjectWrapperXL.
        boost::shared_ptr<Object> object() const { return object_; }
        //@}

        //! \name reset a object
        //@{
        //! save this object and replace the member object.
        void reset(boost::shared_ptr<Object> object);
        //@}

        //! \name Logging
        //@{
        //! Write this object to the given output stream.
        /*! Called by the logging framework.  Enhanced from the base class function
        to provide additional information specific to the Excel platform.
        */
        virtual void dump(std::ostream& out){ object_->dump(out);}
        //@}

        //! \name getting time
        //@{
        //! get the initially time of creating object
        double creationTime() const { return creationTime_; }
        //! get the last time of creating object
        double updateTime() const { return updateTime_; }
        //@}

    protected:
        // Reference to the Object contained by ObjectWrapperXL.
        boost::shared_ptr<Object> object_;

    private:
        //When dirty=false, the object is up to date.  When Dirty=true, the object is invalid
        bool dirty_;
        typedef std::list<ObjectWrapper*>::iterator iteratorOW;
        //The time at which the Object was initially created
        double creationTime_;
        //The time at which the Object was last recreated
        double updateTime_;
    };

    inline ObjectWrapper::ObjectWrapper(const boost::shared_ptr<Object>& object)
        : object_(object), dirty_(false) {
            creationTime_ = updateTime_ = getTime();
    }


    inline void ObjectWrapper::reCreate(){
        try {
            SerializationFactory::instance().createObject( 
                object_->properties(), true);
            dirty_ = false;
            //updateTime_ = getTime();

        } catch (const std::exception &e) {
            OH_FAIL("Error in function ObjectWrapper::reCreate : " << e.what());
        }    
    }

    inline void ObjectWrapper::update(){
        notifyObservers();
        dirty_ = true;
    }

    inline void ObjectWrapper::reset(boost::shared_ptr<Object> object) {

        object_ = object;
        updateTime_ = getTime();
        notifyObservers();
    }

    inline std::ostream& operator<<(std::ostream& out,const boost::shared_ptr<ObjectWrapper> &ow) {
        ow->dump(out);
        return out;
    }
}
#endif

