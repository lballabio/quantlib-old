/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004, 2005, 2006, 2007  Eric Ehlers
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
    \brief Class ObjectWrapper - Ensure that Object references are up to date.
*/

#ifndef oh_objectwrapper_hpp
#define oh_objectwrapper_hpp

#include <ostream>
#include <oh/object.hpp>
#include <oh/observable.hpp>
#include <oh/serializationfactory.hpp>
#include <oh/utilities.hpp>

namespace ObjectHandler {

    //! Container to ensure that Object references are updated.
    /*! ObjectWrapper holds a reference to an Object.  Before returning the reference
        the ObjectHandler client application, the ObjectWrapper ensures that the state
        of the Object is updated as necessary to reflect any changes in its precedents.
        There is a 1:1 relationship between Object and ObjectWrapper instances.

        ObjectWrapper inherits from Observer and Observable.  ObjectWrapper registers as
        an Observer of its Object's precedents.  If any of those precedents changes,
        the ObjectWrapper is notified and sets its Dirty property to true.  If the
        ObjectHandler client application attempts to retrieve a Dirty Object, the
        ObjectWrapper first recreates the Object, ensuring that its state reflects
        any changes in the precedents.
    */
    class ObjectWrapper : public Observer, public Observable {        
    public:
        //! \name Structors
        //@{
        //! Construct the ObjectWrapper from the given Object.
        ObjectWrapper(const boost::shared_ptr<Object>& object);
        //! Virtual destructor - unregister with Observers.
        virtual ~ObjectWrapper() {
            unregisterAllWith();
        }
        //@}

        //! \name Behavior
        //@{
        //! Recreate the Object contained by the ObjectWrapper.
        /*! This function is called on any attempt to retrieve a Dirty Object.
            To recreate the Object, we take its ValueObject, which is a snapshot
            of the arguments to the Object's constructor, and pass this ValueObject
            to the SerializationFactory which recreates the Object.
        */
        void reCreate();
        //! Update the ObjectWrapper following a change in its precedents.
        /*! This function is called by the Observable with which this Observer
            has registered.  Sets Dirty -> true.
        */
        virtual void update();
        //! Return a copy of the reference to the Object contained by ObjectWrapper.
        boost::shared_ptr<Object> object() const { return object_; }
        //! Replace the contained Object with the one provided.
        void reset(boost::shared_ptr<Object> object);
        //@}

        //! \name Inspectors
        //@{
        //! The object's initial creation time.
        double creationTime() const { return creationTime_; }
        //! The time of the object's last update.
        double updateTime() const { return updateTime_; }
        //! Query the value of the dirty flag.
        /*! False means the Object is up to date, true means it is invalid.
        */
        bool dirty() const { return dirty_; }
        //@}

        //! \name Logging
        //@{
        //! Write this object to the given output stream.
        virtual void dump(std::ostream& out) { object_->dump(out); }
        //@}

    protected:
        //! Reference to the Object contained by ObjectWrapper.
        boost::shared_ptr<Object> object_;

    private:
        // Flag indicating whether contained Object is up to date.
        bool dirty_;
        // Time at which Object was first created.
        double creationTime_;
        // Time at which Object was last recreated.
        double updateTime_;
    };

    inline ObjectWrapper::ObjectWrapper(const boost::shared_ptr<Object>& object)
        : object_(object), dirty_(false) {
            creationTime_ = updateTime_ = getTime();
    }

    inline void ObjectWrapper::reCreate(){
        try {
            object_ = SerializationFactory::instance().NewObject( 
                object_->properties());
            dirty_ = false;
            updateTime_ = getTime();
        } catch (const std::exception &e) {
            OH_FAIL("Error in function ObjectWrapper::reCreate() : " << e.what());
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

    //! Log the given ObjectWrapper to the given stream.
    inline std::ostream& operator<<(std::ostream& out, const boost::shared_ptr<ObjectWrapper> &ow) {
        ow->dump(out);
        return out;
    }
}

#endif

