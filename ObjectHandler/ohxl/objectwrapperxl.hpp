/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2008 Eric Ehlers

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
    \brief Class ObjectWrapperXL - Customization of the ObjectWrapper class for the Excel platform
*/

#ifndef oh_objectwrapperxl_hpp
#define oh_objectwrapperxl_hpp

#include <oh/objectwrapper.hpp>

namespace ObjectHandler {

    class CallingRange;

    //! Customization of the ObjectWrapper class for the Excel platform.
    /*! Extend ObjectWrapper with additional Excel-specific functionality e.g:
        - Retain a reference to the cell which constructed the Object
        - Suffix the Object's ID with an update count e.g. my_object#00123
    */
    class ObjectWrapperXL : public ObjectWrapper {
    public:
        //! \name Structors
        //@{
        //! Constructor - accepts ID and reference to contained Object.
        /*! Unlike ObjectWrapper, ObjectWrapperXL knows its own ID as additional
            functionality is implemented around this value.

            The callingRange parameter is a reference to the calling cell.  A null
            value indicates that this constructor was invoked from Excel VBA code.
        */
        ObjectWrapperXL(
            const std::string &id,
            const boost::shared_ptr<Object> &object,
            const boost::shared_ptr<CallingRange> &callingRange);
        //! Destructor - de-register this object with its CallingRange object.
        virtual ~ObjectWrapperXL();
        //@}

        //! \name Inspectors
        //@{
        //! Retrieve the object's ID.
        const std::string &id() const { return id_; }
        //! Retrieve the object's ID as reformatted for the Excel platform.
        const std::string &idFull() const { return idFull_; }
        //! Get the key identifying the CallingRange object.
        /*! Returns the string "VBA" if this object was constructed by VBA code. */
        std::string callerKey() const;
        //! Get the address of the calling cell.
        /*! Returns the string "VBA" if this object was constructed by VBA code. */
        std::string callerAddress() const;
        //! Query the value of the "permanent" flag.
        /*! The request is forwarded to the Object contained by this ObjectWrapperXL. */
        const virtual bool &permanent() const { return object_->permanent(); }
        //! Calling Range
        boost::shared_ptr<CallingRange>& getCallingRange(){ return callingRange_;}
        //@}

        //! \name Logging
        //@{
        //! Write this object to the given output stream.
        /*! Called by the logging framework.  Enhanced from the base class function
            to provide additional information specific to the Excel platform.
        */
        virtual void dump(std::ostream& out);
        //@}

        //! \name Behavior
        //@{
        //! Replace the contained Object with the one provided.
        void reset(boost::shared_ptr<Object> object);
        //! Associate this object with a different calling range.
        void resetCaller(boost::shared_ptr<CallingRange> callingRange);
        //@}
    private:
        // Reference to the worksheet cell in which this object resides.
        boost::shared_ptr<CallingRange> callingRange_;
        // This object's ID, in the same format used for the base Object class.
        std::string id_;
        // This object's ID, reformatted for Excel with the update count e.g. my_object#00123.
        // For objects created from VBA, the full ID is the same as the normal one.
        std::string idFull_;
    };

}

#endif

